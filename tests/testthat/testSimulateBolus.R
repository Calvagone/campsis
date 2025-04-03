library(testthat)

context("Test the simulate method with boluses")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a bolus"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "simple_bolus"

  dataset <- Dataset(1) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a bolus, single-labelled arm"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "simple_bolus"

  dataset <- Dataset(1, label="My Arm") %>%
    add(Bolus(time=0, amount=1000, compartment="DEPOT")) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename),
    expect_equal(unique(results$ARM), "My Arm") # Check that the arm label is present
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a bolus, 2 arms"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "bolus_2arms"

  arm1 <- Arm(1, subjects=10) %>%
    add(Bolus(time=0, amount=1000, compartment="DEPOT")) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  arm2 <- Arm(2, subjects=10) %>%
    add(Bolus(time=0, amount=2000, compartment="DEPOT")) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  dataset <- Dataset() %>%
    add(arm1) %>%
    add(arm2)

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), dataset %>% length() * 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a bolus, 2 labelled arms"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "bolus_2arms"

  arm1 <- Arm(1, subjects=10, label="TRT 1") %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  arm2 <- Arm(2, subjects=10, label="TRT 2") %>%
    add(Bolus(time=0, amount=2000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  dataset <- Dataset() %>%
    add(c(arm1, arm2))

  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), dataset %>% length() * 49),
    outputRegressionTest(results, output=c("CP", "ARM"), filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate multiple doses, with and without the repeat option"), {
  
  # We disable IIV since this is not the topic of this test
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    disable("IIV")
  regFilename <- "multiple_doses_repeat_option"
  
  # 4 days ON / 3 days OFF for 3 weeks
  schedule1 <- CyclicSchedule(duration=weeks(1), repetitions=2)
  schedule2 <- RepeatAtSchedule(times=c(weeks(1), weeks(2)))
  expectedTimes <- c(c(0,24,48,72), c(0,24,48,72) + weeks(1), c(0,24,48,72) + weeks(2))
  
  arm1 <- Arm(1, subjects=1, label="TRT 1") %>%
    add(Bolus(time=0, amount=1000, compartment="DEPOT", ii=24, addl=3, repeat_option=schedule1))
  
  arm2 <- Arm(2, subjects=1, label="TRT 2") %>%
    add(Bolus(time=0, amount=1000, compartment="DEPOT", ii=24, addl=3, repeat_option=schedule2))
  
  arm3 <- Arm(3, subjects=1, label="TRT 3") %>%
    add(Bolus(time=expectedTimes, amount=1000, compartment="DEPOT"))
  
  dataset <- Dataset() %>%
    add(c(arm1, arm2, arm3)) %>%
    add(Observations(times=seq(0, weeks(3), by=1)))
  
  # results <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1)
  # spaghettiPlot(results, "CP", "ARM") +
  #   ggplot2::facet_wrap(~ARM, ncol=1)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    resultsArm1 <- results %>% dplyr::filter(.data$ARM=="TRT 1") %>% dplyr::mutate(ID=0),
    resultsArm2 <- results %>% dplyr::filter(.data$ARM=="TRT 2") %>% dplyr::mutate(ID=0),
    resultsArm3 <- results %>% dplyr::filter(.data$ARM=="TRT 3") %>% dplyr::mutate(ID=0),
    outputRegressionTest(resultsArm1, output=c("CP"), filename=regFilename),
    outputRegressionTest(resultsArm2, output=c("CP"), filename=regFilename),
    outputRegressionTest(resultsArm3, output=c("CP"), filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
