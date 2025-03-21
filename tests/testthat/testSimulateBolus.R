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
