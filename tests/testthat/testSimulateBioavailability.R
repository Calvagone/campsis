library(testthat)

context("Test the simulate method with different bioavailabilities")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a bolus, 2 arms, F1 only in arm1, in dataset"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "bolus_2arms_bioavailability"
  
  arm1 <- Arm(1, subjects=10) %>%
    add(Bolus(time=0, amount=2000, compartment=1, f=0.75)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  arm2 <- Arm(2, subjects=10) %>%
    add(Bolus(time=0, amount=2000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  dataset <- Dataset() %>%
    add(arm1) %>%
    add(arm2)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), dataset %>% length() * 49)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a simple bolus with bioavailability, dataset versus model"), {
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "simple_bolus_bioavailability"
  
  # Bioavailability implemented in dataset
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1, f=0.75)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), dataset %>% length() * 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())

  # Bioavailability implemented in model
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), dataset %>% length() * 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})


test_that(getTestName("Simulate several fixed F's at once"), {
  model <- model_suite$testing$nonmem$advan4_trans4

  # Bioavailability implemented in dataset
  dataset <- Dataset(4) %>%
    add(Bolus(time=0, amount=1000, compartment=1, f=c(0.3, 0.6, 0.9, 1))) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  simulation <- expression(model %>% disable("IIV") %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    cmax <- results %>% dplyr::filter(TIME==2.5) %>% dplyr::pull(CP),
    expect_equal(round(cmax, 2), c(2.89, 5.77, 8.66, 9.62))
  )
  campsisTest(simulation, test, env=environment())
})
