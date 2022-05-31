library(testthat)

context("Test the simulate method with lag times")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a bolus with fixed lag time in dataset"), {
  model <- model_library$advan4_trans4
  regFilename <- "bolus_fixed_lag_time"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1, lag=2)) %>% # 2-hour lag time
    add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=paste0(regFilename, "_dataset"))
  
  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49 * length(dataset)),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a bolus with fixed lag time in model"), {
  model <- model_library$advan4_trans4
  regFilename <- "bolus_fixed_lag_time"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  # 2 hours lag time, no variability
  model <- model %>% add(LagTime(compartment=1, rhs="2"))

  datasetRegressionTest(dataset, model, seed=seed, filename=paste0(regFilename, "_model"))
  
  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49 * length(dataset)),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
