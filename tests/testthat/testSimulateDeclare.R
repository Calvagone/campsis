library(testthat)

context("Test the declare argument of the simulate function")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Declare argument with mrgsolve", {
  model <- model_library$advan4_trans4
  model <- model %>% replace(Equation("KA", "THETA_KA*exp(ETA_KA + SOMETHING)"))
  regFilename <- "simple_bolus"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  tablefun <- ~.x %>% dplyr::mutate(SOMETHING=0)
  
  # RxODE does not complain
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, tablefun=tablefun)
  
  # mrgsolve complains and cannot build the model
  expect_error(model %>% simulate(dataset, dest="mrgsolve", seed=seed, tablefun=tablefun))

  # mrgsolve does not complain if SOMETHING variable is declared
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, tablefun=tablefun, declare="SOMETHING")
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})