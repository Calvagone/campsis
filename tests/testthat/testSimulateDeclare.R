library(testthat)

context("Test the declare argument of the simulate function")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Declare argument with mrgsolve", {
  model <- model_suite$nonmem$advan4_trans4
  model <- model %>% replace(Equation("KA", "THETA_KA*exp(ETA_KA + SOMETHING)"))
  regFilename <- "simple_bolus"
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>% 
    add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  tablefun <- ~.x %>% dplyr::mutate(SOMETHING=0)
  
  test <- expression(
    if (destEngine %in% c("RxODE", "rxode2")) {
      # RxODE does not complain
      results <- model %>% simulate(dataset, dest=destEngine, seed=seed, tablefun=tablefun)
      outputRegressionTest(results, output="CP", filename=regFilename)
    },
    if (destEngine %in% c("mrgsolve")) {
      # mrgsolve complains and cannot build the model
      expect_error(model %>% simulate(dataset, dest=destEngine, seed=seed, tablefun=tablefun))
      
      # mrgsolve does not complain if SOMETHING variable is declared
      results <- model %>% simulate(dataset, dest=destEngine, seed=seed, tablefun=tablefun, declare="SOMETHING")
      outputRegressionTest(results, output="CP", filename=regFilename)
    }
  )
  campsisTest(expression(), test, env=environment())
})
