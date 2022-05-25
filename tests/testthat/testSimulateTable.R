library(testthat)

context("Test the simulate method with 2-dimensional datasets")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Simulate a bolus (rxode2/mrgsolve) by giving the exported dataset (table form)", {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus"

  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  # rxode2, via exported table
  table <- dataset %>% export(dest="rxode2", model=model, seed=seed)
  results1 <- model %>% simulate(table, dest="rxode2", seed=seed)

  # Mrgsolve, via exported table
  table <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
  results2 <- model %>% simulate(table, dest="mrgsolve", seed=seed)

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)

  # Check ARM column is not needed
  table <- dataset %>% export(dest="rxode2", model=model, seed=seed) %>% dplyr::select(-ARM)
  results3 <- model %>% simulate(table, dest="rxode2", seed=seed)
  outputRegressionTest(results3, output="CP", filename=regFilename)
})
