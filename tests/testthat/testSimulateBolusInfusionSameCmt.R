library(testthat)

context("Simulate boluses and infusions into the same compartment")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Bolus and infusion in CMT 1", {

  regFilename <- "bolus_infusion_same_cmt"

  model <- model_library$advan4_trans4
  model <- model %>% add(InfusionDuration(compartment=1, rhs="5"))

  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=5, amount=500, compartment=1))
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results1 <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)

  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49)

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})
