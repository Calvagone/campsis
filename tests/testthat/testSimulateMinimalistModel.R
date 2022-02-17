library(testthat)

context("Test the simulate method with minimalist examples")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Simulate a minimalist model (no parameter files, no IIV & error model)", {
  regFilename <- "minimalist_model"
  model <- suppressWarnings(read.campsis(paste0(testFolder, "models/", regFilename)))
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  spaghettiPlot(results1, "A_CENTRAL")
  spaghettiPlot(results2, "A_CENTRAL")
  
  outputRegressionTest(results1, output="A_CENTRAL", filename=regFilename)
  outputRegressionTest(results2, output="A_CENTRAL", filename=regFilename)
})