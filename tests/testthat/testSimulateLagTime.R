library(testthat)
library(pmxmod)

context("Test the simulate method with lag times")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus with lag time in dataset", {
  model <- getNONMEMModelTemplate(4,4)
  regFilename <- "bolus_lag_time_dataset"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 2 hours lag time with 20% CV
  lag <- LagTime(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2)))
  dataset <- dataset %>% add(lag)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})
