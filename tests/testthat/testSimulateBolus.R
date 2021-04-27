library(testthat)
library(pmxmod)

context("Test the simulate method with boluses")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate a bolus, 2 arms (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "bolus_2arms"
    
  arm1 <- Arm(1, subjects=10)
  arm2 <- Arm(2, subjects=10)
  arm1 <- arm1 %>% add(Bolus(time=0, amount=1000, compartment=1))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm1 <- arm1 %>% add(Observations(times=seq(0,24, by=0.5)))
  arm2 <- arm2 %>% add(Observations(times=seq(0,24, by=0.5)))

  dataset <- Dataset() %>% add(arm1) %>% add(arm2)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP", "ARM")
  shadedPlot(results1, "CP", "ARM")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP", "ARM")
  shadedPlot(results2, "CP", "ARM")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})
