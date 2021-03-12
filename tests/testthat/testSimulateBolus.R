library(testthat)
library(pmxmod)

context("Test the simulate method with boluses")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }

  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
  regressionTest(dataset, model, seed=seed, filename="simple_bolus.csv")
})

test_that("Simulate a bolus, 2 arms", {
  model <- getNONMEMModelTemplate(4,4)
  
  arm1 <- Arm(1, subjects=10)
  arm2 <- Arm(2, subjects=10)
  arm1 <- arm1 %>% add(Bolus(time=0, amount=1000, compartment=1))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=2000, compartment=1))
  
  for (time in seq(0,24, by=0.5)) {
    arm1 <- arm1 %>% add(Observation(time=time))
    arm2 <- arm2 %>% add(Observation(time=time))
  }
  
  dataset <- Dataset() %>% add(arm1) %>% add(arm2)
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP", "ARM")
  shadedPlot(results, "CP", "ARM")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  regressionTest(dataset, model, seed=seed, filename="bolus_2arms.csv")
})

test_that("Simulate a bolus with lag time", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  
  # 2 hours lag time with 20% CV
  lag <- LagTime(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2)))
  dataset <- dataset %>% add(lag)
  
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }

  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  regressionTest(dataset, model, seed=seed, filename="bolus_lag_time.csv")
  
})
