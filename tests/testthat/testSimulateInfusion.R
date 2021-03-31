library(testthat)
library(pmxmod)

context("Test the simulate method with infusions")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate an infusion using the duration", {
  model <- getNONMEMModelTemplate(3,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 5 hours duration
  dataset <- dataset %>% add(InfusionDuration(compartment=1, ConstantDistribution(5)))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
  datasetRegressionTest(dataset, model, seed=seed, filename="infusion_duration")
})

test_that("Simulate an infusion using the rate", {
  model <- getNONMEMModelTemplate(3,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 5 hours duration
  dataset <- dataset %>% add(InfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
  datasetRegressionTest(dataset, model, seed=seed, filename="infusion_duration")
})

test_that("Simulate an infusion using the rate and lag time", {
  model <- getNONMEMModelTemplate(3,4)
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 2 hours lag time with 20% CV
  lag <- LagTime(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2)))
  
  # 5 hours duration
  dataset <- dataset %>% add(InfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  dataset <- dataset %>% add(lag)

  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  datasetRegressionTest(dataset, model, seed=seed, filename="infusion_rate_lag_time1")
})

test_that("Simulate an infusion using the rate and lag time (parameter distribution)", {
  model <- getNONMEMModelTemplate(3,4)
  model@parameters <- model@parameters %>% add(Theta(name="ALAG1", index=5, value=2)) # 2 hours lag time
  model@parameters <- model@parameters %>% add(Omega(name="ALAG1", index=5, index2=5, value=0.2^2)) #20% CV
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  # Add lag time
  lag <- LagTime(compartment=2, ParameterDistribution(theta="ALAG1", omega="ALAG1"))
  dataset <- dataset %>% add(lag)
  
  # 5 hours duration
  dataset <- dataset %>% add(InfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  expect_true(dataset %>% hasModelDistribution())
  datasetRegressionTest(dataset, model, seed=seed, filename="infusion_rate_lag_time2")
})
