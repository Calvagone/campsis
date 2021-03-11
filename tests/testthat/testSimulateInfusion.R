library(testthat)
library(pmxmod)

context("Test the simulate method with infusions")


test_that("Simulate an infusion using the duration", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(InfusionDuration(compartment=2, ConstantDistribution(5)))
  
  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
})

test_that("Simulate an infusion using the rate", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(InfusionDuration(compartment=2, ConstantDistribution(200), rate=TRUE))
  
  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
})

test_that("Simulate an infusion using the rate and lag time", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, rate=200, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  # 2 hours lag time with 20% CV
  lag <- LagTime(compartment=2, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2)))
  dataset <- dataset %>% add(lag)

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
})