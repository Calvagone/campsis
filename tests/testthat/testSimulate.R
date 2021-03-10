library(testthat)
library(pmxmod)

context("Test the simulate method")

test_that("Simulate a bolus", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
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

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP", "ARM")
  shadedPlot(results, "CP", "ARM")
  
  expect_equal(nrow(results), 49*20)
})

test_that("Simulate a bolus with lag time", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(LagTime(compartment=1, mean=1, variance=0.04))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset@arms@list[[1]]@subjects <- as.integer(10)

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49*10)
  
})


test_that("Simulate an infusion using the duration", {
  model <- getNONMEMModelTemplate(4,4)

  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, duration=5, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
})

test_that("Simulate an infusion using the rate", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, rate=200, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }

  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49)
})

test_that("Simulate an infusion using the rate and lag time", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, rate=200, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(LagTime(compartment=2, mean=1, variance=0.04))
  dataset@arms@list[[1]]@subjects <- as.integer(10)
  
  results <- model %>% simulate(dataset, dest="RxODE")
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 49*10)
})