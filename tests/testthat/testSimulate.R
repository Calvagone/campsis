library(testthat)
library(ggplot2)

context("Test the simulate method")

plotCp <- function(results) {
  if ("id" %in% colnames(results)) {
    p <- ggplot(results, aes(x=time, y=CP, group=id)) +
      geom_line()
    print(p)
  } else {
    p <- ggplot(results, aes(x=time, y=CP)) +
      geom_line()
    print(p)
  }
}

test_that("Simulate a bolus", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(DatasetConfig(defObsCmt=2))

  results <- model %>% simulate(dataset, dest="RxODE")
  plotCp(results)
  
  expect_equal(nrow(results), 49)
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
  plotCp(results)
  
  expect_equal(nrow(results), 49*10)
  
})


test_that("Simulate an infusion using the duration", {
  model <- getNONMEMModelTemplate(4,4)

  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, duration=5, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(DatasetConfig(defObsCmt=2))

  results <- model %>% simulate(dataset, dest="RxODE")
  plotCp(results)
})

test_that("Simulate an infusion using the rate", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, rate=200, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(DatasetConfig(defObsCmt=2))
  
  results <- model %>% simulate(dataset, dest="RxODE")
  plotCp(results)
})

test_that("Simulate an infusion using the rate and lag time", {
  model <- getNONMEMModelTemplate(4,4)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, rate=200, compartment=2))
  for (time in seq(0,24, by=0.5)) {
    dataset <- dataset %>% add(Observation(time=time))
  }
  dataset <- dataset %>% add(LagTime(compartment=2, mean=1, variance=0.04))
  dataset <- dataset %>% add(DatasetConfig(defObsCmt=2))
  dataset@arms@list[[1]]@subjects <- as.integer(10)
  
  results <- model %>% simulate(dataset, dest="RxODE")
  plotCp(results)
})