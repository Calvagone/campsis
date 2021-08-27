library(testthat)

context("Test that simulations with weird cases work as expected")
seed <- 1

test_that("Simulate a bolus without observation", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000))

  expect_error(model %>% simulate(dataset, dest="RxODE", seed=seed), regexp="Dataset does not contain any observation")
})

test_that("Simulate a bolus with single observation at time 0 (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000))
  dataset <- dataset %>% add(Observations(time=0))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  
  expect_equal(nrow(results1), 1)
  expect_equal(nrow(results2), 1)
  variables <- c("id", "time", "CP")
  expect_equal(results1[variables], results2[variables])
})

test_that("Simulate a model which is not valid", {
  model <- model_library$advan4_trans4
  
  # Corrupt name slot of parameter KA
  model@parameters@list[[1]]@name <- c("KA", "KA2")
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000))
  dataset <- dataset %>% add(Observations(time=0))
  
  expect_error(model %>% simulate(dataset, dest="RxODE"), regexp="name is length 2. Should be 1.")
})

test_that("Simulate a dataset which is not valid", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000))
  dataset <- dataset %>% add(Observations(time=0))
  
  # Corrupt amount slot of first bolus
  dataset@arms@list[[1]]@protocol@treatment@list[[1]]@amount <- c(1000,1000)
  
  expect_error(model %>% simulate(dataset, dest="RxODE"), regexp="amount is length 2. Should be 1.")
})