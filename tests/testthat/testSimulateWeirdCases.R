library(testthat)
library(pmxmod)

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
  expect_equal(results1[variables] %>% as.data.frame(), results2[variables])
})