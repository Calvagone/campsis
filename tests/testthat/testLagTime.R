library(testthat)

context("Test lag times")

test_that("Lag time creation", {
  
  lag <- LagTime(compartment=1, mean=1, variance=0.04)
  expect_equal(lag@compartment, 1)
  expect_equal(lag@mean, 1)
  expect_equal(lag@variance, 0.04)
  
  # Missing variance
  expect_error(LagTime(compartment=1, mean=1))
})