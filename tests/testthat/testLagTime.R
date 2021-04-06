library(testthat)
library(pmxmod)

context("Test lag times")

test_that("Lag time creation", {
  
  lag <- TreatmentLagTime(compartment=1, FunctionDistribution(fun="rnorm", args=list(mean=1, variance=0.04)))
  expect_equal(lag@compartment, 1)
  expect_equal(lag@distribution@fun, "rnorm")
  expect_equal(lag@distribution@args, list(mean=1, variance=0.04))
  
  # Missing distribution
  expect_error(TreatmentLagTime(compartment=1))
})
