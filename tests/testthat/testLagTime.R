library(testthat)
library(pmxmod)

context("Test lag times")

test_that("Lag time creation", {
  
  lag <- LagTime(compartment=1, FunctionDistribution(fun="rnorm", args=list(mean=1, variance=0.04)))
  expect_equal(lag@compartment, 1)
  expect_equal(lag@distribution@fun, "rnorm")
  expect_equal(lag@distribution@args, list(mean=1, variance=0.04))
  
  # Missing distribution
  expect_error(LagTime(compartment=1))
  
  # n <- 1000
  # THETA_V <- 5
  # OMEGA_V <- 0.04
  # 
  # samples1 <- THETA_V * exp(rnorm(n, mean=0, sd=sqrt(OMEGA_V)))
  # sd(log(samples1))
  # mean(log(samples1))
  # 
  # samples2 <- rlnorm(n, meanlog=log(THETA_V), sdlog=sqrt(OMEGA_V))
  # sd(log(samples2))
  # mean(log(samples2))
})