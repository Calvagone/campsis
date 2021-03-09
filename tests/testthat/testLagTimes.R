library(testthat)

context("Test lag times")

test_that("Test add, getName, getByCompartment methods", {
  
  lags <- new("lag_times")
  lag1 <- LagTime(compartment=1, mean=1, variance=0.04)
  lag2 <- LagTime(compartment=2, mean=1, variance=0.04)
  
  lags <- lags %>% add(lag1) %>% add(lag2)
  
  expect_equal(lags %>% length(), 2)
  expect_equal(lag1 %>% getName(), "LAG_TIME [CMT=1]")
  expect_equal(lags %>% getByCompartment(as.integer(2)), lag2)
})