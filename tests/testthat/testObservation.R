library(testthat)
library(pmxmod)

context("Test all types of observations")

test_that("Simple observation is working well", {
  
  observation <- new("observation", time=0) 
  expect_equal(observation@time, 0)
  expect_equal(observation %>% getName(), "OBS [TIME=0, CMT=NA]")
})