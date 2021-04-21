library(testthat)
library(pmxmod)

context("Test the observations class")

test_that("Observations are working well", {
  
  observations <- Observations(times=c(5,6,1,2,3,4,2)) 
  expect_equal(observations@times, c(1,2,3,4,5,6))
  expect_equal(observations %>% getName(), "OBS [TIMES=c(1,2,3,4,5,6), CMT=NA]")
})

test_that("Observations exceptions are working well", {
  expect_error(Observations(times=NULL))
  expect_error(Observations(times=numeric(0))) # times is length 0. Should be at least 1.
})