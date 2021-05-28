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
  expect_error(Observations(times=numeric(0)), regexp="times is length 0")
})

test_that("Negative times cannot be accepted", {
  expect_error(Observations(times=c(-1, 2)), regexp="Some time values are negative")
})

test_that("Observations set is working as expected", {
  obs1 <- Observations(times=c(0,1,2,3))
  obs2 <- Observations(times=c(3,4,5,6))
  set <- new("observations_set")
  set <- set %>% add(c(obs1, obs2))
  expect_equal(set %>% getTimes(), c(0,1,2,3,4,5,6))
})