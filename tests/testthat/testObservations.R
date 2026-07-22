library(testthat)

context("Test the observations class")

test_that("Observations are working well", {
  
  observations <- Observations(times=c(5,6,1,2,3,4,2)) 
  expect_equal(observations@times, TimeVector(c(1,2,3,4,5,6)))
  expect_equal(observations %>% get_name(), "OBS [TIMES=c(1,2,3,4,5,6), CMT=NA]")
})

test_that("Observations exceptions are working well", {
  expect_error(Observations(times=NULL))
  expect_error(Observations(times=numeric(0)), regexp="times is length 0")
})

test_that("Negative times cannot be accepted", {
  expect_error(Observations(times=c(-1, 2)), regexp="Some values in slot 'times' are negative")
})

test_that("Observations set is working as expected", {
  obs1 <- Observations(times=c(0,1,2,3))
  obs2 <- Observations(times=c(3,4,5,6))
  set <- new("observations_set")
  set <- set %>% add(c(obs1, obs2))
  expect_equal(set %>% getTimes(), c(0,1,2,3,4,5,6))
})

test_that("Method getTimes in observations works as expected", {
  obs <- Observations(times=c(0,1,2,4,8,12,24))
  expect_equal(obs %>% getTimes(), c(0,1,2,4,8,12,24))
  
  obs <- Observations(times=c(0,1,2,4,8,12,24), rep=DosingSchedule())
  expect_equal(obs %>% getTimes(doseTimes=c(0,24,48)), unique(c(c(0,1,2,4,8,12,24), c(0,1,2,4,8,12,24)+24, c(0,1,2,4,8,12,24)+48)))
})
