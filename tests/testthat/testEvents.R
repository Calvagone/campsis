library(testthat)
library(campsismod)

context("Test all methods from the events class")

test_that("Add, length, contains, getByName methods work as expected", {
  events <- Events()
  
  event1 <- Event(name="Event 1", times=5, fun=function(id, time) {
    # Do something
  })
  
  event2 <- Event(name="Event 2", times=c(5,10), fun=function(id, time) {
    # Do something
  })
  
  # Add both events to list
  events <- events %>% add(event1) %>% add(event2)
  
  expect_equal(events %>% length(), 2)
  
  # Retrieve event times
  times <- events %>% getTimes()
  expect_equal(times, c(5,10))
})

test_that("Empty events list", {
  events <- Events()
  expect_equal(events %>% length(), 0)
  
  # Retrieve event times
  times <- events %>% getTimes()
  expect_equal(times, numeric(0))
})