library(testthat)
library(pmxmod)

context("Test all methods from the events class")

test_that("Add, length, contains, getByName methods work as expected", {
  events <- Events()
  
  event1 <- Event(times=5, fun=function(id, time) {
    # Do something
  })
  
  events <- events %>% add(event1)
  
  expect_equal(events %>% length(), 1)
})