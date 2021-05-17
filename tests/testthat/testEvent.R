library(testthat)
library(pmxmod)

context("Test all methods from the event class")

test_that("Minimalist event", {
  
  event <- Event(times=5, fun=function(id, time) {
    # Do something
  })
  expect_equal(event@name, "Unnamed event")
  expect_equal(event@times, 5)
  expect_equal(event@debug, FALSE)
})

