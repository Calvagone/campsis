library(testthat)

context("Test all methods from the event class")

test_that("Minimalist event", {
  
  event <- Event(times=5, fun=function(id, time) {
    # Do something
  })
  expect_equal(event@name, "Unnamed event")
  expect_equal(event@times, 5)
  expect_equal(event@debug, FALSE)
})

test_that("A couple of wrong events", {
  expect_error(Event(times=-5, fun=function(id, time) {}), regexp="Some values in slot 'times' are negative")
  expect_error(Event(times="HELLO", fun=function(id, time) {}))
  expect_error(Event(name=character(0), times=5, fun=function(id, time) {}), regexp="name is length 0")
})

