library(testthat)
library(pmxmod)

context("Test all methods from the occasion class")

test_that("Create a simple occasion", {
  occasion <- Occasion(colname="OCC", values=c(1,2,3), doseNumbers=c(1,2,3)) 
  expect_equal(occasion@colname, "OCC")
  expect_equal(occasion@values, c(1,2,3))
  expect_equal(occasion@dose_numbers, c(1,2,3))
})

test_that("Create incorrect occasions", {
  expect_error(Occasion(colname="OCC", values=c(1,2,3), doseNumbers=c(1,2)), regexp="values and dose_numbers slots should have equal lengths")
  expect_error(Occasion(colname="OCC", values=NULL, doseNumbers=1), regexp="values is length 0. Should be at least 1.")
})
