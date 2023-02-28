library(testthat)

context("Test the simulation settings")

test_that("Default simulation settings work as expected", {
  
  settings <- Settings()
  
  # Hardware settings, default values
  expect_equal(settings@hardware@cpu, 1)
  expect_equal(settings@hardware@parallel, FALSE)
  
  # NOCB settings, default values
  expect_equal(settings@nocb@enable, NA)
  expect_equal(settings@nocb@variables, character(0))
})

test_that("Hardware settings work as expected", {
  
  settings <- Settings(cpu=10, parallel=TRUE)
  
  # Hardware settings, default values
  expect_equal(settings@hardware@cpu, 10)
  expect_equal(settings@hardware@parallel, TRUE)
  
  # NOCB settings, default values
  expect_equal(settings@nocb@enable, NA)
  expect_equal(settings@nocb@variables, character(0))
})

test_that("NOCB settings work as expected", {
  
  settings <- Settings(NOCB(FALSE, "OCC"))
  
  # Hardware settings, default values
  expect_equal(settings@hardware@cpu, 1)
  expect_equal(settings@hardware@parallel, FALSE)
  
  # NOCB settings, overridden values
  expect_equal(settings@nocb@enable, FALSE)
  expect_equal(settings@nocb@variables, "OCC")
})
