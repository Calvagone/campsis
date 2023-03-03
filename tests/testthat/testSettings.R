library(testthat)

context("Test the simulation settings")

test_that("Default simulation settings work as expected", {
  
  settings <- Settings()
  
  # Hardware settings, default values
  expect_equal(settings@hardware@cpu, 1)
  expect_equal(settings@hardware@slice_parallel, FALSE)
  expect_equal(settings@hardware@slice_size, as.integer(NA))
  expect_equal(settings@hardware@dataset_parallel, FALSE)
  expect_equal(settings@hardware@dataset_slice_size, 500)
  
  # Solver settings, default values
  expect_equal(settings@solver@atol, 1e-08)
  expect_equal(settings@solver@rtol, 1e-08)
  expect_equal(settings@solver@hmax, as.numeric(NA))
  expect_equal(settings@solver@maxsteps, 70000)
  expect_equal(settings@solver@method, "liblsoda")
  
  # NOCB settings, default values
  expect_equal(settings@nocb@enable, NA)
  expect_equal(settings@nocb@variables, character(0))
})

test_that("Hardware settings work as expected", {
  
  settings <- Settings(Hardware(cpu=10, slice_parallel=TRUE, slice_size=100,
                                dataset_parallel=TRUE, dataset_slice_size=250))
  
  # Hardware settings, default values
  expect_equal(settings@hardware@cpu, 10)
  expect_equal(settings@hardware@slice_parallel, TRUE)
  expect_equal(settings@hardware@slice_size, as.integer(100))
  expect_equal(settings@hardware@dataset_parallel, TRUE)
  expect_equal(settings@hardware@dataset_slice_size, 250)
})

test_that("NOCB settings work as expected", {
  
  settings <- Settings(NOCB(FALSE, "OCC"))

  # NOCB settings, overridden values
  expect_equal(settings@nocb@enable, FALSE)
  expect_equal(settings@nocb@variables, "OCC")
})
