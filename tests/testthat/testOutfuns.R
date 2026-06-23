library(testthat)
library(dplyr)

context("Test the output_functions class")

seed <- 1
source(file.path(getwd(), test_path(), "testUtils.R"))

test_that("Output functions can be added and retrieved", {
  outfuns <- Outfuns() %>%
    add(Outfun(~PI(.x, variable="CP"), fun_name="PI on CP")) %>%
    add(Outfun(~PI(.x, variable="Y"), fun_name="PI on Y"))

  expect_equal(outfuns %>% length(), 2)
  expect_equal(outfuns %>% getByIndex(1) %>% .@fun_name, "PI on CP")
  expect_equal(outfuns %>% getByIndex(2) %>% .@fun_name, "PI on Y")
})

test_that("Adding a duplicate fun_name throws an error", {
  expect_error(
    Outfuns() %>%
      add(Outfun(fun_name="Function 1")) %>%
      add(Outfun(fun_name="Function 1")),
    regexp="'Function 1' is already present"
  )
})

test_that("output_function extends pmx_element and output_functions extends pmx_list", {
  expect_true(is(Outfun(), "pmx_element"))
  expect_true(is(Outfuns(), "pmx_list"))
})

test_that("No level other than 'replicate' is allowed since Campsis v1.9.0", {
  expect_error(
    preprocessOutfun(Outfuns() %>%
      add(Outfun(level="scenario", fun_name="a")) %>%
      add(Outfun(level="replicate", fun_name="b"))),
    regexp="No level other than 'replicate' is allowed"
  )
})
