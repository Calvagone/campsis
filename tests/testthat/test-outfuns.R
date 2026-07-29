library(testthat)
library(dplyr)

context("Test the outfun(s) class")

test_that("Output functions can be added and retrieved", {
  outfuns <- Outfuns() %>%
    add(Outfun(~compute_pi(.x, variable="CP"), name="PI on CP")) %>%
    add(Outfun(~compute_pi(.x, variable="Y"), name="PI on Y"))

  expect_equal(outfuns %>% length(), 2)
  expect_equal(outfuns %>% getByIndex(1) %>% .@name, "PI on CP")
  expect_equal(outfuns %>% getByIndex(2) %>% .@name, "PI on Y")
})

test_that("Adding a duplicate name throws an error", {
  expect_error(
    Outfuns() %>%
      add(Outfun(name="Function 1")) %>%
      add(Outfun(name="Function 1")),
    regexp="'Function 1' is already present"
  )
})

test_that("Outfun() extends pmx_element and Outfuns() extends pmx_list", {
  expect_true(is(Outfun(), "pmx_element"))
  expect_true(is(Outfuns(), "pmx_list"))
})

test_that("No level other than 'replicate' is allowed since Campsis v1.9.0", {
  expect_error(
    preprocess_outfun(Outfuns() %>%
      add(Outfun(level="scenario", name="a")) %>%
      add(Outfun(level="replicate", name="b"))),
    regexp="No level other than 'replicate' is allowed"
  )
})
