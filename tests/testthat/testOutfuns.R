library(testthat)

context("Test the output_functions class")

test_that("Output functions can be added and retrieved", {
  outfuns <- Outfuns() %>%
    add(Outfun(~PI(.x, variable="CP"), level="scenario")) %>%
    add(Outfun(~PI(.x, variable="CP"), level="replicate"))

  expect_equal(outfuns %>% length(), 2)
  expect_equal(outfuns %>% getByIndex(1) %>% .@level, "scenario")
  expect_equal(outfuns %>% getByIndex(2) %>% .@level, "replicate")
})

test_that("Adding a duplicate level throws an error", {
  expect_error(
    Outfuns() %>%
      add(Outfun(level="scenario")) %>%
      add(Outfun(level="scenario")),
    regexp="already present"
  )
})

test_that("output_function extends pmx_element and output_functions extends pmx_list", {
  expect_true(is(Outfun(), "pmx_element"))
  expect_true(is(Outfuns(), "pmx_list"))
})
