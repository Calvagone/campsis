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

test_that("Simulate with Outfuns returns a named list; single Outfun still returns a data frame", {
  if (skipLongTests()) return(TRUE)

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0, 1, 2, 4, 8, 12, 24)))

  # Single Outfun: result must be a plain data frame (unchanged behaviour)
  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine,
                                    outfun=Outfun(~PI(.x, variable="CP"), fun_name="cp"),
                                    seed=seed))
  test <- expression(expect_true(is.data.frame(results)))
  campsisTest(simulation, test, env=environment())

  # Outfuns with 2 functions at the same level: result must be a named list
  outfuns <- Outfuns() %>%
    add(Outfun(~PI(.x, variable="CP"), fun_name="cp")) %>%
    add(Outfun(~PI(.x, variable="Y"), fun_name="y"))

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, outfun=outfuns, seed=seed))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("cp", "y")),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["cp"]]))),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["y"]])))
  )
  campsisTest(simulation, test, env=environment())
})
