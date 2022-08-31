library(testthat)

context("Test the scenario class")

test_that("Empty scenario", {
  scenario <- Scenario()
  expect_equal(scenario@name, as.character(NA))
  expect_equal(scenario@model, ~.x)
  expect_equal(scenario@dataset,  ~.x)
})

test_that("Scenario that overrides the default model", {
  scenario <- Scenario("Scenario 1", model=model_suite$nonmem$advan1_trans1)
  expect_equal(scenario@name, "Scenario 1")
  expect_equal(scenario@model, model_suite$nonmem$advan1_trans1)
  expect_equal(scenario@dataset,  ~.x)
})

test_that("Scenario that overrides the default dataset", {
  scenario <- Scenario("Scenario 2", dataset=Dataset(1))
  expect_equal(scenario@name, "Scenario 2")
  expect_equal(scenario@model, ~.x)
  expect_equal(scenario@dataset,  Dataset(1))
})

test_that("Incorrect scenario arguments are not accepted", {
  expect_error(Scenario(dataset=Dataset(1), model="ANYTHING"),
               regexp="model must be a CAMPSIS model, a function or a lambda formula")
  expect_error(Scenario(dataset="ANYTHING", model=model_suite$nonmem$advan1_trans1),
               regexp="dataset must be a CAMPSIS dataset, a function or a lambda formula")
})

test_that("Incorrect scenario slots must be detected", {
  scenario <- Scenario()
  scenario@name <- c("Scenario 1", "Scenario 2")
  expect_error(validObject(scenario), regexp="name is length 2. Should be 1")
  
  scenario <- Scenario()
  scenario@model <- "ANYTHING"
  expect_error(validObject(scenario), regexp="model must be a CAMPSIS model, a function or a lambda formula")
})
