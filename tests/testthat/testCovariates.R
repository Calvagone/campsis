library(testthat)
library(pmxmod)

context("Test all methods from the covariates class")

test_that("Add, length, getNames methods", {
  
  covariates <- new("covariates")
  
  # Add constant covariate
  covariate <- new("constant_covariate", name="WT", value=70)
  covariates <- covariates %>% add(covariate)
  expect_equal(covariates %>% length(), 1)
  
  # Add fixed covariate, WT already there
  covariate <- new("fixed_covariate", name="WT", values=c(60,70,80))
  expect_error(covariates %>% add(covariate))
  
  # Add fixed covariate
  covariate <- new("fixed_covariate", name="BW", values=c(60,70,80))
  covariates <- covariates %>% add(covariate)
  expect_equal(covariates %>% length(), 2)
  
  # Get names test
  expect_equal(covariates %>% getNames(), c("WT", "BW"))
  
})
