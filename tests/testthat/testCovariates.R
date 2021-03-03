
library(testthat)

context("Test all methods from the covariates class")

test_that("Add and length methods", {
  
  covariates <- new("covariates")
  
  # Add constant covariate
  covariate <- new("constant_covariate", name="WT", value=70)
  covariates <- covariates %>% add(covariate)
  expect_equal(covariates %>% length(), 1)
  
  # Add fixed covariate
  covariate <- new("fixed_covariate", name="WT", values=c(60,70,80))
  expect_error(covariates %>% add(covariate))
})
