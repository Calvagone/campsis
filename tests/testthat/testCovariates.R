library(testthat)
library(campsismod)

context("Test all methods from the covariates class")

test_that("Add, length, getNames methods", {
  
  covariates <- new("covariates")
  
  # Add constant covariate
  covariate <- Covariate("WT", ConstantDistribution(70)) 
  covariates <- covariates %>% add(covariate)
  expect_equal(covariates %>% length(), 1)
  
  # Add fixed covariate, WT already there
  covariate <- Covariate("WT", FixedDistribution(c(60,70,80)))
  expect_error(covariates %>% add(covariate))
  
  # Add fixed covariate
  covariate <- Covariate("BW", FixedDistribution(c(60,70,80)))
  covariates <- covariates %>% add(covariate)
  expect_equal(covariates %>% length(), 2)
  
  # Get names test
  expect_equal(covariates %>% getNames(), c("WT", "BW"))
  
})

test_that("Select method", {
  
  covariates <- new("covariates")
  
  covariates <- covariates %>% add(Covariate("WT", 70))
  covariates <- covariates %>% add(Covariate("AGE", 40))
  covariates <- covariates %>% add(EventCovariate("DOSE", 1000))
  covariates <- covariates %>% add(EventCovariate("STATE", 1000))

  expect_equal(covariates %>% getNames(), c("WT", "AGE", "DOSE", "STATE"))
  expect_equal(covariates %>% select("event_covariate") %>% getNames(), c("DOSE", "STATE"))
})
