library(testthat)
library(campsismod)

context("Test all methods from the covariates class")

test_that("Add, length, get_names methods work well", {
  
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
  expect_equal(covariates %>% get_names(), c("WT", "BW"))
})

test_that("Selecting specific covariates works as expected", {
  
  addCovariates <- function(x) {
    x <- x %>% add(Covariate("WT", 70))
    x <- x %>% add(Covariate("AGE", 40))
    x <- x %>% add(EventCovariate("DOSE", 1000))
    x <- x %>% add(EventCovariate("STATE", 1000))
    x <- x %>% add(TimeVaryingCovariate("TEMPERATURE", data.frame(TIME=c(0, 24), VALUE=c(37, 36.5))))
    return(x)
  }
  
  # Add covariates to the covariates object
  covariates <- new("covariates")
  covariates <- covariates %>% addCovariates()

  # Retrieve all names
  expect_equal(covariates %>% get_names(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Strictly equal to:
  expect_equal(covariates %>% get_covariates() %>% get_names(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Fixed covariates only
  expect_equal(covariates %>% get_fixed_covariates() %>% get_names(), c("WT", "AGE"))
  
  # Event covariates only
  expect_equal(covariates %>% get_event_covariates() %>% get_names(), c("DOSE", "STATE"))
  
  # Time-varying covariates only
  expect_equal(covariates %>% get_time_varying_covariates() %>% get_names(), c("TEMPERATURE"))
  
  
  # Add covariates to a dataset object
  dataset <- Dataset()
  expect_equal(dataset %>% get_covariates() %>% get_names(), character(0))
  dataset <- dataset %>% addCovariates()

  # Retrieve all names
  expect_equal(dataset %>% get_covariates() %>% get_names(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Fixed covariates only
  expect_equal(dataset %>% get_fixed_covariates() %>% get_names(), c("WT", "AGE"))
  
  # Event covariates only
  expect_equal(dataset %>% get_event_covariates() %>% get_names(), c("DOSE", "STATE"))
  
  # Time-varying covariates only
  expect_equal(dataset %>% get_time_varying_covariates() %>% get_names(), c("TEMPERATURE"))
})
