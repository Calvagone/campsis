library(testthat)
library(campsismod)

context("Test all methods from the covariates class")

test_that("Add, length, getNames methods work well", {
  
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
  expect_equal(covariates %>% getNames(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Strictly equal to:
  expect_equal(covariates %>% getCovariates() %>% getNames(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Fixed covariates only
  expect_equal(covariates %>% getFixedCovariates() %>% getNames(), c("WT", "AGE"))
  
  # Event covariates only
  expect_equal(covariates %>% getEventCovariates() %>% getNames(), c("DOSE", "STATE"))
  
  # Time-varying covariates only
  expect_equal(covariates %>% getTimeVaryingCovariates() %>% getNames(), c("TEMPERATURE"))
  
  
  # Add covariates to a dataset object
  dataset <- Dataset()
  expect_equal(dataset %>% getCovariates() %>% getNames(), character(0))
  dataset <- dataset %>% addCovariates()

  # Retrieve all names
  expect_equal(dataset %>% getCovariates() %>% getNames(), c("WT", "AGE", "DOSE", "STATE", "TEMPERATURE"))
  
  # Fixed covariates only
  expect_equal(dataset %>% getFixedCovariates() %>% getNames(), c("WT", "AGE"))
  
  # Event covariates only
  expect_equal(dataset %>% getEventCovariates() %>% getNames(), c("DOSE", "STATE"))
  
  # Time-varying covariates only
  expect_equal(dataset %>% getTimeVaryingCovariates() %>% getNames(), c("TEMPERATURE"))
})
