
library(testthat)

context("Test all methods from the covariate class")

testFolder <<- ""

test_that("Constant covariate", {
  
  covariate <- new("constant_covariate", name="WT", value=70) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@value, 70)
  
  # No value slot provided
  expect_error(new("constant_covariate", name="WT"))
  
  # Two values provided
  expect_error(new("constant_covariate", name="WT", value=c(60, 70)))
  
})

test_that("Fixed covariate", {
  
  covariate <- new("fixed_covariate", name="WT", values=c(50, 60, 70)) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@values, c(50, 60, 70))
  
  # No values slot provided
  expect_error(new("fixed_covariate", name="WT"))
  
  # Empty values list
  expect_error(new("fixed_covariate", name="WT", values=numeric(0)))
})

test_that("Function covariate", {
  
  covariate <- new("function_covariate", name="WT", fun="rnorm", args=c(mean=70, sd=1)) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@args, c(mean=70, sd=1))
  
  set.seed(1)
  covariate %>% sample(n=as.integer(5))
  
})


