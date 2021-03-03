
library(testthat)

context("Test all methods from the covariate class")

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
  
  # Example 1
  covariate <- new("function_covariate", name="VAR", fun="sample.int", args=list(n=10, size="n")) 
  expect_equal(covariate@name, "VAR")
  expect_equal(covariate@args, list(n=10, size="n"))
  
  set.seed(1)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(covariate@values, c(9, 4, 7, 1, 2))
  
  # Example 2
  covariate <- new("function_covariate", name="WT", fun="rnorm", args=list(mean=70, sd=5)) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@args, list(mean=70, sd=5))
  
  set.seed(1)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(round(covariate@values), c(67, 71, 66, 78, 72))
})


