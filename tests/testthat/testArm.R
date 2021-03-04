library(testthat)

context("Test all methods from the arm class")

test_that("Default arm", {
  
  arm <- new("arm") 
  expect_equal(arm@id, 1)
  expect_equal(arm@subjects, 1)
  expect_equal(arm %>% getName(), "ARM 1")
})