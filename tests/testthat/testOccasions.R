library(testthat)
library(pmxmod)

context("Test all methods from the occasions class")

test_that("Add, length, getNames methods", {
  
  occasions <- new("occasions")
  
  # Add first occasion
  occ1 <- Occasion(colname="OCC1", values=c(1,2,3), doseNumbers=c(1,2,3)) 
  occasions <- occasions %>% add(occ1)
  expect_equal(occasions %>% length(), 1)
  
  # Add second occasion
  occ2 <- Occasion(colname="OCC2", values=c(1,2,3), doseNumbers=c(1,3,4)) 
  occasions <- occasions %>% add(occ2)
  expect_equal(occasions %>% length(), 2)
  
  # Get names test
  expect_equal(occasions %>% getNames(), c("OCC1", "OCC2"))
  
})