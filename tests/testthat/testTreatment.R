library(testthat)

context("Test the treatment class")

test_that("Test a couple of methods", {
  
  bolus1 <- new("bolus", time=0, amount=1000)
  bolus2 <- new("bolus", time=24, amount=1000)
  bolus3 <- new("bolus", time=48, amount=1000)
  bolus3_dup <- new("bolus", time=48, amount=1000)
  
  treatment <- new("treatment")
  treatment <- treatment %>% add(bolus1)
  treatment <- treatment %>% add(bolus2)
  treatment <- treatment %>% add(bolus3)
  
  expect_equal(treatment %>% length(), 3)
  
  # No duplicate in treatment is possible
  expect_error(treatment %>% add(bolus3_dup)) # Element BOLUS [TIME=48, AMOUNT=1000, CMT=NA] is already present.
})