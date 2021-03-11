library(testthat)
library(pmxmod)

context("Test all types of treatment entries")

test_that("Bolus is working correctly", {
  
  bolus <- new("bolus", time=0, amount=1000) 
  expect_equal(bolus@time, 0)
  expect_equal(bolus@amount, 1000)
  expect_equal(bolus %>% getName(), "BOLUS [TIME=0, AMOUNT=1000, CMT=NA]")
})

test_that("Infusion is working correctly", {
  
  infusion <- new("infusion", time=0, amount=1000) 
  expect_equal(infusion@time, 0)
  expect_equal(infusion@amount, 1000)
  expect_equal(infusion %>% getName(), "INFUSION [TIME=0, AMOUNT=1000, CMT=NA]")
})

test_that("Infusion errors", {
  expect_error(Infusion(time=0)) # amount is missing
})


test_that("Is treatment entry test", {
  expect_true(is(new("bolus", time=0, amount=100), "treatment_entry"))
})