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
  
  infusion <- new("infusion", time=0, amount=1000, duration=2.5) 
  expect_equal(infusion@time, 0)
  expect_equal(infusion@amount, 1000)
  expect_equal(infusion@duration, 2.5)
  expect_equal(infusion %>% getName(), "INFUSION [TIME=0, AMOUNT=1000, CMT=NA]")
})

test_that("Infusion errors", {
  expect_error(Infusion(time=0, amount=1000)) # Please specify either the duration or rate
})

test_that("Infusion duration or rate", {
  infusion1 <- Infusion(time=0, amount=1000, duration=5)
  infusion2 <- Infusion(time=0, amount=1000, rate=200)
  
  expect_equal(infusion1 %>% getRate(), infusion2 %>% getRate())
})

test_that("Is treatment entry test", {
  expect_true(is(new("bolus", time=0, amount=100), "treatment_entry"))
})