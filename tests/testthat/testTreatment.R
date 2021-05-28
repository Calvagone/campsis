library(testthat)
library(pmxmod)

context("Test the treatment class")

test_that("Test a couple of methods", {
  
  bolus1 <- Bolus(time=0, amount=1000)
  expect_true(is(bolus1, "bolus"))
  bolus2 <- Bolus(time=24, amount=1000)
  bolus3 <- Bolus(time=48, amount=1000)
  bolus3_dup <- Bolus(time=48, amount=1000)
  
  treatment <- new("treatment")
  treatment <- treatment %>% add(bolus1)
  treatment <- treatment %>% add(bolus2)
  treatment <- treatment %>% add(bolus3)
  
  expect_equal(treatment %>% length(), 3)
  
  # No duplicate in treatment is possible
  expect_error(treatment %>% add(bolus3_dup)) # Element BOLUS [TIME=48, AMOUNT=1000, CMT=NA] is already present.
  
  # Assign dose number
  treatment <- treatment %>% assignDoseNumber()
  expect_equal(treatment@list[[1]]@dose_number, 1)
  expect_equal(treatment@list[[2]]@dose_number, 2)
  expect_equal(treatment@list[[3]]@dose_number, 3)
  
  # Infusion class
  infusion1 <- Infusion(time=0, amount=1000)
  expect_true(is(infusion1, "infusion"))
})

test_that("Bolus using time as numeric vector is working well", {
  boluses <- Bolus(time=(0:6)*24, amount=1000)
  expect_equal(boluses %>% length(), 7)
  
  ds <- Dataset() %>% add(boluses)
  expect_equal(ds@arms@list[[1]]@protocol@treatment %>% length(), 7)
})

test_that("Infusion using time as numeric vector is working well", {
  infusions <- Infusion(time=(0:6)*24, amount=1000)
  expect_equal(infusions %>% length(), 7)
  
  ds <- Dataset() %>% add(infusions)
  expect_equal(ds@arms@list[[1]]@protocol@treatment %>% length(), 7)
})