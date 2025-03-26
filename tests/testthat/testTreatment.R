library(testthat)

context("Test the treatment class")

test_that("Test a couple of methods", {
  
  bolus1 <- Bolus(time=0, amount=1000)
  expect_true(is(bolus1, "bolus"))
  bolus2 <- Bolus(time=24, amount=1000)
  bolus3 <- Bolus(time=48, amount=1000)
  bolus3_dup <- Bolus(time=48, amount=1000)
  
  treatment <- new("treatment")
  treatment@list <- treatment@list %>%
    append(bolus1) %>%
    append(bolus2) %>%
    append(bolus3)
  
  expect_equal(treatment %>% length(), 3)
  
  # Duplicate in treatment is possible since Campsis v1.7.0
  treatment@list <- treatment@list %>%
    append(bolus3_dup)
  
  # Assign dose number
  treatment <- treatment %>%
    assignDoseNumber()
  expect_equal(treatment@list[[1]]@dose_number, 1)
  expect_equal(treatment@list[[2]]@dose_number, 2)
  expect_equal(treatment@list[[3]]@dose_number, 3)
  expect_equal(treatment@list[[4]]@dose_number, 3) # Duplicate gets the same dose no (makes sense to me)
  
  # Infusion class
  infusion1 <- Infusion(time=0, amount=1000)
  expect_true(is(infusion1, "infusion"))
  expect_true(is(infusion1, "infusion_wrapper"))
})

test_that("Bolus using time as numeric vector is working well", {
  boluses <- Bolus(time=(0:6)*24, amount=1000, wrap=F)
  expect_equal(boluses %>% length(), 7)
  
  ds <- Dataset() %>% add(boluses)
  expect_equal(ds@arms@list[[1]]@protocol@treatment %>% length(), 7)
})

test_that("Infusion using time as numeric vector is working well", {
  infusions <- Infusion(time=(0:6)*24, amount=1000, wrap=F)
  expect_equal(infusions %>% length(), 7)
  
  ds <- Dataset() %>% add(infusions)
  expect_equal(ds@arms@list[[1]]@protocol@treatment %>% length(), 7)
})