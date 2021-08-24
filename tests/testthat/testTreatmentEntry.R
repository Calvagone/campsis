library(testthat)

context("Test all types of treatment entries")

test_that("Bolus is working correctly", {
  bolus <- Bolus(time=0, amount=1000) 
  expect_equal(bolus@time, 0)
  expect_equal(bolus@amount, 1000)
  expect_equal(bolus %>% getName(), "BOLUS [TIME=0, AMOUNT=1000, CMT=NA]")
})

test_that("Infusion is working correctly", {
  
  infusion <- Infusion(time=0, amount=1000) 
  expect_equal(infusion@time, 0)
  expect_equal(infusion@amount, 1000)
  expect_equal(infusion %>% getName(), "INFUSION [TIME=0, AMOUNT=1000, CMT=NA]")
})

test_that("Infusion errors", {
  expect_error(Infusion(time=0)) # amount is missing
})

test_that("Infusion time is negative", {
  expect_error(Infusion(time=-1, amount=100), regexp="Some time values are negative")
})


test_that("Is treatment entry test", {
  expect_true(is(new("bolus", time=0, amount=100), "treatment_entry"))
})

test_that("sample method for bolus is working well", {
  bolus <- Bolus(time=0, amount=1000, f=0.6, lag=ConstantDistribution(2)) 
  res <- bolus %>% sample(n=as.integer(10))
  expect_equal(res$ID, seq_len(10))
  expect_equal(unique(res$AMT), 1000*0.6)
  expect_equal(unique(res$TIME), 0+2)
})

test_that("sample method for infusion is working well", {
  infusion <- Infusion(time=0, amount=1000, duration=2) 
  res <- infusion %>% sample(n=as.integer(10))
  expect_equal(res$ID, seq_len(10))
  expect_equal(unique(res$RATE), 1000/2)
  
  infusion <- Infusion(time=0, amount=1000, rate=200) 
  res <- infusion %>% sample(n=as.integer(10))
  expect_equal(res$ID, seq_len(10))
  expect_equal(unique(res$RATE), 200)
})
