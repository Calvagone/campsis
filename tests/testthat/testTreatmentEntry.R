library(testthat)

context("Test all types of treatment entries")

test_that("Bolus is working correctly", {
  bolus <- Bolus(time=0, amount=1000) 
  expect_equal(bolus@time, 0)
  expect_equal(bolus@amount, 1000)
  expect_equal(bolus %>% getName(), "BOLUS [TIME=0, CMT=DEFAULT]")
})

test_that("Infusion is working correctly", {
  
  infusion <- Infusion(time=0, amount=1000) 
  expect_equal(infusion@time, 0)
  expect_equal(infusion@amount, 1000)
  expect_equal(infusion %>% getName(), "INFUSION [TIME=0, CMT=DEFAULT]")
})

test_that("Infusion errors", {
  expect_error(Infusion(time=0)) # amount is missing
})

test_that("Infusion time is negative", {
  expect_error(Infusion(time=-1, amount=100), regexp="Some values in slot 'time' are negative")
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

test_that("'time' vector or 'ii' and 'addl' are equivalent (boluses)", {
  boluses1 <- Bolus(time=c(0,24,48), amount=100)
  expect_equal(boluses1 %>% length(), 3)
  
  boluses2 <- Bolus(time=0, amount=100, ii=24, addl=2)
  expect_equal(boluses2 %>% length(), 3)
  
  expect_equal(boluses1, boluses2)
  
  bolus1 <- Bolus(time=24, amount=100)
  bolus2 <- Bolus(time=24, amount=100, ii=24, addl=0)
  
  expect_equal(list(bolus1), bolus2)
})

test_that("'time' vector or 'ii' and 'addl' are equivalent (infusions)", {
  infusions1 <- Infusion(time=c(0,24,48), amount=100)
  expect_equal(infusions1 %>% length(), 3)
  
  infusions2 <- Infusion(time=0, amount=100, ii=24, addl=2)
  expect_equal(infusions2 %>% length(), 3)
  
  expect_equal(infusions1, infusions2)
  
  infusion1 <- Infusion(time=24, amount=100)
  infusion2 <- Infusion(time=24, amount=100, ii=24, addl=0)
  
  expect_equal(list(infusion1), infusion2)
})

test_that("assertions on 'ii' and 'addl' work well", {
  
  expect_error(Bolus(time=c(0), amount=100, ii=24), regexp="addl can't be NULL if ii is specified")
  expect_error(Infusion(time=c(0), amount=100, ii=24), regexp="addl can't be NULL if ii is specified")
  expect_error(Bolus(time=c(0), amount=100, addl=0), regexp="ii can't be NULL if addl is specified")
  expect_error(Bolus(time=c(24,48), amount=100, ii=24, addl=0), regexp="time must be a single numeric value if used with ii and addl")
  expect_error(Bolus(time=0, amount=100, ii=24, addl=-2), regexp="addl must be positive")
  expect_error(Bolus(time=0, amount=100, ii=-2, addl=2), regexp="ii must be higher than 0")
  expect_error(Bolus(time=0, amount=100, ii=24, addl=5.2), regexp="addl must be a single integer value")
  expect_error(Bolus(time=0, amount=100, ii=c(24,48), addl=2), regexp="ii must be a single numeric value")
})

test_that("Bolus or infusion wrapper", {
  wrapper <- Infusion(time=0, amount=100, compartment="CENTRAL", ii=24, addl=2, wrap=T, ref="Admin1")
  expect_equal(as.character(class(wrapper)), "infusion_wrapper")
  expect_equal(wrapper %>% getName(), "INFUSION WRAPPER [REF=Admin1]")
  expect_equal(wrapper@ii, 24)
  expect_equal(wrapper@addl, 2)
  
  wrapper <- Bolus(time=0, amount=100, compartment=c("DEPOT1", "DEPOT2"), ii=24, addl=2, wrap=T, ref="Admin1")
  expect_equal(as.character(class(wrapper)), "bolus_wrapper")
  expect_equal(wrapper %>% getName(), "BOLUS WRAPPER [REF=Admin1]")
  expect_equal(wrapper@ii, 24)
  expect_equal(wrapper@addl, 2)
})








