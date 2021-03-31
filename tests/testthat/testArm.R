library(testthat)
library(pmxmod)

context("Test all methods from the arm class")

test_that("Default arm", {
  
  arm <- new("arm") 
  expect_equal(arm@id, 1)
  expect_equal(arm@subjects, 1)
  expect_equal(arm %>% getName(), "ARM 1")
})

test_that("getCovariateNames method works well", {
  arm <- new("arm") 
  arm <- arm %>% add(Covariate("WT", ConstantDistribution(70)))
  arm <- arm %>% add(Covariate("WT2", ConstantDistribution(70)))
  expect_equal(arm %>% getCovariateNames(), c("WT", "WT2"))
})

test_that("getIOVNames method works well", {
  arm <- new("arm") 
  arm <- arm %>% add(IOV(colname="IOV_KA", distribution=NormalDistribution(mean=0, sd=0.1)))
  expect_equal(arm %>% getIOVNames(), c("IOV_KA"))
})

