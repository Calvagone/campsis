library(testthat)

context("Test all methods from the arm class")

test_that("Default arm", {
  arm <- Arm()
  expect_equal(arm@id, as.integer(NA))
  expect_equal(arm@subjects, 1)
  expect_equal(arm@label, as.character(NA))
})

test_that("Custom arm", {
  arm <- Arm(id=1, subjects=10, label="Cohort A")
  expect_equal(arm@id, 1)
  expect_equal(arm@subjects, 10)
  expect_equal(arm@label, "Cohort A")
})

test_that("getCovariateNames method works well", {
  arm <- Arm()
  arm <- arm %>% add(Covariate("WT", ConstantDistribution(70)))
  arm <- arm %>% add(Covariate("WT2", ConstantDistribution(70)))
  expect_equal(arm %>% getCovariateNames(), c("WT", "WT2"))
})

test_that("getIOVNames method works well", {
  arm <- Arm()
  arm <- arm %>% add(IOV(colname="IOV_KA", distribution=NormalDistribution(mean=0, sd=0.1)))
  expect_equal(arm %>% getIOVNames(), c("IOV_KA"))
})
