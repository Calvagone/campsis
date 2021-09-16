library(testthat)

context("Test dose adaptation objects")

test_that("Instantiate dose adaptations work well", {
  
  # Missing formula
  expect_error(DoseAdaptation(), regexp="argument \"formula\" is missing")
  
  # Only 1 formula is expected
  expect_error(DoseAdaptation(c("AMT*WT", "HELLO")), regexp="formula is length 2. Should be 1")
  
  # Compartments may be empty, not NA
  expect_error(DoseAdaptation("AMT*WT", compartments=NA), regexp="Some values in slot 'compartments' are NA")
  
  # OK
  obj <- DoseAdaptation("AMT*WT")
  
  # OK
  obj <- DoseAdaptation("AMT*WT", compartments=c(1,2,3))
  
  expect_equal(obj %>% getName(), "DOSE ADAPTATION [CMTS=c(1,2,3)]")
})