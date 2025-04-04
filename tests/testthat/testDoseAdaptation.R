library(testthat)

context("Test dose adaptation objects")

test_that("Instantiate dose adaptations work well", {
  
  # Missing formula
  expect_error(DoseAdaptation(), regexp="(argument \"formula\" is missing)|(argument \"formula\" est manquant)")
  
  # Only 1 formula is expected
  expect_error(DoseAdaptation(c("AMT*WT", "HELLO")), regexp="formula is length 2. Should be 1")
  
  # Compartments may be empty, not NA
  expect_error(DoseAdaptation("AMT*WT", compartments=NA), regexp="Some values in slot 'compartments' are NA")
  
  # Compartments undefined
  obj <- DoseAdaptation("AMT*WT")
  expect_equal(obj %>% getName(), "DOSE ADAPTATION [CMT=ALL]")
  expect_equal(capture.output(show(obj)), "-> Dose adaptation (CMT=ALL): AMT*WT")
  
  # Compartments as integer
  obj <- DoseAdaptation("AMT*WT", compartments=c(1,2,3))
  expect_equal(obj %>% getName(), "DOSE ADAPTATION [CMT=c(1,2,3)]")
  expect_equal(capture.output(show(obj)), "-> Dose adaptation (CMT=1,2,3): AMT*WT")
  
  # Compartments as character
  obj <- DoseAdaptation("AMT*WT", compartments=c("DEPOT1", "DEPOT2", "DEPOT3"))
  expect_equal(obj %>% getName(), "DOSE ADAPTATION [CMT=c(DEPOT1,DEPOT2,DEPOT3)]")
})