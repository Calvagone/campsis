library(testthat)
library(pmxmod)

context("Test the simulate method with different bioavailabilities")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus, 2 arms", {
  model <- getNONMEMModelTemplate(4,4)
  
  arm1 <- Arm(1, subjects=10)
  arm2 <- Arm(2, subjects=10)
  arm1 <- arm1 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm1 <- arm1 %>% add(Observations(times=seq(0,24, by=0.5)))
  arm2 <- arm2 %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # Add F1=0.75 (20%CV) into first arm
  arm1 <- arm1 %>% add(Bioavailability(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(0.75), sdlog=0.2))))
  
  dataset <- Dataset() %>% add(arm1) %>% add(arm2)
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP", "ARM")
  shadedPlot(results, "CP", "ARM")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  expect_false(dataset %>% hasModelDistribution())
  regressionTest(dataset, model, seed=seed, filename="bolus_2arms_bioavailability.csv")
})
