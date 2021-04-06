library(testthat)
library(pmxmod)

context("Test the simulate method with different bioavailabilities")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus, 2 arms, F1 only in arm1, in dataset", {
  model <- getNONMEMModelTemplate(4,4)
  
  arm1 <- Arm(1, subjects=10)
  arm2 <- Arm(2, subjects=10)
  arm1 <- arm1 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm1 <- arm1 %>% add(Observations(times=seq(0,24, by=0.5)))
  arm2 <- arm2 %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # Add F1=0.75 (20%CV) into first arm
  arm1 <- arm1 %>% add(TreatmentBioavailability(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(0.75), sdlog=0.2))))
  
  dataset <- Dataset() %>% add(arm1) %>% add(arm2)
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP", "ARM")
  shadedPlot(results, "CP", "ARM")
  
  expect_equal(nrow(results), dataset %>% length() * 49)
  expect_false(dataset %>% hasModelDistribution())
  
  datasetRegressionTest(dataset, model, seed=seed, filename="bolus_2arms_bioavailability")
})

test_that("Simulate a simple bolus with bioavailability", {
  model <- getNONMEMModelTemplate(4,4)
  regFilename <- "simple_bolus_bioavailability"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # Bioavailability implemented in dataset
  dataset <- dataset %>% add(TreatmentBioavailability(compartment=1, distribution=ConstantDistribution(0.75)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49 * dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49 * dataset %>% length())
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # Bioavailability implemented in model
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results3 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49 * dataset %>% length())
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49 * dataset %>% length())
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})
