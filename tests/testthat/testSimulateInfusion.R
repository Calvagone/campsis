library(testthat)
library(pmxmod)

context("Test the simulate method with infusions")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate infusion using duration in dataset, then in model", {
  model <- getNONMEMModelTemplate(3,4)
  regFilename <- "infusion_duration"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 5 hours infusion duration implemented in dataset
  dataset <- dataset %>% add(TreatmentInfusionDuration(compartment=1, ConstantDistribution(5)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # 5 hours infusion duration implemented in model
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  model <- model %>% add(InfusionDuration(compartment=1, rhs="5"))
  
  results3 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49)
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49)
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate in dataset", {
  model <- getNONMEMModelTemplate(3,4)
  regFilename <- "infusion_duration"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 5 hours infusion duration implemented in dataset
  dataset <- dataset %>% add(TreatmentInfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # 5 hours infusion duration implemented in model
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  model <- model %>% add(InfusionDuration(compartment=1, rhs="200", rate=TRUE))
  
  results3 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49)
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49)
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate and lag time in dataset", {
  model <- getNONMEMModelTemplate(3,4)
  regFilename <- "infusion_rate_lag_time1_dataset"
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # 2 hours lag time with 20% CV
  lag <- TreatmentLagTime(compartment=1, FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2)))
  
  # 5 hours duration
  dataset <- dataset %>% add(TreatmentInfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  dataset <- dataset %>% add(lag)

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate and lag time (parameter distribution) in dataset", {
  model <- getNONMEMModelTemplate(3,4)
  regFilename <- "infusion_rate_lag_time2_dataset"
  model@parameters <- model@parameters %>% add(Theta(name="ALAG1", index=5, value=2)) # 2 hours lag time
  model@parameters <- model@parameters %>% add(Omega(name="ALAG1", index=5, index2=5, value=0.2^2)) #20% CV
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  # Add lag time
  lag <- TreatmentLagTime(compartment=2, ParameterDistribution(theta="ALAG1", omega="ALAG1"))
  dataset <- dataset %>% add(lag)
  
  # 5 hours duration
  dataset <- dataset %>% add(TreatmentInfusionDuration(compartment=1, ConstantDistribution(200), rate=TRUE))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  expect_true(dataset %>% hasModelDistribution())
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})
