library(testthat)
library(campsismod)

context("Test the simulate method with infusions")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate infusion using duration in dataset, then in model", {
  model <- model_library$advan3_trans4
  regFilename <- "infusion_duration"
  
  # 5 hours infusion duration implemented in dataset
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1, duration=5))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
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
  spaghettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49)
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49)
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate in dataset", {
  model <- model_library$advan3_trans4
  regFilename <- "infusion_duration"
  
  # 5 hours infusion duration implemented in dataset
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1, rate=200))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # 5 hours infusion duration implemented in model
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  model <- model %>% add(InfusionRate(compartment=1, rhs="200"))
  
  results3 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49)
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49)
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate and lag time in dataset", {
  model <- model_library$advan3_trans4
  regFilename <- "infusion_rate_lag_time1_dataset"
  
  # 5 hours duration
  duration <- 5
  # 2 hours lag time with 20% CV
  lag <- FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1, duration=duration, lag=lag))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate infusion using rate and lag time (parameter distribution) in dataset", {
  model <- model_library$advan3_trans4
  regFilename <- "infusion_rate_lag_time2_dataset"
  model <- model %>% add(Theta(name="ALAG1", index=5, value=2)) # 2 hours lag time
  model <- model %>% add(Omega(name="ALAG1", index=5, index2=5, value=0.2^2)) #20% CV

  dataset <- Dataset(10)
  lag <- ParameterDistribution(model, theta="ALAG1", omega="ALAG1")
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1, rate=200, lag=lag))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), dataset %>% length() * 49)
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), dataset %>% length() * 49)
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})
