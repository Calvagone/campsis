library(testthat)

context("Test the simulate method with different bioavailabilities")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus, 2 arms, F1 only in arm1, in dataset", {
  model <- model_library$advan4_trans4
  
  arm1 <- Arm(1, subjects=10)
  arm2 <- Arm(2, subjects=10)
  arm1 <- arm1 %>% add(Bolus(time=0, amount=2000, compartment=1, f=0.75))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=2000, compartment=1))
  arm1 <- arm1 %>% add(Observations(times=seq(0,24, by=0.5)))
  arm2 <- arm2 %>% add(Observations(times=seq(0,24, by=0.5)))
  
  dataset <- Dataset() %>% add(arm1) %>% add(arm2)
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP", "ARM")
  shadedPlot(results, "CP", "ARM")
  
  expect_equal(nrow(results), dataset %>% length() * 49)

  datasetRegressionTest(dataset, model, seed=seed, filename="bolus_2arms_bioavailability")
})

test_that("Simulate a simple bolus with bioavailability (dataset versus model)", {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus_bioavailability"
  
  # Bioavailability implemented in dataset
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1, f=0.75))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 49 * dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
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
  spaghettiPlot(results3, "CP")
  expect_equal(nrow(results3), 49 * dataset %>% length())
  
  results4 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results4, "CP")
  expect_equal(nrow(results4), 49 * dataset %>% length())
  
  outputRegressionTest(results3, output="CP", filename=regFilename)
  outputRegressionTest(results4, output="CP", filename=regFilename)
})


test_that("Simulate several fixed fs at once", {
  model <- model_library$advan4_trans4

  # Bioavailability implemented in dataset
  dataset <- Dataset(4)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1, f=c(0.3, 0.6, 0.9, 1)))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results <- model %>% disable("IIV") %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
  
  cmax <- results %>% dplyr::filter(TIME==2.5) %>% dplyr::pull(CP)
  expect_equal(round(cmax, 2), c(2.89, 5.77, 8.66, 9.62))
})
