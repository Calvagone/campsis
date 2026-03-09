library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import a few basic Campsis datasets from JSON", {
  
  # 1A
  dataset1a <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example1a.json"))
  
  expArm1 <- Arm(subjects=100, label="Arm 1") %>%
    add(Bolus(time=0, amount=50, compartment="ABS", ii=24, addl=6)) %>%
    add(Infusion(time=0, amount=50, compartment="CENTRAL", ii=24, addl=6, duration=2))
  expArm2 <- Arm(subjects=100, label="Arm 2") %>%
    add(Bolus(time=0, amount=100, compartment="ABS", ii=24, addl=6))
  expDataset1a <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(seq(0, 168, by=24)))
  
  expect_equal(dataset1a, expDataset1a)
  
  # 1B
  dataset1b <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example1b.json"))
  expDataset1b <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(TimeSequence(0, 168, by=1)))
  
  expect_equal(dataset1b, expDataset1b)
  
  # 1C
  dataset1c <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example1c.json"))
  expDataset1c <- Dataset() %>%
    add(expArm1) %>%
    add(expArm2) %>%
    add(Observations(TimeSequence(0, 24, by=1), rep=DosingSchedule()))
  
  expect_equal(dataset1c, expDataset1c)
  
  # 1D = 1A but all time units in days
  dataset1d <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example1d.json"))
  expect_equal(dataset1d, expDataset1a)
  
  # 1E = 1B but all time units in days
  dataset1e <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example1e.json"))
  expect_equal(dataset1e, expDataset1b)
  
  # Example 2: dataset settings
  dataset2 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example2.json"))
  expArm2 <- Arm(subjects=100, label="My dataset") %>%
    add(Bolus(time=0, amount=100, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(TimeSequence(0, 24, by=1), rep=DosingSchedule()))
  expDataset2 <- Dataset() %>%
    add(expArm2) %>%
    add(DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE, timeUnitExport="day"))

  expect_equal(dataset2, expDataset2)
  
  # Example 3: dataset covariates
  dataset3 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_example3_covariates.json"))
  expArm3 <- Arm(subjects=100, label="My dataset") %>%
    add(Observations(TimeSequence(0, 24, by=1))) %>%
    add(Covariate("BW1", 70.5)) %>%
    add(Covariate("BW2", c(70.5, 80.5, 90.5))) %>%
    add(Covariate("BW3", NormalDistribution(70.5, 10.5))) %>%
    add(Covariate("BW4", UniformDistribution(50.0, 100.0))) %>%
    add(Covariate("BW5", LogNormalDistribution(4.5, 2.3))) %>%
    add(Covariate("SEX", DiscreteDistribution(x=c(0,1), c(0.6,0.4))))
  expDataset3 <- Dataset() %>%
    add(expArm3)
  
  expect_equal(dataset3, expDataset3)
})

test_that("Import Campsis datasets that include a dose adaptation layer from JSON", {
  dataset <- Dataset(json=file.path(testFolder, "json_examples", "dataset_dose_adaptation_example1.json"))
  expArm <- Arm(subjects=100, label="My dataset") %>%
    add(Bolus(time=0, amount=50, compartment="ABS", ii=24, addl=0)) %>%
    add(Observations(TimeSequence(0, 24, by=1))) %>%
    add(Covariate("BW", NormalDistribution(70.5, 10.5))) %>%
    add(DoseAdaptation("AMT*WT", compartments="ABS")) %>%
    add(DoseAdaptation("TO_ALL_CMTS"))
  expDataset <- Dataset() %>%
    add(expArm)
  
  expect_equal(dataset, expDataset)
})

test_that("Import Campsis datasets that include a bootstrap layer from JSON", {
  # Example 1 with bootstrap
  dataset1 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_bootstrap_example1.json"))
  
  expArm1 <- Arm(subjects=100, label="My dataset") %>%
    add(Observations(TimeSequence(0, 24, by=1))) %>%
    add(Bootstrap(data=data.frame(BS_ID=c(1,2,3), BW=c(70,75,80), AGE=c(30,35,40)), replacement=TRUE, random=TRUE, export_id=TRUE))
  
  expDataset1 <- Dataset() %>%
    add(expArm1)
  
  expect_equal(dataset1, expDataset1)
  
  # Example 2 with bootstrap (same but no row identifier)
  dataset2 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_bootstrap_example2.json"))
  expect_equal(dataset2, expDataset1)
  expect_equal(dataset1, dataset2)
})

test_that("Import Campsis datasets that include a cyclic treatment schedule from JSON", {
  
  dataset1 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_cyclic_schedule_example1.json"))
  
  expArm1 <- Arm(subjects=100, label="My dataset") %>%
    add(Bolus(time=0, amount=50, compartment="ABS", ii=24, addl=6, rep=CyclicSchedule(duration=24*28, repetitions=1))) %>%
    add(Observations(TimeSequence(0, 24, by=1), rep=DosingSchedule()))
  
  expDataset1 <- Dataset() %>%
    add(expArm1)
  
  expect_equal(dataset1, expDataset1)
  
  # Same but duration in hours and unit is not specified
  dataset2 <- Dataset(json=file.path(testFolder, "json_examples", "dataset_cyclic_schedule_example2.json"))
  expect_equal(dataset2, expDataset1)
})

test_that("Import Campsis settings in JSON format", {
  
  # 1A
  settings1a <- Settings(json=file.path(testFolder, "json_examples", "settings_example1a.json"))
  expSettings1a <- Settings(DefaultSettings(engine="mrgsolve", seed=1, outvars=c("CONC", "CONC_ERR"), disabled_variabilities="IIV"))
  
  expect_equal(settings1a, expSettings1a)
  
  # 1B
  settings1b <- Settings(json=file.path(testFolder, "json_examples", "settings_example1b.json"))
  expSettings1b <- Settings(DefaultSettings(engine="mrgsolve", seed=1, outvars=c("CONC", "CONC_ERR")))
  
  expect_equal(settings1b, expSettings1b)
})

test_that("Import Campsis scenarios in JSON format", {
  
  # 1A
  scenarios1a <- Scenarios(json=file.path(testFolder, "json_examples", "scenarios_example1a.json"))
  
  expScenarios1a <- Scenarios() %>%
    add(Scenario(name="Base scenario")) %>%
    add(Scenario(name="Slow KA") %>%
          add(ReplaceAction(Theta(name="KA", value=0.3, label="Absorption rate", unit="1/h"))))
  
  expect_equal(scenarios1a, expScenarios1a)
})
