library(testthat)
library(pmxmod)

context("Test all methods from the dataset class")

test_that("Add entry, order, filter (simple example)", {
  
  dataset <- Dataset() 
  
  # Add doses
  dataset <- dataset %>% add(Bolus(time=0, amount=100))
  dataset <- dataset %>% add(Bolus(time=24, amount=100))
  dataset <- dataset %>% add(Bolus(time=48, amount=100))


  # Add observations
  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% add(Observation(time=t))
  }
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  expect_equal(nrow(table), 16)
})

test_that("Two arms example", {
  
  # Create 2 arms
  arm1 <- Arm(id=1, subjects=4)
  arm2 <- Arm(id=2, subjects=3)

  # Add doses in respective arms
  arm1 <- arm1 %>% add(Bolus(time=0, amount=100))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=200))
    
  # Add observations
  for (t in seq(0, 48, by=4)) {
    obs <- Observation(time=t)
    arm1 <- arm1 %>% add(obs)
    arm2 <- arm2 %>% add(obs)
  }
  
  # Create dataset
  dataset <- Dataset() 
  dataset <- dataset %>% add(arm1)
  dataset <- dataset %>% add(arm2)
  
  # Total number of subjects
  expect_equal(dataset %>% length(), 7)
  
  # Arms number
  expect_equal(length(dataset@arms), 2)
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  expect_equal(nrow(table), 98)
  
})

test_that("Export using config", {
  
  dataset <- Dataset() 
  
  # Add doses
  dataset <- dataset %>% add(Bolus(time=0, amount=100))
  dataset <- dataset %>% add(Bolus(time=24, amount=100))
  dataset <- dataset %>% add(Bolus(time=48, amount=100))
  
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    dataset <- dataset %>% add(Observation(time=t))
  }
  
  # Export to RxODE
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  expect_true(all(c(1,2) %in% table$CMT))
  
})

test_that("Export constant covariates work well (N=1, N=2)", {
  
  dataset <- Dataset()
  
  # Add doses
  dataset <- dataset %>% add(Bolus(time=0, amount=100))
  dataset <- dataset %>% add(Bolus(time=24, amount=100))
  dataset <- dataset %>% add(Bolus(time=48, amount=100))
  
  # Add covariate
  dataset <- dataset %>% add(Covariate(name="WT", ConstantDistribution(value=70)))
  dataset <- dataset %>% add(Covariate(name="HT", ConstantDistribution(value=180)))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    dataset <- dataset %>% add(Observation(time=t))
  }
  
  # Export to RxODE N=1
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  
  # Export to RxODE N=2
  arm <- dataset@arms %>% default()
  arm@subjects <- as.integer(2)
  dataset@arms <- dataset@arms %>% pmxmod::replace(arm)
  
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  
})

test_that("Export fixed covariates work well (N=3)", {

  arm <- Arm(id=1, subjects=3)
  
  # Add doses
  arm <- arm %>% add(Bolus(time=0, amount=100))
  arm <- arm %>% add(Bolus(time=24, amount=100))
  arm <- arm %>% add(Bolus(time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(Covariate(name="WT", FixedDistribution(values=c(65, 70, 75))))
  arm <- arm %>% add(Covariate(name="HT", FixedDistribution(values=c(175, 180, 185))))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(Observation(time=t))
  }
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, data.frame(ID=c(1,2,3), WT=c(65,70,75), HT=c(175,180,185)))
})


test_that("Export function covariates work well (N=3)", {
  
  arm <- Arm(id=1, subjects=3)
  
  # Add doses
  arm <- arm %>% add(Bolus(time=0, amount=100))
  arm <- arm %>% add(Bolus(time=24, amount=100))
  arm <- arm %>% add(Bolus(time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(Covariate(name="WT", FunctionDistribution(fun="rnorm", args=list(mean=70, sd=10))))
  arm <- arm %>% add(Covariate(name="HT", FunctionDistribution(fun="rnorm", args=list(mean=180, sd=20))))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(Observation(time=t))
  }
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  set.seed(1)
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct() %>% dplyr::mutate(WT=round(WT), HT=round(HT))
  expect_equal(subTable, data.frame(ID=c(1,2,3), WT=c(64,72,62), HT=c(212,187,164)))
})

test_that("Export boostrap covariates work well (N=8)", {
  
  arm <- new("arm", id=as.integer(1), subjects=as.integer(8))
  
  # Add doses
  arm <- arm %>% add(Bolus(time=0, amount=100))
  arm <- arm %>% add(Bolus(time=24, amount=100))
  arm <- arm %>% add(Bolus(time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(Covariate("WT", BootstrapDistribution(data=c(65, 70, 75), random=TRUE, replacement=TRUE)))
  arm <- arm %>% add(Covariate("HT", BootstrapDistribution(data=c(175, 180, 185), random=TRUE, replacement=TRUE)))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(Observation(time=t))
  }
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE
  set.seed(1)
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, data.frame(ID=c(1,2,3,4,5,6,7,8), WT=c(65,75,65,70,65,75,75,70), HT=c(180,185,185,175,175,175,180,180)))
})

