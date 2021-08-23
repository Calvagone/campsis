library(testthat)
library(campsismod)

context("Test all methods from the dataset class")

test_that("Add entry, order, filter, getTimes (simple example)", {
  
  dataset <- Dataset() 
  
  # Add doses
  dataset <- dataset %>% add(Bolus(time=0, amount=100))
  dataset <- dataset %>% add(Bolus(time=24, amount=100))
  dataset <- dataset %>% add(Bolus(time=48, amount=100))

  # Add observations
  dataset <- dataset %>% add(Observations(times=seq(0, 48, by=4)))
  
  # Get times
  expect_equal(dataset %>% getTimes(), seq(0, 48, by=4))
  
  # Export to RxODE
  table1 <- dataset %>% export(dest="RxODE")
  expect_equal(nrow(table1), 16)
  expect_true(is(table1, "tbl_df"))
  
  # Export to mrgsolve
  table2 <- dataset %>% export(dest="mrgsolve")
  expect_equal(nrow(table2), 16)
  expect_true(is(table2, "tbl_df"))
})

test_that("Two arms example", {
  
  # Create 2 arms
  arm1 <- Arm(id=1, subjects=4)
  arm2 <- Arm(id=2, subjects=3)

  # Add doses in respective arms
  arm1 <- arm1 %>% add(Bolus(time=0, amount=100))
  arm2 <- arm2 %>% add(Bolus(time=0, amount=200))
    
  # Add observations
  obs <- Observations(times=seq(0, 48, by=4))
  arm1 <- arm1 %>% add(obs)
  arm2 <- arm2 %>% add(obs)
  
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
  
  # Replace numbers of subjects in second arm
  arm2Bis <- dataset@arms %>% getByIndex(2)
  arm2Bis@subjects <- as.integer(5)
  dataset <- dataset %>% replace(arm2Bis)
  
  # Total number of subjects
  expect_equal(dataset %>% length(), 9)
})

test_that("Export using config", {
  
  dataset <- Dataset() 
  
  # Add doses
  dataset <- dataset %>% add(Bolus(time=0, amount=100))
  dataset <- dataset %>% add(Bolus(time=24, amount=100))
  dataset <- dataset %>% add(Bolus(time=48, amount=100))
  
  
  # Add observations
  dataset <- dataset %>% add(Observations(times=seq(0, 48, by=10)))
  
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
  dataset <- dataset %>% add(Covariate(name="WT", 70))
  dataset <- dataset %>% add(Covariate(name="HT", 180))
  dataset <- dataset %>% add(TimeVaryingCovariate(name="DOSE", 100))
  
  expect_equal(dataset %>% getCovariateNames(), c("WT", "HT", "DOSE"))
  expect_equal(dataset %>% getTimeVaryingCovariateNames(), c("DOSE"))
  
  # Add observations
  dataset <- dataset %>% add(Observations(times=seq(0, 48, by=10)))
  
  # Export to RxODE N=1
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  expect_true(all(table$DOSE==100)) # Even if covariate can be adapted by events
  
  # Export to RxODE N=2
  arm <- dataset@arms %>% default()
  arm@subjects <- as.integer(2)
  dataset@arms <- dataset@arms %>% campsismod::replace(arm)
  
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  expect_true(all(table$DOSE==100)) # Even if covariate can be adapted by events
  
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
  arm <- arm %>% add(Observations(times=seq(0, 48, by=10)))
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE")
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, tibble::tibble(ID=c(1,2,3), WT=c(65,70,75), HT=c(175,180,185)))
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
  arm <- arm %>% add(Observations(times=seq(0, 48, by=10)))
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE", seed=1)
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct() %>% dplyr::mutate(WT=round(WT), HT=round(HT))
  expect_equal(subTable, tibble::tibble(ID=c(1,2,3), WT=c(64,72,62), HT=c(212,187,164)))
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
  arm <- arm %>% add(Observations(times=seq(0, 48, by=10)))
  
  dataset <- Dataset()
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE
  config <- DatasetConfig(defObsCmt=2)
  dataset <- dataset %>% add(config)
  table <- dataset %>% export(dest="RxODE", seed=1)
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, tibble::tibble(ID=c(1,2,3,4,5,6,7,8), WT=c(65,75,65,70,65,75,75,70), HT=c(180,185,185,175,175,175,180,180)))
})

test_that("Export occasions works well - example 1", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 60, by=10)))
  
  # Add occasions
  ds <- ds %>% add(Occasion("MY_OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))

  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # All OCC values are used because 3 doses
  expect_equal(table$MY_OCC, rep(c(1,1,1,1,2,2,2,3,3,3), 2))
})

test_that("Export occasions works well - example 2", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 60, by=10)))
  
  # Add occasions
  ds <- ds %>% add(Occasion("MY_OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # Check value 3 is not used (no 3rd dose)
  expect_equal(table$MY_OCC, rep(c(1,1,1,1,2,2,2,2,2), 2))
})


test_that("Export occasions works well - example 3", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  ds <- ds %>% add(Bolus(time=72, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 80, by=10)))
  
  # Add occasions (skip occasion on dose 3)
  ds <- ds %>% add(Occasion("MY_OCC", values=c(1,2,4), doseNumbers=c(1,2,4)))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # All OCC values are used because 3 doses
  expect_equal(table$MY_OCC, rep(c(1,1,1,1,2,2,2,2,2,2,2,4,4), 2))
})

test_that("Export occasions works well - example 4", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  ds <- ds %>% add(Bolus(time=72, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 80, by=10)))
  
  # Add occasions (skip occasion on dose 3)
  ds <- ds %>% add(Occasion("MY_OCC", values=c(2,3,4), doseNumbers=c(2,3,4)))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # Is this the expected behaviour ? This is arbitrary, for sure
  expect_equal(table$MY_OCC, rep(c(0,0,0,0,2,2,2,3,3,3,3,4,4), 2))
})


test_that("Occasion can be added into arms", {

  addProtocol <- function(x) {
    # Add doses
    x <- x %>% add(Bolus(time=0, amount=100))
    x <- x %>% add(Bolus(time=24, amount=100))
    x <- x %>% add(Bolus(time=48, amount=100))
    
    # Add observations
    x <- x %>% add(Observations(times=seq(0, 60, by=10)))
    
    # Add occasions
    x <- x %>% add(Occasion("MY_OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  }
  
  arm1 <- Arm(id=1, subjects=1) %>% addProtocol()
  arm2 <- Arm(id=2, subjects=1) %>% addProtocol()
  ds <- Dataset() %>% add(c(arm1, arm2))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # All OCC values are used because 3 doses
  expect_equal(table$MY_OCC, rep(c(1,1,1,1,2,2,2,3,3,3), 2))
})

test_that("Export IOV works well - example 1", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  ds <- ds %>% add(Bolus(time=72, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 80, by=10)))
  
  # Add occasions (skip occasion on dose 3)
  ds <- ds %>% add(IOV("IOV_KA", distribution=NormalDistribution(0, sd=1), doseNumbers=c(3,4)))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # Arbitrary but OK
  expect_equal(round(table$IOV_KA,2), c(0,0,0,0,0,0,0,-0.63,-0.63,-0.63,-0.63,0.18,0.18,0,0,0,0,0,0,0,-0.84,-0.84,-0.84,-0.84,1.60,1.60))
})

test_that("Export IOV works well - example 2", {
  
  ds <- Dataset(2)
  
  # Add doses
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  ds <- ds %>% add(Bolus(time=72, amount=100))
  
  # Add observations
  ds <- ds %>% add(Observations(times=seq(0, 80, by=10)))
  
  # Add occasions (skip occasion on dose 3)
  ds <- ds %>% add(IOV("IOV_KA", distribution=NormalDistribution(0, sd=1), doseNumbers=c(1,3)))
  
  # Export to RxODE
  table <- ds %>% export(dest="RxODE", seed=1)
  
  # Arbitrary but OK
  expect_equal(round(table$IOV_KA,2), c(-0.63,-0.63,-0.63,-0.63,-0.63,-0.63,-0.63,0.18,0.18,0.18,0.18,0.18,0.18,-0.84,-0.84,-0.84,-0.84,-0.84,-0.84,-0.84,1.60,1.60,1.60,1.60,1.60,1.60))
})

