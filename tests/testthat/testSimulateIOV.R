library(testthat)
library(pmxmod)

context("Test the simulate method with IOV")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate 1000mg QD with IOV on KA (1)", {
  regFilename <- "3_boluses_iov_ka_1"
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("KA", rhs="THETA_KA*exp(ETA_KA + IOV_KA)")
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=0.2))))
  
  expect_equal(dataset %>% getIOVNames(), "IOV_KA")
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 145*dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), 145*dataset %>% length())
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate 1000mg QD with IOV on KA (2)", {
  regFilename <- "3_boluses_iov_ka_2"
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("KA", rhs="THETA_KA*exp(ETA_KA + IOV_KA)")
  model <- model %>% add(Omega("IOV_KA", value=0.2^2))

  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=EtaDistribution(model, omega="IOV_KA")))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")
  expect_equal(nrow(results1), 145*dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaghettiPlot(results2, "CP")
  expect_equal(nrow(results2), 145*dataset %>% length())
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate IOV on F1", {
  # Model with IIV and IOV on F1
  model <- model_library$advan4_trans4
  model <- model %>% add(Theta("F1", value=0.75))
  model <- model %>% add(Omega("F1", value=0.2^2))
  model <- model %>% add(Omega("IOV_F1", index=7, index2=7, value=0.2^2, same=FALSE)) # 20% IOV
  model <- model %>% add(Bioavailability(compartment=1, rhs="F1"))
  model <- model %>% addEquation("F1", rhs="THETA_F1*exp(ETA_F1 + IOV_F1)", after="Q")
  
  getDataset <- function(model) {
    dataset <- Dataset(10)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
    dataset <- dataset %>% add(IOV(colname="IOV_F1", distribution=EtaDistribution(model, omega="IOV_F1")))
    return(dataset)
  }
  
  # Simulate just IIV
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  results1 <- model_no_iov %>% simulate(dataset_no_iov, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_boluses_iiv_f1")
  
  # Simulate just IIV + IOV
  dataset <- getDataset(model)
  results2 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_iov_f1")
  
  spaghettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on ALAG1", {
  # Model with IIV on ALAG1
  model <- model_library$advan4_trans4
  model <- model %>% add(Theta("ALAG1", value=5))
  model <- model %>% add(Omega("ALAG1", value=0.2^2))
  model <- model %>% add(Omega("IOV_ALAG1", value=0.2^2, same=FALSE)) # 20% IOV
  model <- model %>% add(LagTime(compartment=1, rhs="ALAG1"))
  model <- model %>% addEquation("ALAG1", rhs="THETA_ALAG1*exp(ETA_ALAG1 + IOV_ALAG1)")
  
  getDataset <- function(model) {
    dataset <- Dataset(10)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
    dataset <- dataset %>% add(IOV(colname="IOV_ALAG1", distribution=EtaDistribution(model, omega="IOV_ALAG1")))
    return(dataset)
  }

  # Simulate just IIV
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  results1 <- model_no_iov %>% simulate(dataset_no_iov, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_boluses_iiv_alag1")
  
  # Simulate just IIV + IOV
  dataset <- getDataset(model)
  results2 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_iov_alag1")
  
  spaghettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on D1", {
  # Model with IIV on D1
  model <- model_library$advan3_trans4
  model <- model %>% add(Theta("D1", value=5))
  model <- model %>% add(Omega("D1", value=0.2^2))
  model <- model %>% add(Omega("IOV_D1", value=0.5^2, same=FALSE)) # 50% IOV
  model <- model %>% add(InfusionDuration(compartment=1, rhs="D1"))
  model <- model %>% addEquation("D1", rhs="THETA_D1*exp(ETA_D1 + IOV_D1)")
  
  getDataset <- function(model) {
    dataset <- Dataset(10)
    dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Infusion(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Infusion(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
    dataset <- dataset %>% add(IOV(colname="IOV_D1", distribution=EtaDistribution(model, omega="IOV_D1")))
    return(dataset)
  }
  
  # Simulate just IIV
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  results1 <- model_no_iov %>% simulate(dataset_no_iov, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_infusions_iiv_d1")
  
  # Simulate just IIV + IOV
  dataset <- getDataset(model)
  results2 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_infusions_iiv_iov_d1")
  
  spaghettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})
