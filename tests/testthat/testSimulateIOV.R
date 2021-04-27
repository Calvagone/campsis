library(testthat)
library(pmxmod)

context("Test the simulate method with IOV")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate 1000mg QD with IOV on KA (1)", {
  regFilename <- "3_boluses_iov_ka_1"
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("MAIN")
  pk@code[[1]] <- "KA=THETA_KA*exp(ETA_KA + IOV_KA)"
  model@model <- model@model %>% pmxmod::replace(pk)
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=0.2))))
  
  expect_equal(dataset %>% getIOVNames(), "IOV_KA")
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 145*dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 145*dataset %>% length())
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate 1000mg QD with IOV on KA (2)", {
  regFilename <- "3_boluses_iov_ka_2"
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("MAIN")
  pk@code[[1]] <- "KA=THETA_KA*exp(ETA_KA + IOV_KA)"
  model@model <- model@model %>% pmxmod::replace(pk)
  model@parameters <- model@parameters %>% add(Omega("IOV_KA", index=6, index2=6, value=0.2^2))

  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=EtaDistribution(model, omega="IOV_KA")))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  expect_equal(nrow(results1), 145*dataset %>% length())
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  expect_equal(nrow(results2), 145*dataset %>% length())
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Simulate IOV on F1", {
  # Model with IIV and IOV on F1
  model <- getNONMEMModelTemplate(4,4)
  model@parameters <- model@parameters %>% add(Theta("F1", index=6, value=0.75))
  model@parameters <- model@parameters %>% add(Omega("F1", index=6, index2=6, value=0.2^2))
  model@parameters <- model@parameters %>% add(Omega("IOV_F1", index=7, index2=7, value=0.2^2, same=FALSE)) # 20% IOV
  model <- model %>% add(Bioavailability(compartment=1, rhs="F1"))
  
  pk <- model@model %>% getByName("MAIN")
  pk@code <- pk@code %>% append("F1=THETA_F1*exp(ETA_F1 + IOV_F1)")
  model@model <- model@model %>% pmxmod::replace(pk)
  
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
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on ALAG1", {
  # Model with IIV on ALAG1
  model <- getNONMEMModelTemplate(4,4)
  model@parameters <- model@parameters %>% add(Theta("ALAG1", index=6, value=5))
  model@parameters <- model@parameters %>% add(Omega("ALAG1", index=6, index2=6, value=0.2^2))
  model@parameters <- model@parameters %>% add(Omega("IOV_ALAG1", index=7, index2=7, value=0.2^2, same=FALSE)) # 20% IOV
  model <- model %>% add(LagTime(compartment=1, rhs="ALAG1"))
  
  pk <- model@model %>% getByName("MAIN")
  pk@code <- pk@code %>% append("ALAG1=THETA_ALAG1*exp(ETA_ALAG1 + IOV_ALAG1)")
  model@model <- model@model %>% pmxmod::replace(pk)
  
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
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on D1", {
  # Model with IIV on D1
  model <- getNONMEMModelTemplate(3,4)
  model@parameters <- model@parameters %>% add(Theta("D1", index=5, value=5))
  model@parameters <- model@parameters %>% add(Omega("D1", index=5, index2=5, value=0.2^2))
  model@parameters <- model@parameters %>% add(Omega("IOV_D1", index=6, index2=6, value=0.5^2, same=FALSE)) # 50% IOV
  model <- model %>% add(InfusionDuration(compartment=1, rhs="D1"))

  pk <- model@model %>% getByName("MAIN")
  pk@code <- pk@code %>% append("D1=THETA_D1*exp(ETA_D1 + IOV_D1)")
  model@model <- model@model %>% pmxmod::replace(pk)
  
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
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})
