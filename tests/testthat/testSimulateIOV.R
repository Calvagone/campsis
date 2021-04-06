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
  pk <- model@model %>% getByName("PK")
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
  pk <- model@model %>% getByName("PK")
  pk@code[[1]] <- "KA=THETA_KA*exp(ETA_KA + IOV_KA)"
  model@model <- model@model %>% pmxmod::replace(pk)
  model@parameters <- model@parameters %>% add(Omega("IOV_KA", index=6, index2=6, value=0.2^2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=EtaDistribution(omega="IOV_KA")))
  
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
  # Model with IIV on F1
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("PK")
  model@parameters <- model@parameters %>% add(Theta("F1", index=6, value=0.75))
  model@parameters <- model@parameters %>% add(Omega("F1", index=6, index2=6, value=0.2^2))

  # Model with IIV and IOV on F1
  model_iov <- model
  iovCvPc <- 20 # 20% CV
  
  # Add IOV
  model@parameters <- model@parameters %>% add(Omega("IOV_F1", index=7, index2=7, value=(0/100)^2))
  model_iov@parameters <- model_iov@parameters %>% add(Omega("IOV_F1", index=7, index2=7, value=(iovCvPc/100)^2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_F1_COL", distribution=EtaDistribution(omega="IOV_F1")))
  
  # Add bioavailability
  dataset <- dataset %>% add(TreatmentBioavailability(compartment=1, ParameterDistribution(theta="F1", omega="F1", iov="IOV_F1_COL")))

  # Simulate just IIV
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_f1")
  
  # Simulate just IIV + IOV
  results2 <- model_iov %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model_iov, seed=seed, filename="3_boluses_iiv_iov_f1")
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on ALAG1", {
  # Model with IIV on ALAG1
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("PK")
  model@parameters <- model@parameters %>% add(Theta("ALAG1", index=6, value=5))
  model@parameters <- model@parameters %>% add(Omega("ALAG1", index=6, index2=6, value=0.2^2))
  
  # Model with IIV and IOV on ALAG1
  model_iov <- model
  iovCvPc <- 20 # 20% CV
  
  # Add IOV
  model@parameters <- model@parameters %>% add(Omega("IOV_ALAG1", index=7, index2=7, value=(0/100)^2))
  model_iov@parameters <- model_iov@parameters %>% add(Omega("IOV_ALAG1", index=7, index2=7, value=(iovCvPc/100)^2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_ALAG1_COL", distribution=EtaDistribution(omega="IOV_ALAG1")))
  
  # Add bioavailability
  dataset <- dataset %>% add(TreatmentLagTime(compartment=1, ParameterDistribution(theta="ALAG1", omega="ALAG1", iov="IOV_ALAG1_COL")))
  
  # Simulate just IIV
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_alag1")
  
  # Simulate just IIV + IOV
  results2 <- model_iov %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model_iov, seed=seed, filename="3_boluses_iiv_iov_alag1")
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})

test_that("Simulate IOV on D1", {
  # Model with IIV on D1
  model <- getNONMEMModelTemplate(3,4)
  pk <- model@model %>% getByName("PK")
  model@parameters <- model@parameters %>% add(Theta("D1", index=5, value=5))
  model@parameters <- model@parameters %>% add(Omega("D1", index=5, index2=5, value=0.2^2))
  
  # Model with IIV and IOV on D1
  model_iov <- model
  iovCvPc <- 50 # 50% CV
  
  # Add IOV
  model@parameters <- model@parameters %>% add(Omega("IOV_D1", index=6, index2=6, value=(0/100)^2))
  model_iov@parameters <- model_iov@parameters %>% add(Omega("IOV_D1", index=6, index2=6, value=(iovCvPc/100)^2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Infusion(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Infusion(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_D1_COL", distribution=EtaDistribution(omega="IOV_D1")))
  
  # Add infusion duration
  dataset <- dataset %>% add(TreatmentInfusionDuration(compartment=1, ParameterDistribution(theta="D1", omega="D1", iov="IOV_D1_COL")))
  
  # Simulate just IIV
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  datasetRegressionTest(dataset, model, seed=seed, filename="3_infusions_iiv_d1")
  
  # Simulate just IIV + IOV
  results2 <- model_iov %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  datasetRegressionTest(dataset, model_iov, seed=seed, filename="3_infusions_iiv_iov_d1")
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})
