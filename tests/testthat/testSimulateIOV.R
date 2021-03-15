library(testthat)
library(pmxmod)

context("Test the simulate method with IOV")

overwriteNonRegressionFiles <<- TRUE
testFolder <<- "C:/prj/pmxsim/tests/testthat/"
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate 1000mg QD with IOV on KA (1)", {
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("PK")
  pk@code[[1]] <- "KA=THETA_1*exp(ETA_1 + IOV_KA)"
  model@model <- model@model %>% pmxmod::replace(pk)
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=0.2))))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 145*dataset %>% length())
  regressionTest(dataset, model, seed=seed, filename="3_boluses_iov_ka_1.csv")
})

test_that("Simulate 1000mg QD with IOV on KA (2)", {
  model <- getNONMEMModelTemplate(4,4)
  pk <- model@model %>% getByName("PK")
  pk@code[[1]] <- "KA=THETA_1*exp(ETA_1 + IOV_KA)"
  model@model <- model@model %>% pmxmod::replace(pk)
  model@parameters <- model@parameters %>% add(Omega("IOV_KA", index=6, index2=6, value=0.2^2))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=EtaDistribution(omega="IOV_KA")))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results, "CP")
  
  expect_equal(nrow(results), 145*dataset %>% length())
  regressionTest(dataset, model, seed=seed, filename="3_boluses_iov_ka_2.csv")
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
  dataset <- dataset %>% add(Bioavailability(compartment=1, ParameterDistribution(theta="F1", omega="F1", iov="IOV_F1_COL")))

  # Simulate just IIV
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  regressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_f1.csv")
  
  # Simulate just IIV + IOV
  results2 <- model_iov %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  regressionTest(dataset, model_iov, seed=seed, filename="3_boluses_iiv_iov_f1.csv")
  
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
  dataset <- dataset %>% add(LagTime(compartment=1, ParameterDistribution(theta="ALAG1", omega="ALAG1", iov="IOV_ALAG1_COL")))
  
  # Simulate just IIV
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1$ARM <- "IIV"
  regressionTest(dataset, model, seed=seed, filename="3_boluses_iiv_alag1.csv")
  
  # Simulate just IIV + IOV
  results2 <- model_iov %>% simulate(dataset, dest="RxODE", seed=seed)
  results2$id <- results2$id + dataset %>% length()
  results2$ARM <- "IIV + IOV"
  regressionTest(dataset, model_iov, seed=seed, filename="3_boluses_iiv_iov_alag1.csv")
  
  spaguettiPlot(rbind(results1, results2), "CP", "ARM")
  shadedPlot(rbind(results1, results2), "CP", "ARM")
})
