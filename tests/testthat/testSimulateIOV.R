library(testthat)
library(campsismod)

context("Test the simulate method with IOV")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate 1000mg QD with IOV on KA (1)", {
  regFilename <- "3_boluses_iov_ka_1"
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("KA", rhs="THETA_KA*exp(ETA_KA + IOV_KA)")
  obsTimes1 <- c(15, 50, 55, 60, 65, 70) # by 0.5 in the non-regression file
  obsTimes2 <- seq(0, 72, by=0.5)
  
  for (obsTimes in list(obsTimes1, obsTimes2)) {
    dataset <- Dataset(10)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=obsTimes))
    dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=0.2))))
    
    expect_equal(dataset %>% getIOVNames(), "IOV_KA")
    
    #table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed, nocb=TRUE)
    #table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed, nocb=TRUE)

    results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocbvars="IOV_KA")
    results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE, nocbvars="IOV_KA")
    results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocbvars="IOV_KA")
    results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE, nocbvars="IOV_KA")
    
    outputRegressionTest(results1a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results1b, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2b, output="CP", filename=regFilename, times=obsTimes)
    
    spaghettiPlot(results1a, "CP")
    spaghettiPlot(results1b, "CP")
    spaghettiPlot(results2a, "CP")
    spaghettiPlot(results2b, "CP")
  }
  
})

test_that("Simulate 1000mg QD with IOV on KA (2) (this test sometimes fails with RxODE version > 1.0.5)", {
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
  
  results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocbvars="IOV_KA")
  results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE, nocbvars="IOV_KA")
  results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocbvars="IOV_KA")
  results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE, nocbvars="IOV_KA")
  
  outputRegressionTest(results1a, output="CP", filename=regFilename)
  outputRegressionTest(results1b, output="CP", filename=regFilename)
  outputRegressionTest(results2a, output="CP", filename=regFilename)
  outputRegressionTest(results2b, output="CP", filename=regFilename)
  
  spaghettiPlot(results1a, "CP")
  spaghettiPlot(results1b, "CP")
  spaghettiPlot(results2a, "CP")
  spaghettiPlot(results2b, "CP")
})

test_that("Simulate IOV on F1 (this test always fails with RxODE version > 1.0.5)", {
  regFilename <- "3_boluses_iiv_iov_f1"
  
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
    dataset <- dataset %>% add(Observations(times=seq(0,72, by=4)))
    dataset <- dataset %>% add(IOV(colname="IOV_F1", distribution=EtaDistribution(model, omega="IOV_F1")))
    return(dataset)
  }
  
  # IIV only
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_boluses_iiv_f1")
  
  # IIV + IOV (RxODE / mrgsolve)
  dataset <- getDataset(model)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE)
  results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE)

  outputRegressionTest(results1a, output="CP", filename=regFilename)
  outputRegressionTest(results1b, output="CP", filename=regFilename)
  outputRegressionTest(results2a, output="CP", filename=regFilename)
  outputRegressionTest(results2b, output="CP", filename=regFilename)
  
  spaghettiPlot(results1a, "CP")
  spaghettiPlot(results1b, "CP")
  spaghettiPlot(results2a, "CP")
  spaghettiPlot(results2b, "CP")
})


test_that("Simulate IOV on ALAG1 (this test always fails with RxODE version > 1.0.5)", {
  regFilename <- "3_boluses_iiv_iov_alag1"
  
  # Model with IIV on ALAG1
  model <- model_library$advan4_trans4
  model <- model %>% add(Theta("ALAG1", value=5))
  model <- model %>% add(Omega("ALAG1", value=0.2^2))
  model <- model %>% add(Omega("IOV_ALAG1", value=0.2^2, same=FALSE)) # 20% IOV
  model <- model %>% add(LagTime(compartment=1, rhs="ALAG1"))
  model <- model %>% addEquation("ALAG1", rhs="THETA_ALAG1*exp(ETA_ALAG1 + IOV_ALAG1)")
  
  obsTimes <- seq(0,72, by=0.5)
  getDataset <- function(model, times) {
    dataset <- Dataset(10)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=times))
    dataset <- dataset %>% add(IOV(colname="IOV_ALAG1", distribution=EtaDistribution(model, omega="IOV_ALAG1")))
    return(dataset)
  }

  # IIV only
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov, obsTimes)
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_boluses_iiv_alag1")
  
  # IIV + IOV (RxODE / mrgsolve)
  dataset <- getDataset(model, obsTimes)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  startTimes <- c(0,24,47) # 47,48 NOT WORKING WITH MRGSOLVE LOCF
  for (startTime in startTimes) {
    cat(startTime)
    obsTimes <- seq(startTime, 72, by=1)
    dataset <- getDataset(model, obsTimes)
  
    results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed)
    results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE)
    results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
    results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE)
    
    table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed, nocb=FALSE)
    table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed, nocb=FALSE)
    
    outputRegressionTest(results1a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results1b, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2b, output="CP", filename=regFilename, times=obsTimes)
    
    spaghettiPlot(results1a, "CP")
    spaghettiPlot(results1b, "CP")
    spaghettiPlot(results2a, "CP")
    spaghettiPlot(results2b, "CP")
  }
})

test_that("Simulate IOV on D1", {
  regFilename <- "3_infusions_iiv_iov_d1"
  
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
  
  # IIV only
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_infusions_iiv_d1")
  
  # IIV + IOV (RxODE / mrgsolve)
  dataset <- getDataset(model)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE)
  results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE)
  
  outputRegressionTest(results1a, output="CP", filename=regFilename)
  outputRegressionTest(results1b, output="CP", filename=regFilename)
  outputRegressionTest(results2a, output="CP", filename=regFilename)
  outputRegressionTest(results2b, output="CP", filename=regFilename)
  
  spaghettiPlot(results1a, "CP")
  spaghettiPlot(results1b, "CP")
  spaghettiPlot(results2a, "CP")
  spaghettiPlot(results2b, "CP")
})

test_that("Simulate IOV on F1", {
  regFilename <- "3_bolus_iov_on_f1"
  
  # Model with IIV on D1
  model <- model_library$advan4_trans4
  model <- model %>% add(Theta("F1", value=0.75))
  model <- model %>% add(Omega("F1", value=0.09)) # 30% CV
  model <- model %>% addEquation("F1", rhs="THETA_F1*exp(ETA_F1 + IOV_F1)")
  model <- model %>% add(Bioavailability(1, rhs="F1"))
  
  startTimes <- c(0,24,47) # 47,48 NOT WORKING WITH MRGSOLVE LOCF
  for (startTime in startTimes) {
    cat(startTime)
    obsTimes <- seq(startTime, 72, by=1)
    dataset <- Dataset(3)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=obsTimes))
    dataset <- dataset %>% add(IOV(colname="IOV_F1", NormalDistribution(0, 0.5))) # 50% CV in addition
    
    table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
    table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
    
    results1a <- model %>% simulate(dataset, dest="RxODE", seed=1, outvars="F1")
    results1b <- model %>% simulate(dataset, dest="RxODE", seed=1, outvars="F1", nocb=TRUE)
    results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=1, outvars="F1")
    results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=1, outvars="F1", nocb=TRUE)
    
    outputRegressionTest(results1a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results1b, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2b, output="CP", filename=regFilename, times=obsTimes)
    
    spaghettiPlot(results1a, "CP")
    spaghettiPlot(results2a, "CP")
  }
  
})
