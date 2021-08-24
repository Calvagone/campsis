library(testthat)

context("Test the simulate method with the NOCB/LOCF switch")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Weight as a time-varying covariate (NOCB vs LOCF)", {
  model <- model_library$advan4_trans4
  model <- model %>% replace(Equation("CL", paste0((model %>% getEquation("CL"))@rhs, "*pow(BW/70, 0.75)")))
  
  dataset <- Dataset(4)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=c(0,3,9,16,24,26,48)))

  # Left join time-varying BW column
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed) # CAREFUL, SEED NEEDED FOR REPRODUCIBILITY
  weight <- data.frame(TIME=c(0,3,9,16,24,26,48), BW=c(150,20,100,250,40,170,10))
  table <- table %>% dplyr::left_join(weight, by="TIME")
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename="wt_as_time_varying_cov")
  
  # LOCF tests
  regFilename <- "wt_as_time_varying_cov_locf"
  
  results1 <- model %>% simulate(table, dest="RxODE", seed=seed, nocb=FALSE)
  spaghettiPlot(results1, "CP")

  results2 <- model %>% simulate(table, dest="mrgsolve", seed=seed, declare="BW", nocb=FALSE) # NOCB TRUE by default
  spaghettiPlot(results2, "CP")

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # NOCB tests
  regFilename <- "wt_as_time_varying_cov_nocb"
  
  results1 <- model %>% simulate(table, dest="RxODE", seed=seed, nocb=TRUE)
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(table, dest="mrgsolve", seed=seed, declare="BW", nocb=TRUE)
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})


test_that("NOCB/LOCF should not have any effect on treatment occasion", {
  model <- model_library$advan4_trans4
  model <- model %>% removeEquation("KA")
  model <- model %>% add(Equation("KA", "0"))
  model <- model %>% add(IfStatement("OCC==1", Equation("KA", "THETA_KA*1.5*exp(ETA_KA)")))
  model <- model %>% add(IfStatement("OCC==2", Equation("KA", "THETA_KA*0.5*exp(ETA_KA)")))
  model <- model %>% add(IfStatement("OCC==3", Equation("KA", "THETA_KA*0.1*exp(ETA_KA)")))
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=12, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Occasion("OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  dataset <- dataset %>% add(Observations(times=seq(24, 36)))
  
  regFilename <- "occ_as_time_varying_cov"
  # table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
  # table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  # LOCF tests
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars="KA", nocbvars="OCC")
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars="KA", nocbvars="OCC")
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # NOCB tests
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars="KA", nocb=TRUE, nocbvars="OCC")
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars="KA", nocb=TRUE, nocbvars="OCC")
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("NOCB/LOCF should not have any effect on IOV (e.g. on clearance)", {
  regFilename <- "3_boluses_iov_cl"
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("CL", rhs="THETA_CL*exp(ETA_CL + IOV_CL)")
  
  for(startTime in c(0, 20, 23, 24, 48)) {
    obsTimes <- seq(startTime,72, by=5)

    dataset <- Dataset(3)
    dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
    dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
    dataset <- dataset %>% add(Observations(times=obsTimes)) # BEFORE by 0.5, NOW by 5
    dataset <- dataset %>% add(IOV(colname="IOV_CL", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=1))))
    
    table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
    table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
    
    results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocbvars="IOV_CL")
    spaghettiPlot(results1a, "CP")
    
    results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, nocb=TRUE, nocbvars="IOV_CL")
    spaghettiPlot(results1b, "CP")
    
    results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocbvars="IOV_CL")
    spaghettiPlot(results2a, "CP")
    
    results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, nocb=TRUE, nocbvars="IOV_CL")
    spaghettiPlot(results2b, "CP")
    
    outputRegressionTest(results1a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results1b, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2a, output="CP", filename=regFilename, times=obsTimes)
    outputRegressionTest(results2b, output="CP", filename=regFilename, times=obsTimes)
  }
  
})