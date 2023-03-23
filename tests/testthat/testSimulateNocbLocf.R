library(testthat)

context("Test the simulate method with the NOCB/LOCF switch")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Weight as a time-varying covariate, NOCB vs LOCF"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$nonmem$advan4_trans4
  equation <- model %>% find(Equation("CL"))
  model <- model %>% replace(Equation("CL", paste0(equation@rhs, "*pow(BW/70, 0.75)")))
  
  dataset <- Dataset(4) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Observations(times=c(0,3,9,16,24,26,48)))

  # Left join time-varying BW column
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed) # CAREFUL, SEED NEEDED FOR REPRODUCIBILITY
  weight <- data.frame(TIME=c(0,3,9,16,24,26,48), BW=c(150,20,100,250,40,170,10))
  table <- table %>% dplyr::left_join(weight, by="TIME")
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename="wt_as_time_varying_cov")
  
  # LOCF tests
  regFilename <- "wt_as_time_varying_cov_locf"
  simulation <- expression(simulate(model=model, dataset=table, dest=destEngine, settings=Settings(NOCB(FALSE), Declare("BW")), seed=seed))
  # Note: argument declare is only needed for mrgsolve
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
  
  # NOCB tests
  regFilename <- "wt_as_time_varying_cov_nocb"
  simulation <- expression(simulate(model=model, dataset=table, dest=destEngine, settings=Settings(NOCB(TRUE), Declare("BW")), seed=seed))
  # Note: argument declare is only needed for mrgsolve
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})


test_that(getTestName("NOCB/LOCF should not have any effect on treatment occasion"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$nonmem$advan4_trans4 %>%
    delete(Equation("KA")) %>%
    add(Equation("KA", "0")) %>%
    add(IfStatement("OCC==1", Equation("KA", "THETA_KA*1.5*exp(ETA_KA)"))) %>%
    add(IfStatement("OCC==2", Equation("KA", "THETA_KA*0.5*exp(ETA_KA)"))) %>% 
    add(IfStatement("OCC==3", Equation("KA", "THETA_KA*0.1*exp(ETA_KA)")))
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=12, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Occasion("OCC", values=c(1,2,3), doseNumbers=c(1,2,3))) %>%
    add(Observations(times=seq(24, 36)))
  
  regFilename <- "occ_as_time_varying_cov"
  # table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
  # table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  # LOCF tests
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, settings=Settings(NOCB(FALSE, "OCC")), seed=seed, outvars="KA"))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
  
  # NOCB tests
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, settings=Settings(NOCB(TRUE, "OCC")), seed=seed, outvars="KA"))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("NOCB/LOCF should not have any effect on IOV, e.g. on clearance"), {
  if (skipLongTest) return(TRUE)
  regFilename <- "3_boluses_iov_cl"
  model <- model_suite$nonmem$advan4_trans4
  model <- model %>% replace(Equation("CL", rhs="THETA_CL*exp(ETA_CL + IOV_CL)"))
  
  for(startTime in c(0, 20, 23, 24, 48)) {
    obsTimes <- seq(startTime,72, by=5)

    dataset <- Dataset(3) %>%
      add(Bolus(time=0, amount=1000, compartment=1)) %>%
      add(Bolus(time=24, amount=1000, compartment=1)) %>%
      add(Bolus(time=48, amount=1000, compartment=1)) %>%
      add(Observations(times=obsTimes)) %>% # BEFORE by 0.5, NOW by 5  
      add(IOV(colname="IOV_CL", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=1))))
    
    # table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
    # table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
    
    # LOCF tests
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, settings=Settings(NOCB(FALSE, "IOV_CL")), seed=seed))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
    
    # NOCB tests
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, settings=Settings(NOCB(TRUE, "IOV_CL")), seed=seed))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
  }
  
})
