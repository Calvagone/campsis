library(testthat)

context("Test the simulate method with IOV")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate 1000mg QD with IOV on KA (1)"), {
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_boluses_iov_ka_1"
  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% replace(Equation("KA", "THETA_KA*exp(ETA_KA + IOV_KA)"))
  obsTimes1 <- c(15, 50, 55, 60, 65, 70) # by 0.5 in the non-regression file
  obsTimes2 <- seq(0, 72, by=0.5)
  
  for (obsTimes in list(obsTimes1, obsTimes2)) {
    dataset <- Dataset(10) %>%
      add(Bolus(time=0, amount=1000, compartment=1)) %>%
      add(Bolus(time=24, amount=1000, compartment=1)) %>%
      add(Bolus(time=48, amount=1000, compartment=1)) %>%
      add(Observations(times=obsTimes)) %>%
      add(IOV(colname="IOV_KA", distribution=FunctionDistribution(fun="rnorm", args=list(mean=0, sd=0.2))))
    
    expect_equal(dataset %>% getIOVs() %>% getNames(), "IOV_KA")
    
    #table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed, nocb=TRUE)
    #table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed, nocb=TRUE)
    
    # NOCB=FALSE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE, "IOV_KA"))))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())

    # NOCB=TRUE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE, "IOV_KA"))))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
  }
})

test_that(getTestName("Simulate 1000mg QD with IOV on KA (2)"), {
  # This test could sometimes fail with RxODE version > 1.0.5 & < 1.1.0
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_boluses_iov_ka_2"
  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% replace(Equation("KA", "THETA_KA*exp(ETA_KA + IOV_KA)"))
  model <- model %>% add(Omega("IOV_KA", value=0.2^2))

  dataset <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Bolus(time=24, amount=1000, compartment=1)) %>%
    add(Bolus(time=48, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,72, by=0.5))) %>%
    add(IOV(colname="IOV_KA", distribution=EtaDistribution(model, omega="IOV_KA")))

  # NOCB=FALSE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE, "IOV_KA"))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
  
  # NOCB=TRUE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE, "IOV_KA"))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate IOV on F1"), {
  # This test always failed with RxODE version > 1.0.5 & < 1.1.0
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_boluses_iiv_iov_f1"

  # Model with IIV and IOV on F1
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    add(Theta("F1", value=0.75)) %>%
    add(Omega("F1", value=0.2^2)) %>%
    add(Omega("IOV_F1", index=7, index2=7, value=0.2^2, same=FALSE)) %>% # 20% IOV
    add(Bioavailability(compartment=1, rhs="F1")) %>% 
    add(Equation("F1", "THETA_F1*exp(ETA_F1 + IOV_F1)"), Position(Equation("Q")))

  getDataset <- function(model) {
    dataset <- Dataset(10) %>%
      add(Bolus(time=0, amount=1000, compartment=1)) %>%
      add(Bolus(time=24, amount=1000, compartment=1)) %>%
      add(Bolus(time=48, amount=1000, compartment=1)) %>%
      add(Observations(times=seq(0,72, by=4))) %>% 
      add(IOV(colname="IOV_F1", distribution=EtaDistribution(model, omega="IOV_F1")))
    return(dataset)
  }

  # IIV only
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_boluses_iiv_f1")

  # IIV + IOV (RxODE / mrgsolve)
  dataset <- getDataset(model)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  # NOCB=FALSE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
  
  # NOCB=TRUE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})


test_that(getTestName("Simulate IOV on ALAG1"), {
  # This test always failed with RxODE version > 1.0.5 & < 1.1.0
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_boluses_iiv_iov_alag1"

  # Model with IIV on ALAG1
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    add(Theta("ALAG1", value=5)) %>%
    add(Omega("ALAG1", value=0.2^2)) %>%
    add(Omega("IOV_ALAG1", value=0.2^2, same=FALSE)) %>% # 20% IOV in model  
    add(LagTime(compartment=1, rhs="ALAG1")) %>%
    add(Equation("ALAG1", "THETA_ALAG1*exp(ETA_ALAG1 + IOV_ALAG1)"))

  obsTimes <- seq(0,72, by=0.5)
  getDataset <- function(model, times) {
    dataset <- Dataset(10) %>%
      add(Bolus(time=0, amount=1000, compartment=1)) %>%
      add(Bolus(time=24, amount=1000, compartment=1)) %>%
      add(Bolus(time=48, amount=1000, compartment=1)) %>%
      add(Observations(times=times)) %>%
      add(IOV(colname="IOV_ALAG1", distribution=EtaDistribution(model, omega="IOV_ALAG1")))
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
    #cat(startTime)
    obsTimes <- seq(startTime, 72, by=1)
    dataset <- getDataset(model, obsTimes)

    #table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed, nocb=FALSE)
    #table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed, nocb=FALSE)

    # NOCB=FALSE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE))))
    test <- expression(
      # Bug in RxODE/rxode2 (time 0 is there while it should not)
      results <- if (startTime != 0) {results %>% dplyr::filter(TIME != 0)} else {results},
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
    
    # NOCB=TRUE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE))))
    test <- expression(
      # Bug in RxODE/rxode2 (time 0 is there while it should not)
      results <- if (startTime != 0) {results %>% dplyr::filter(TIME != 0)} else {results},
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
  }
})

test_that(getTestName("Simulate IOV on D1"), {
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_infusions_iiv_iov_d1"

  # Model with IIV on D1
  model <- model_suite$testing$nonmem$advan3_trans4 %>%
    add(Theta("D1", value=5)) %>%
    add(Omega("D1", value=0.2^2)) %>%
    add(Omega("IOV_D1", value=0.5^2, same=FALSE)) %>% # 50% IOV  
    add(InfusionDuration(compartment=1, rhs="D1")) %>%
    add(Equation("D1", "THETA_D1*exp(ETA_D1 + IOV_D1)"))

  getDataset <- function(model) {
    dataset <- Dataset(10) %>%
      add(Infusion(time=0, amount=1000, compartment=1)) %>% 
      add(Infusion(time=24, amount=1000, compartment=1)) %>% 
      add(Infusion(time=48, amount=1000, compartment=1)) %>% 
      add(Observations(times=seq(0,72, by=0.5))) %>% 
      add(IOV(colname="IOV_D1", distribution=EtaDistribution(model, omega="IOV_D1")))
    return(dataset)
  }

  # IIV only
  model_no_iov <- model %>% disable("IOV")
  dataset_no_iov <- getDataset(model_no_iov)
  datasetRegressionTest(dataset_no_iov, model_no_iov, seed=seed, filename="3_infusions_iiv_d1")

  # IIV + IOV (RxODE / mrgsolve)
  dataset <- getDataset(model)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  # NOCB=FALSE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
  
  # NOCB=TRUE
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate IOV on F1"), {
  if (skipLongTests()) return(TRUE)
  regFilename <- "3_bolus_iov_on_f1"

  # Model with IIV on D1
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    add(Theta("F1", value=0.75)) %>%
    add(Omega("F1", value=0.09)) %>% # 30% CV  
    add(Equation("F1", "THETA_F1*exp(ETA_F1 + IOV_F1)")) %>%
    add(Bioavailability(1, rhs="F1"))

  startTimes <- c(0,24,47) # 47,48 NOT WORKING WITH MRGSOLVE LOCF
  for (startTime in startTimes) {
    #cat(startTime)
    obsTimes <- seq(startTime, 72, by=1)
    dataset <- Dataset(3) %>%
      add(Bolus(time=0, amount=1000, compartment=1)) %>%
      add(Bolus(time=24, amount=1000, compartment=1)) %>%
      add(Bolus(time=48, amount=1000, compartment=1)) %>%
      add(Observations(times=obsTimes)) %>% 
      add(IOV(colname="IOV_F1", NormalDistribution(0, 0.5))) # 50% CV in addition

    #table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
    #table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
    
    # NOCB=FALSE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(FALSE)), outvars="F1"))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
    
    # NOCB=TRUE
    simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, settings=Settings(NOCB(TRUE)), outvars="F1"))
    test <- expression(
      outputRegressionTest(results, output="CP", filename=regFilename, times=obsTimes),
      spaghettiPlot(results, "CP")
    )
    campsisTest(simulation, test, env=environment())
  }
})
