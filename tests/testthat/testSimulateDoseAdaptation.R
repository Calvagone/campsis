library(testthat)

context("Test simple dose adaptations (based on a covariate present in dataset)")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Dose adaptations based on weight work well"), {
  model <- model_suite$nonmem$advan4_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw"
  
  dataset <- Dataset(2) %>%
    add(Bolus(time=seq(0,6)*24, amount=0.5)) %>% # 0.5mg / kg  
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Dose adaptations preserve specified infusion duration"), {
  model <- model_suite$nonmem$advan3_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw_infusion"
  
  dataset <- Dataset(2) %>%
    add(Infusion(time=seq(0,6)*24, amount=0.5, duration=10)) %>% # 0.5mg/kg, 10 hours 
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Dose adaptations preserve specified infusion rate"), {
  model <- model_suite$nonmem$advan3_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw_infusion"
  
  subj1 <- Arm(subjects=1) %>%
    add(Infusion(time=seq(0,6)*24, amount=0.5, rate=5)) %>% # 0.5mg/kg, 50mg/10h=5mg/h 
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", 100)) %>%
    add(DoseAdaptation("AMT*WT"))
  
  subj2 <- Arm(subjects=1) %>%
    add(Infusion(time=seq(0,6)*24, amount=0.5, rate=2.5)) %>% # 0.5mg/kg, 25mg/10h=2.5mg/h 
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", 50)) %>%
    add(DoseAdaptation("AMT*WT"))
  
  dataset <- Dataset() %>% add(c(subj1, subj2))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Dose adaptations based on weight work well, check argument compartments works as expected"), {
  model <- model_suite$nonmem$advan4_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw"
  
  # Add independant ODE
  model <- model %>% add(Ode("A_TEST", "-log(2)/100*A_TEST"))
  model <- model %>% updateCompartments()
  
  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2) %>% 
    add(Bolus(time=0, amount=0.5, compartment=1, ii=24, addl=6)) %>%
    add(Bolus(time=0, amount=1000, compartment=5)) %>%
    add(Observations(times=times)) %>%
    add(Covariate("WT", c(100, 50)))
  
  # Dose adaptation only for compartment 1 (not 5!)
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT", compartments=1))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    outputRegressionTest(results %>% dplyr::filter(TIME != 0), output="A_TEST", filename=regFilename), # Remove first time (RxODE:0, mrgsolve:1000)
	  spaghettiPlot(results, "A_TEST", "ID")
  )
  campsisTest(simulation, test, env=environment())
})
