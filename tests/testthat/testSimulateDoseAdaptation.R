library(testthat)

context("Test simple dose adaptations (based on a covariate present in dataset)")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Dose adaptations based on weight work well (rxode2/mrgsolve)", {
  model <- model_library$advan4_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw"

  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=seq(0,6)*24, amount=0.5)) # 0.5mg / kg
  dataset <- dataset %>% add(Observations(times=seq(0,7*24, by=4)))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT"))

  results1 <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)

  # spaghettiPlot(results1, "CP", "id")
  # spaghettiPlot(results2, "CP", "id")
})

test_that("Dose adaptations preserve specified infusion duration (rxode2/mrgsolve)", {
  model <- model_library$advan3_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw_infusion"

  dataset <- Dataset(2) %>%
    add(Infusion(time=seq(0,6)*24, amount=0.5, duration=10)) %>% # 0.5mg/kg, 10 hours
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  results1 <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)

  # spaghettiPlot(results1, "CP", "ID")
  # spaghettiPlot(results2, "CP", "ID")
})

test_that("Dose adaptations preserve specified infusion rate (rxode2/mrgsolve)", {
  model <- model_library$advan3_trans4 %>% disable("IIV")
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

  results1 <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)

  # spaghettiPlot(results1, "CP", "ID")
  # spaghettiPlot(results2, "CP", "ID")
})

test_that("Dose adaptations based on weight work well, check argument compartments works as expected (rxode2/mrgsolve)", {
  model <- model_library$advan4_trans4 %>% disable("IIV")
  regFilename <- "dose_adaptation_by_bw"

  # Add independant ODE
  model <- model %>% add(Ode("A_TEST", "-log(2)/100*A_TEST"))
  model <- model %>% updateCompartments()

  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=0, amount=0.5, compartment=1, ii=24, addl=6))
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=5))
  dataset <- dataset %>% add(Observations(times=times))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))

  # Dose adaptation only for compartment 1 (not 5!)
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT", compartments=1))

  results1 <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)

  # Remove first time (rxode2:0, mrgsolve:1000)
  outputRegressionTest(results1 %>% dplyr::filter(TIME != 0), output="A_TEST", filename=regFilename)
  outputRegressionTest(results2 %>% dplyr::filter(TIME != 0), output="A_TEST", filename=regFilename)

  # spaghettiPlot(results1, "A_TEST", "id")
  # spaghettiPlot(results2, "A_TEST", "id")
})
