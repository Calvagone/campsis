library(testthat)
library(campsismod)

context("Test the simulate method with characteristics implemented in model")
seed <<- 1

test_that("Add lag time to model", {
  model <- model_library$advan4_trans4
  model <- model %>% add(LagTime(1, "2*exp(ETA_KA)"))

  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
  
})

test_that("Add bioavailability to model", {
  model <- model_library$advan4_trans4 %>% disable("IIV")
  model <- model %>% add(Bioavailability(1, "0.75*exp(IOV_F1)"))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,48, by=0.5)))
  
  dataset <- dataset %>% add(IOV("IOV_F1", distribution=NormalDistribution(0, 0.05)))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
  
})

test_that("Add infusion rate to model", {
  model <- model_library$advan3_trans4
  model <- model %>% add(InfusionRate(1, "200"))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
})

test_that("Add infusion duration to model", {
  model <- model_library$advan3_trans4
  model <- model %>% add(InfusionDuration(1, "5"))

  dataset <- Dataset(10)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))

  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
})

test_that("Reproduce RxODE bug with a minimalist example", {
  model <- model_library$advan4_trans4 %>% disable("IIV")
  model <- model %>% add(Bioavailability(1, "0.75*exp(IOV_F1)"))
  model <- model %>% add(InfusionDuration(1, "1*exp(ETA_KA)"))
  
  dataset <- Dataset(10)
  dataset <- dataset %>% add(Infusion(time=0.01, amount=1000, compartment=1)) # Setting time=0 makes RxODE crash
  dataset <- dataset %>% add(Infusion(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,48, by=0.5)))
  
  dataset <- dataset %>% add(IOV("IOV_F1", distribution=NormalDistribution(0, 0.05)))
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaghettiPlot(results, "CP")
  
})
