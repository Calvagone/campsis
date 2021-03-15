library(testthat)
library(pmxmod)

context("Test the simulate method with IOV")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- "C:/prj/pmxsim/tests/testthat/"
seed <<- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate 1000mg QD with IOV on KA", {
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
  regressionTest(dataset, model, seed=seed, filename="3_boluses_iov_ka.csv")
})
