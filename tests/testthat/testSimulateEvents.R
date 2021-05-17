library(testthat)
library(pmxmod)

context("Test the simulate method with events")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate simple events (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=48, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=1)))
  
  events <- Events()
  event1 <- Event(name="Event 1", times=c(10,30), fun=function(id, time) {
    # Do something
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaguettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaguettiPlot(results2, "CP")
})