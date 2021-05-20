library(testthat)
library(pmxmod)

context("Test the simulate method with events")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("No-effect events (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  events <- Events()
  event1 <- Event(name="Event 1", times=c(10, 20.1), fun=function(inits) {
   return(inits)
  })
  events <- events %>% add(event1)
  
  results1a <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaguettiPlot(results1a, "CP")
  
  results1b <- model %>% simulate(dataset, dest="RxODE", events=NULL, seed=seed)
  spaguettiPlot(results1b, "CP")
  
  results2a <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaguettiPlot(results2a, "CP")
  
  results2b <- model %>% simulate(dataset, dest="mrgsolve", events=NULL, seed=seed)
  spaguettiPlot(results2b, "CP")
  
  outputRegressionTest(results1a, output="CP", filename=regFilename)
  outputRegressionTest(results1b, output="CP", filename=regFilename)
  outputRegressionTest(results2a, output="CP", filename=regFilename)
  outputRegressionTest(results2b, output="CP", filename=regFilename)
})

test_that("Clear central compartment events (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "clear_central_event"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  events <- Events()
  event1 <- Event(name="Event 1", times=c(15), fun=function(inits) {
    inits$A_CENTRAL <- 0
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaguettiPlot(results1, "CP") # Still drug in A_PERIPHERAL
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaguettiPlot(results2, "CP") # Still drug in A_PERIPHERAL

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})