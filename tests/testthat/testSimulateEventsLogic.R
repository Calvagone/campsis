library(testthat)
library(pmxmod)

context("Test the events/interruption logic")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Interrupted simulation and uninterrupted simulation must give same results (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus" # Existing non-regression file (see testSimulateBolus.R)
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  events <- Events()
  # 10 is part of the observations
  # 20.1 is not part of the observations
  event1 <- Event(name="Event 1", times=c(10, 20.1), fun=function(inits) {
    # Don't do anything. Just return the inits data frame.
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1a <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaghettiPlot(results1a, "CP")
  
  results1b <- model %>% simulate(dataset, dest="RxODE", events=NULL, seed=seed)
  spaghettiPlot(results1b, "CP")
  
  results2a <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaghettiPlot(results2a, "CP")
  
  results2b <- model %>% simulate(dataset, dest="mrgsolve", events=NULL, seed=seed)
  spaghettiPlot(results2b, "CP")
  
  outputRegressionTest(results1a, output="CP", filename=regFilename)
  outputRegressionTest(results1b, output="CP", filename=regFilename)
  outputRegressionTest(results2a, output="CP", filename=regFilename)
  outputRegressionTest(results2b, output="CP", filename=regFilename)
})
