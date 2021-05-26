library(testthat)
library(pmxmod)

context("Test the events/interruption logic")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simple interrutpions - No events - (RxODE/mrgsolve)", {
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

test_that("Interruptions at doses times - BW covariate - No events - (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("CL", paste0(model %>% getEquation("CL"), "*pow(BW/70, 0.75)"))
  regFilename <- "event_interruption_at_dose"
  
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,48,by=5)))
  dataset <- dataset %>% add(Covariate(name="BW", 70))
  
  events <- Events()
  # 10 is part of the observations
  # 24 is not part of the observations but is a bolus time
  # 34 is not part of the observations and not part of the doses times
  event1 <- Event(name="Event 1", times=c(10, 24, 34), fun=function(inits) {
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
