library(testthat)

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
  model <- model %>% campsismod::replace(Equation("CL", paste0((model %>% getEquation("CL"))@rhs, "*pow(BW/70, 0.75)")))
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

test_that("Interruptions at doses times - BW covariate - IOV on KA - No events - (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  model <- model %>% campsismod::replace(Equation("CL", paste0((model %>% getEquation("CL"))@rhs, "*pow(BW/70, 0.75)")))
  model <- model %>% campsismod::replace(Equation("KA", "THETA_KA*exp(ETA_KA + IOV_KA)"))
  regFilename <- "event_interruption_at_dose_iov"
  
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,48,by=5)))
  dataset <- dataset %>% add(Covariate(name="BW", 70))
  dataset <- dataset %>% add(IOV(colname="IOV_KA", distribution=NormalDistribution(0, sd=0.3)))
  
  # Uncomment these 2 lines to understand how IOV is exported!
  # dataset <- dataset %>% add(new("event_related_observations", times=c(10, 23, 24, 34), compartment=as.integer(NA)))
  # table <- dataset %>% export(dest="RxODE", event_related=TRUE)
  
  events <- Events()
  # 10 OBS TIME
  # 23 NOTHING, JUST BEFORE DOSE
  # 24 DOSE
  # 34 NOTHING
  event1 <- Event(name="Event 1", times=c(10, 23, 24, 34), fun=function(inits) {
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


test_that("Simulate initial conditions + events (RxODE/mrgsolve)", {
  model <- model_library$advan3_trans4
  model <- model %>% add(InitialCondition(compartment=1, rhs="1000"))
  
  regFilename <- "initial_conditions"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=1)))
  
  events <- Events()
  event1 <- Event(name="Dummy event", times=c(0, 10), fun=function(inits) {
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, events=events)
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, events=events)
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1 %>% dplyr::filter(time >=5), output="CP", filename=regFilename)
  outputRegressionTest(results2 %>% dplyr::filter(time >=5), output="CP", filename=regFilename)
})

test_that("Simulate multiple arms + events (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  
  regFilename <- "event_in_multiple_arms"

  dataset <- Dataset()
  # Treatment arm 1
  arm1 <- Arm(id=1, subjects=2)
  arm1 <- arm1 %>% add(Bolus(time=c(0,24,48), amount=1000))
  arm1 <- arm1 %>% add(Observations(times=seq(0,72, by=5)))
  
  # Treatment arm 2
  arm2 <- Arm(id=2, subjects=2)
  arm2 <- arm2 %>% add(Bolus(time=c(5,29,53), amount=2000))
  arm2 <- arm2 %>% add(Observations(times=seq(0,77, by=6)))
  dataset <- dataset %>% add(c(arm1, arm2))
  
  events <- Events()
  event1 <- Event(name="Dummy event", times=c(0, 5, 20, 29, 40, 48), fun=function(inits) {
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

