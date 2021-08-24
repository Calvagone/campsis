library(testthat)

context("Test the simulate method with events")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

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
  spaghettiPlot(results1, "CP") # Still drug in A_PERIPHERAL
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaghettiPlot(results2, "CP") # Still drug in A_PERIPHERAL

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Give daily dose in absortion (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "event_daily_dose"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Observations(times=seq(0,24*7, by=4)))

  events <- Events()
  event1 <- Event(name="Event 1", times=seq(0, 24*6, by=24), fun=function(inits) {
    inits$A_DEPOT <- inits$A_DEPOT + 1000
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Daily dose in dataset + daily dose through events  (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  regFilename <- "event_daily_dose"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=seq(0, 24*6, by=24), amount=500))
  dataset <- dataset %>% add(Observations(times=seq(0,24*7, by=4)))
  
  events <- Events()
  event1 <- Event(name="Half daily dose", times=seq(0, 24*6, by=24), fun=function(inits) {
    inits$A_DEPOT <- inits$A_DEPOT + 500
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed)
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed)
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Body weight as a time varying covariate (RxODE/mrgsolve)", {
  model <- model_library$advan2_trans2
  model <- model %>% campsismod::replace(Equation("CL", paste0((model %>% getEquation("CL"))@rhs, "*pow(BW/70, 0.75)")))
  regFilename <- "event_varying_bw"
  
  dataset <- Dataset(3)
  for (day in 0:2) {
    dataset <- dataset %>% add(Bolus(time=day*24, amount=1000))
  }
  dataset <- dataset %>% add(Observations(times=seq(0,24*2, by=1)))
  dataset <- dataset %>% add(TimeVaryingCovariate("BW", 100))
  
  events <- Events()
  event1 <- Event(name="Event 1", times=15, fun=function(inits) {
    inits$BW <- 60
    return(inits)
  })
  event2 <- Event(name="Event 2", times=30, fun=function(inits) {
    inits$BW <- 30
    return(inits)
  })
  events <- events %>% add(event1) %>% add(event2)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed, outvars="BW")
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed, outvars="BW")
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("Dose adaptation based on Ctrough (RxODE/mrgsolve)", {
  model <- model_library$advan2_trans2
  regFilename <- "dose_adaptation_ctrough"
  
  dataset <- Dataset(5)
  days <- 7
  dataset <- dataset %>% add(Observations(times=seq(0,24*days, by=1)))
  dataset <- dataset %>% add(TimeVaryingCovariate("DOSE", 1500))
  
  events <- Events()
  event1 <- Event(name="Dose adaptation", times=(seq_len(days)-1)*24, fun=function(inits) {
    inits$DOSE <- ifelse(inits$CP > 5, inits$DOSE*0.75, inits$DOSE)
    inits$A_DEPOT <- inits$A_DEPOT + inits$DOSE
    return(inits)
  })
  events <- events %>% add(event1)
  
  results1 <- model %>% simulate(dataset, dest="RxODE", events=events, seed=seed, outvars="DOSE")
  spaghettiPlot(results1, "CP")
  spaghettiPlot(results1, "DOSE")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", events=events, seed=seed, outvars="DOSE")
  spaghettiPlot(results2, "CP")
  spaghettiPlot(results2, "DOSE")
  
  outputRegressionTest(results1, output=c("CP", "DOSE"), filename=regFilename)
  outputRegressionTest(results2, output=c("CP", "DOSE"), filename=regFilename)
})
