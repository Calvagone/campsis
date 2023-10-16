library(testthat)

context("Test the simulate method with events")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Clear central compartment events"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "clear_central_event"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  events <- Events()
  event1 <- Event(name="Event 1", times=c(15), fun=function(inits) {
    inits$A_CENTRAL <- 0
    return(inits)
  })
  events <- events %>% add(event1)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, events=events, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP") # Still drug in A_PERIPHERAL
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Give daily dose in absortion"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "event_daily_dose"
  
  dataset <- Dataset(3) %>%
    add(Observations(times=seq(0,24*7, by=4)))

  events <- Events()
  event1 <- Event(name="Event 1", times=seq(0, 24*6, by=24), fun=function(inits) {
    inits$A_DEPOT <- inits$A_DEPOT + 1000
    return(inits)
  })
  events <- events %>% add(event1)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, events=events, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Daily dose in dataset + daily dose through events"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$testing$nonmem$advan4_trans4
  regFilename <- "event_daily_dose"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=seq(0, 24*6, by=24), amount=500)) %>%
    add(Observations(times=seq(0,24*7, by=4)))
  
  events <- Events()
  event1 <- Event(name="Half daily dose", times=seq(0, 24*6, by=24), fun=function(inits) {
    inits$A_DEPOT <- inits$A_DEPOT + 500
    return(inits)
  })
  events <- events %>% add(event1)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, events=events, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Body weight as an event covariate"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$testing$nonmem$advan2_trans2
  equation <- model %>% find(Equation("CL"))
  model <- model %>% replace(Equation("CL", paste0(equation@rhs, "*pow(BW/70, 0.75)")))
  regFilename <- "event_varying_bw"
  
  dataset <- Dataset(3)
  for (day in 0:2) {
    dataset <- dataset %>% add(Bolus(time=day*24, amount=1000))
  }
  dataset <- dataset %>% add(Observations(times=seq(0,24*2, by=1)))
  dataset <- dataset %>% add(EventCovariate("BW", 100))
  
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
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, events=events, seed=seed, outvars="BW"))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Dose adaptation based on Ctrough"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$testing$nonmem$advan2_trans2
  regFilename <- "dose_adaptation_ctrough"
  
  dataset <- Dataset(5)
  days <- 7
  dataset <- dataset %>% add(Observations(times=seq(0,24*days, by=1)))
  dataset <- dataset %>% add(EventCovariate("DOSE", 1500))
  
  events <- Events()
  event1 <- Event(name="Dose adaptation", times=(seq_len(days)-1)*24, fun=function(inits) {
    inits$DOSE <- ifelse(inits$CP > 5, inits$DOSE*0.75, inits$DOSE)
    inits$A_DEPOT <- inits$A_DEPOT + inits$DOSE
    return(inits)
  })
  events <- events %>% add(event1)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, events=events, seed=seed, outvars="DOSE"))
  test <- expression(
    outputRegressionTest(results, output=c("CP", "DOSE"), filename=regFilename),
    spaghettiPlot(results, "CP")
  )
  campsisTest(simulation, test, env=environment())
})
