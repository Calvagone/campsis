library(testthat)

context("Test the simulate method with infusions")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate infusion using duration in dataset, then in model"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$nonmem$advan3_trans4
  regFilename <- "infusion_duration"
  
  # 5 hours infusion duration implemented in dataset
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1000, compartment=1, duration=5)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
  

  # 5 hours infusion duration implemented in model
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  model <- model %>% add(InfusionDuration(compartment=1, rhs="5"))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate infusion using rate in dataset"), {
  model <- model_suite$nonmem$advan3_trans4
  regFilename <- "infusion_duration"
  
  # 5 hours infusion duration implemented in dataset
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1000, compartment=1, rate=200)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
  
  # 5 hours infusion duration implemented in model
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  model <- model %>% add(InfusionRate(compartment=1, rhs="200"))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate infusion using rate and lag time in dataset"), {
  model <- model_suite$nonmem$advan3_trans4
  regFilename <- "infusion_rate_lag_time1_dataset"
  
  # 5 hours duration
  duration <- 5
  # 2 hours lag time with 20% CV
  lag <- FunctionDistribution(fun="rlnorm", args=list(meanlog=log(2), sdlog=0.2))
  
  dataset <- Dataset(10) %>%
    add(Infusion(time=0, amount=1000, compartment=1, duration=duration, lag=lag)) %>%
    add(Observations(times=seq(0,24, by=0.5)))

  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49 * dataset %>% length()),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate infusion using rate and lag time (parameter distribution) in dataset"), {
  model <- model_suite$nonmem$advan3_trans4
  regFilename <- "infusion_rate_lag_time2_dataset"
  model <- model %>% add(Theta(name="ALAG1", index=5, value=2)) # 2 hours lag time
  model <- model %>% add(Omega(name="ALAG1", index=5, index2=5, value=0.2^2)) #20% CV

  dataset <- Dataset(10)
  lag <- ParameterDistribution(model, theta="ALAG1", omega="ALAG1")
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1, rate=200, lag=lag))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49 * dataset %>% length()),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
