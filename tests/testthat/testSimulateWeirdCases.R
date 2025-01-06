library(testthat)

context("Test that simulations with weird cases work as expected")

seed <- 1
source(paste0("", "testUtils.R"))
# options(campsis.options=list(SKIP_VERY_LONG_TESTS=FALSE))

test_that(getTestName("Simulate a bolus without observation"), {
  model <- model_suite$testing$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000))

  simulation <- expression()
  test <- expression(
    expect_error(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed),
                 regexp="Dataset does not contain any observation")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a bolus with single observation at time 0"), {
  model <- model_suite$testing$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(time=0))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 1),
    expect_equal(results[c("ID", "TIME", "CP")], tibble::tibble(ID=1, TIME=0, CP=0))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a model which is not valid"), {
  model <- model_suite$testing$nonmem$advan4_trans4

  # Corrupt name slot of parameter KA
  model@parameters@list[[1]]@name <- c("KA", "KA2")

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(time=0))

  simulation <- expression()
  test <- expression(
    expect_error(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed),
                 regexp="name is length 2. Should be 1.")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate a dataset which is not valid"), {
  model <- model_suite$testing$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(time=0))

  # Corrupt amount slot of first bolus
  dataset@arms@list[[1]]@protocol@treatment@list[[1]]@amount <- c(1000,1000)

  simulation <- expression()
  test <- expression(
    expect_error(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed),
                 regexp="amount is length 2. Should be 1.")
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Covariates must be trimmed by campsis to avoid issues"), {

  regFilename <- "trim_covariate"

   model <- CampsisModel() %>%
    add(Equation("EQ_DUMMY", "0")) %>% # Needed for rxode2 only
    add(Ode("A_DUMMY", "0")) %>% # Needed
    add(Equation("MY_COV", "COV0 + THETA_SLOPE*t"), pos=Position(OdeRecord())) %>%
    add(Theta("SLOPE", value=1.0))

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1, compartment=1)) %>%
    add(Covariate("COV0 ", c(10,20,30))) %>% # Trailing space has been voluntarily added
    add(Observations(c(0,1,2,3,4,5)))

  # Note: without trim,
  # rxode2: error is raised
  # mrgsolve: no error is raised. Variable not initialised properly.

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars="MY_COV"))
  test <- expression(
    outputRegressionTest(results, output="MY_COV", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Arm label mapping must first verify the ARM column exists"), {

  arm1 <- Arm(subjects=1, label="Arm 1") %>%
    add(Bolus(time=0, amount=1, compartment=1)) %>%
    add(Observations(c(0,1,2,3,4,5)))

  dataset <- Dataset() %>%
    add(arm1)

  model <- model_suite$testing$nonmem$advan4_trans4

  # Explicitely remove ARM column
  outfun <- Outfun(level="scenario", fun=~.x %>% dplyr::select(-dplyr::all_of("ARM")))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outfun=outfun))
  test <- expression(
    expect_true(nrow(results)==6),
    expect_false("ARM" %in% results)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Model advan1_trans1 must compile properly with mrgsolve v1.5.2 on Windows"), {
  model <- model_suite$nonmem$advan1_trans1
  regFilename <- "advan1_trans1"
  # See issue #160

  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=12, addl=2)) %>%
    add(Observations(times=c(0,12,24) %>% purrr::map(~.x + (1:11)) %>% purrr::list_c(), compartment=1))

  simulation <- expression(model %>% simulate(dataset, dest=destEngine, seed=seed))

  test <- expression(
    outputRegressionTest(results, output="CONC", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("No need to adapt 'future.globals.maxSize' option anymore when dataset is large."), {
  # See original issue #166
  
  # Load Campsis model
  model <- model_suite$pk$`1cpt_fo` %>%
    replace(Theta(name="CL", value=0.01)) %>%
    replace(Theta(name="KA", value=0.01))
  
  # Trial design (large dataset)
  dataset <- Dataset(1000) %>%
    add(Observations(times=0:months(8))) %>%
    add(Bootstrap(data=nhanes, replacement=TRUE, random=TRUE, export_id=TRUE))
  
  expect_true(length(dataset)==1000)
  
  # This test will be skipped most of the time
  # Simulation takes 15 seconds approximately with mrgsolve (OK)
  # Simulation is 60x slower with rxode2... (NOK)
  if (skipVeryLongTests()) return(TRUE)
  
  scenarios <- Scenarios() %>%  
    add(Scenario(name="Long simulation", dataset=~.x %>% add(Bolus(time=months(0:7), amount=10000, compartment=1))))
  
  simulation <- expression(results <- NULL,
                           tmp <- NULL,
                           tictoc::tic(),
                           tmp <- simulate(model=model, dataset=dataset, seed=seed, dest=destEngine, scenarios=scenarios),
                           tictoc::toc(),
                           tmp)
  
  test <- expression(
    expect_true(nrow(results)==length(0:months(8))*1000)
  )
  campsisTest(simulation, test, env=environment())
})
