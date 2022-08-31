library(testthat)

context("Test the simulate method with initial conditions")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate initial conditions, observations starting at 0"), {
  model <- model_suite$nonmem$advan3_trans4
  model <- model %>% add(InitialCondition(compartment=1, rhs="1000"))

  regFilename <- "initial_conditions"

  dataset <- Dataset(3) %>%
    add(Observations(times=seq(0,72, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results %>% dplyr::filter(TIME >=5), output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate initial conditions, observations starting at 5"), {
  model <- model_suite$nonmem$advan3_trans4
  model <- model %>% add(InitialCondition(compartment=1, rhs="1000"))

  regFilename <- "initial_conditions"

  dataset <- Dataset(3) %>%
    add(Observations(times=seq(5,72, by=1)))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
