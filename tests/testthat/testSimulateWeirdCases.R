library(testthat)

context("Test that simulations with weird cases work as expected")

seed <- 1
source(paste0("", "testUtils.R"))

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
