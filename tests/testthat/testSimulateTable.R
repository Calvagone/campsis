library(testthat)

context("Test the simulate method with tabular datasets")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a bolus using the tabular dataset"), {
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus"
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  simulation <- expression(
    table <- dataset %>% export(dest=destEngine, model=model, seed=seed),
    simulate(model=model, dataset=table, dest=destEngine, seed=seed) # seed not important as IIV is part of table
  )
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
  
  # Same but remove ARM column (ARM not mandatory)
  simulation <- expression(
    table <- dataset %>% export(dest=destEngine, model=model, seed=seed) %>% dplyr::select(-ARM),
    simulate(model=model, dataset=table, dest=destEngine, seed=seed)  # seed not important as IIV is part of table
  )
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})