library(testthat)

context("Test the simulate method with scenarios")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate a bolus (RxODE/mrgsolve)", {
  
  scenarios <- Scenarios() %>%
    add(Scenario()) %>% # Original model and dataset
    add(Scenario(model=~.x %>% replace(Theta("KA", value=0.5)))) %>%
    add(Scenario(model=~.x %>% replace(Theta("KA", value=0.1))))
  
  model <- model_library$advan4_trans4
  regFilename <- "simple_bolus"
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  
  # RxODE
  results1 <- model %>% simulate(dataset, dest="RxODE", scenarios=scenarios, seed=seed)
  spaghettiPlot(results1, "CP", "SCENARIO")
  expect_equal(nrow(results1), 49*3)
  
  # Mrgsolve
  results2 <- model %>% simulate(dataset, dest="mrgsolve", scenarios=scenarios, seed=seed)
  spaghettiPlot(results2, "CP", "SCENARIO")
  expect_equal(nrow(results2), 49*3)
})