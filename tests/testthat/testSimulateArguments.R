library(testthat)

context("Test the simulate arguments")

source(paste0("", "testUtils.R"))

test_that("Argument dest works well", {
  model <- model_suite$nonmem$advan4_trans4
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  # Unknown engine
  expect_error(model %>% simulate(dataset=dataset, dest="ENGINE3"),
               regexp="Argument 'dest' must be one of: 'rxode2', 'RxODE', 'mrgsolve' or NULL")
  
  if (noEngineInstalled()) return(TRUE)
  
  # Default engine (RxODE)
  results <- model %>% simulate(dataset=dataset)
  expect_equal(nrow(results), 49)
})

test_that("Auto seed value vs fix seed + default engine", {
  model <- model_suite$nonmem$advan4_trans4
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  if (noEngineInstalled()) return(TRUE)
  
  # Fix seed
  results1 <- model %>% simulate(dataset=dataset, seed=10)
  results2 <- model %>% simulate(dataset=dataset, seed=10)
  expect_true(all(results1$CP==results2$CP))
  expect_equal(results1, results2)
  
  # Check RxODE was chosen as default engine
  if (engineInstalled("RxODE") || engineInstalled("rxode2")) {
    expect_true(all(c("KA", "CL", "V2") %in% colnames(results1)))
  }
  
  # Auto seed vs fixed seed
  results1 <- model %>% simulate(dataset=dataset) # Auto
  results2 <- model %>% simulate(dataset=dataset, seed=10) # Fixed
  expect_false(all(results1$CP==results2$CP))
  
})
