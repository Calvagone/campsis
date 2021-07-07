library(testthat)
library(campsismod)

context("Test that the simulate method works even when no OMEGA's/SIGMA's are provided")
seed <- 1

test_that("Simulate a 1-cpt model without OMEGA's/SIGMA's (Github issue #8)", {
  model <- model_library$advan2_trans2
  oldParameters <- model@parameters
  model@parameters <- Parameters()
  # Keep THETA's only
  model@parameters <- model@parameters %>% add(oldParameters %>% select("theta"))
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  dataset <- dataset %>% add(Covariate(name="EPS_PROP", 0))
  dataset <- dataset %>% add(Covariate(name="ETA_V", 0))
  dataset <- dataset %>% add(Covariate(name="ETA_CL", 0))
  dataset <- dataset %>% add(Covariate(name="ETA_KA", 0))
  
  
  results <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  subject1 <- results %>% dplyr::filter(id==1) %>% dplyr::select(-id)
  subject2 <- results %>% dplyr::filter(id==2) %>% dplyr::select(-id)
  subject3 <- results %>% dplyr::filter(id==3) %>% dplyr::select(-id)
  
  # No IIV, no RUV -> subject 1 strictly identical to subject 2 and 3
  expect_equal(subject1, subject2)
  expect_equal(subject2, subject3)
})