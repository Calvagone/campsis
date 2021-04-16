library(testthat)
library(pmxmod)
library(ggplot2)

context("Simulation with residual unexplained variability")

test_that("Test RUV generated with RxODE & mrgsolve is correct", {
  model <- model_library$advan4_trans4
  ds <- Dataset(100)
  ds <- ds %>% add(Bolus(0, 1000))
  ds <- ds %>% add(Observations(times=seq(0.1,24,by=0.1))) # No predose to avoid zero's
  
  results1 <- model %>% simulate(dataset = ds, dest = "RxODE", seed=1)
  results2 <- model %>% simulate(dataset = ds, dest = "mrgsolve", seed=1)

  # Y=CP*(EPS_PROP + 1)
  # EPS_PROP = Y/CP - 1
  eps1_rxode <- results1$Y / results1$CP - 1
  expect_equal(round(var(eps1_rxode), 3), 0.025)
  
  eps1_mrgsolve <- results2$Y / results2$CP - 1
  expect_equal(round(var(eps1_mrgsolve), 3), 0.025)
})