library(testthat)
library(pmxmod)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("Ctrough uncertainty at day 7", {
  
  model <- model_library$my_model1
  
  ds <- Dataset(100)
  for (day in 0:6) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 7*24)))
  
  results <- model %>% simulate(dataset=ds, dest="RxODE")
  
})
