library(plyr)
library(dplyr)
library(testthat)
library(pmxmod)
library(ggplot2)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("Visualise median uncertainty", {
  
  model <- model_library$my_model1
  
  myFun <- function(x) {
    return(PI(x=x, output="Y", level=0.90))
  }
  
  ds <- Dataset(100)
  for (day in 0:6) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 7*24)))
  
  results <- model %>% simulate(dataset=ds, dest="RxODE", replicates=10, output=myFun)
  vpcPlot(results)
  
})
