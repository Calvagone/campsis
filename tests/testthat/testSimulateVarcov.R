library(plyr)
library(dplyr)
library(testthat)
library(pmxmod)
library(ggplot2)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("VPC on Y", {
  
  model <- model_library$my_model1
  
  myFun <- function(x) {
    return(PI(x=x, output="CP", level=0.90, gather=TRUE))
  }
  
  ds <- Dataset(100)
  for (day in 0:6) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 7*24)))
  
  results <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, output=myFun)
  vpcPlot(results)
})

test_that("VPC on both CP and Y", {
  
  model <- model_library$my_model1
  
  myFun <- function(x) {
    return(dplyr::bind_rows(
            PI(x=x, output="CP", level=0.90, gather=TRUE) %>% dplyr::mutate(output="CP"),
            PI(x=x, output="Y", level=0.90, gather=TRUE) %>% dplyr::mutate(output="Y")))
  }
  
  ds <- Dataset(100)
  for (day in 0:6) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 7*24)))
  
  results <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, output=myFun)
  plots <- vpcPlot(results, scenarios="output")
  plots[[1]]
  plots[[2]]
})

