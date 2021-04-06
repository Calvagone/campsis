library(plyr)
library(dplyr)
library(testthat)
library(pmxmod)
library(ggplot2)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("Visualise median uncertainty", {
  
  model <- model_library$my_model1
  
  ds <- Dataset(100)
  for (day in 0:6) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 7*24)))
  results <- model %>% simulate(dataset=ds, dest="RxODE", replicates=10, output=c("id", "time", "Y"))
  
  summary <- results %>% group_by(replicate, time) %>% summarise(median=median(Y))
  summary <- summary %>% group_by(time) %>% summarise(med.low=quantile(median, 0.05),
                                                      med.med=quantile(median, 0.50),
                                                      med.up=quantile(median, 0.95))

  ggplot(summary, aes(x=time, y=med.med)) +
    geom_line(color="blue") +
    geom_ribbon(aes(ymin=med.low, ymax=med.up), alpha=0.3)
  
})
