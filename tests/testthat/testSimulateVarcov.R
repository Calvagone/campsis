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
  
  summary.med <- results %>% group_by(replicate, time) %>% summarise(metric=median(Y), .groups='drop')
  summary.med <- summary.med %>% group_by(time) %>% summarise(low=quantile(metric, 0.05),
                                                              med=quantile(metric, 0.50),
                                                              up=quantile(metric, 0.95))
  
  summary.pLo <- results %>% group_by(replicate, time) %>% summarise(metric=quantile(Y, 0.05), .groups='drop')
  summary.pLo <- summary.pLo %>% group_by(time) %>% summarise(low=quantile(metric, 0.05),
                                                              med=quantile(metric, 0.50),
                                                              up=quantile(metric, 0.95))
  
  summary.pUp <- results %>% group_by(replicate, time) %>% summarise(metric=quantile(Y, 0.95), .groups='drop')
  summary.pUp <- summary.pUp %>% group_by(time) %>% summarise(low=quantile(metric, 0.05),
                                                              med=quantile(metric, 0.50),
                                                              up=quantile(metric, 0.95))

  ggplot(summary.med, aes(x=time, y=med)) +
    geom_line(color="red", size=0.7) +
    geom_ribbon(aes(ymin=low, ymax=up), alpha=0.15, color=NA, fill="red") +
    geom_line(data=summary.pLo, color="red", lty="dashed", size=0.7) +
    geom_ribbon(aes(ymin=low, ymax=up), data=summary.pLo, alpha=0.15, color=NA, fill="blue") +
    geom_line(data=summary.pUp, color="red", lty="dashed", size=0.7) +
    geom_ribbon(aes(ymin=low, ymax=up), data=summary.pUp, alpha=0.15, color=NA, fill="blue")
  
})
