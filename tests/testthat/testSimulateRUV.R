library(testthat)
library(ggplot2)

context("Simulation with residual unexplained variability")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Test generated RUV is correct"), {
  model <- model_suite$nonmem$advan4_trans4
  
  ds <- Dataset(100) %>%
    add(Bolus(0, 1000)) %>%
    add(Observations(times=seq(0.1,24,by=0.1))) # No predose to avoid zero's
  
  # Back compute EPS_PROP
  # Y=CP*(EPS_PROP + 1)
  # EPS_PROP = Y/CP - 1
  
  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, seed=seed))
  test <- expression(
    eps <- results$Y / results$CP - 1,
    expect_equal(round(var(eps), 3), 0.025)
  )
  campsisTest(simulation, test, env=environment())
})
