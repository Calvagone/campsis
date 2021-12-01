library(testthat)
library(ggplot2)

context("Simulation with full uncertainty (variance-covariance matrix)")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("VPC on CP (predicate)", {
  model <- model_library$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"
  
  ds <- Dataset(100)
  for (day in 0:2) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 3*24, by=4)))

  results1 <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, outfun=~PI(.x, output="CP"), seed=seed)
  vpcPlot(results1)
  results2 <- model %>% simulate(dataset=ds, dest="mrgsolve", replicates=5, outfun=~PI(.x, output="CP"), seed=seed)
  vpcPlot(results2)
  
  vpcOutputRegressionTest(results1, output="CP", filename=regFilename)
  vpcOutputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("VPC on both CP and Y (function)", {

  model <- model_library$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"

  fun <- function(x) {
    return(dplyr::bind_rows(
            PI(x=x, output="CP", level=0.90, gather=TRUE) %>% dplyr::mutate(output="CP"),
            PI(x=x, output="Y", level=0.90, gather=TRUE) %>% dplyr::mutate(output="Y")))
  }

  ds <- Dataset(100)
  for (day in 0:2) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 3*24, by=4)))

  results1 <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, outfun=fun, seed=seed)
  vpcPlot(results1, scenarios="output") + facet_wrap(~output)

  # vpcOutputRegressionTest(results1, output="Y", filename=regFilename) # Not a good test because seed is controlled by RxODE
  vpcOutputRegressionTest(results1, output="CP", filename=regFilename)
})

