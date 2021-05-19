library(testthat)
library(pmxmod)

context("Test the simulate method with initial conditions")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- "C:/prj/pmxsim/tests/testthat/"
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Simulate initial conditions, observations starting at 0 (RxODE/mrgsolve)", {
  model <- model_library$advan3_trans4
  model <- model %>% add(InitialCondition(compartment=1, rhs="1000"))
  
  regFilename <- "simple_bolus"
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Observations(times=seq(0,72, by=0.5)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  spaguettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  spaguettiPlot(results2, "CP")
  
  #outputRegressionTest(results1, output="CP", filename=regFilename)
  #outputRegressionTest(results2, output="CP", filename=regFilename)
})

# test_that("Simulate initial conditions, observations starting at 5 (RxODE/mrgsolve)", {
#   model <- model_library$advan3_trans4
#   model <- model %>% add(InitialCondition(compartment=1, rhs="1000"))
#   
#   regFilename <- "simple_bolus"
#   
#   dataset <- Dataset(3)
#   dataset <- dataset %>% add(Observations(times=seq(5,72, by=0.5)))
#   
#   results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
#   spaguettiPlot(results1, "CP")
#   
#   results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
#   spaguettiPlot(results2, "CP")
#   
#   #outputRegressionTest(results1, output="CP", filename=regFilename)
#   #outputRegressionTest(results2, output="CP", filename=regFilename)
# })
