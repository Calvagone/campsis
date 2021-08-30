library(testthat)

context("Test the simulate method with argument 'dosing' set to TRUE")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Dose adaptations can be checked in CAMPSIS output if dosing is TRUE (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=seq(0,6)*24, amount=0.5)) # 0.5mg / kg
  dataset <- dataset %>% add(Observations(times=times))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT"))
  
  results1a <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  
  # No dosing output
  expectedLength <- times %>% length() * dataset %>% length()
  expect_equal(results1a %>% nrow(), expectedLength)
  expect_equal(results2a %>% nrow(), expectedLength)
  
  results1b <- model %>% simulate(dataset, dest="RxODE", seed=seed, dosing=TRUE)
  results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, dosing=TRUE)
  
  # With dosing output
  expectedLength <- expectedLength + (dataset@arms %>% default())@protocol@treatment %>% length() * dataset %>% length()
  expect_equal(results1b %>% nrow(), expectedLength) 
  expect_equal(results2b %>% nrow(), expectedLength)
  
  expect_equal(results1b %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7)))
  expect_equal(results2b %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7)))
  
  # Check defaut plots are working
  spaghettiPlot(results1b, "A_DEPOT") # RxODE always puts dose before the observation for same time
  spaghettiPlot(results2b, "A_DEPOT")
  shadedPlot(results1b, "A_DEPOT")
  shadedPlot(results2b, "A_DEPOT")
})
