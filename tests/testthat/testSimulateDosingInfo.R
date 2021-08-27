library(testthat)

context("Test the simulate method with argument 'dosing' set to TRUE")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Dose adaptations can be checked in CAMPSIS output if dosing is TRUE (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=seq(0,6)*24, amount=0.5)) # 0.5mg / kg
  dataset <- dataset %>% add(Observations(times=seq(0,7*24, by=4)))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT"))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, dosing=TRUE) %>% dplyr::filter(evid==1)
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, dosing=TRUE, outvars=c("AMT", "CMT", "EVID")) %>% dplyr::filter(EVID==1)
  
  expect_equal(results1$amt, c(rep(50,7), rep(25,7)))
  expect_equal(results2$AMT, c(rep(50,7), rep(25,7)))
})
