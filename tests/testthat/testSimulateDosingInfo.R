library(testthat)

context("Test the simulate method with argument 'dosing' set to TRUE")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Dose adaptations can be checked in CAMPSIS output if dosing is TRUE (rxode2/mrgsolve)", {
  model <- model_library$advan4_trans4

  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2) %>%
    add(Bolus(time=seq(0,6)*24, amount=0.5)) %>% # 0.5mg / kg
    add(Observations(times=times)) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  results1a <- model %>% simulate(dataset, dest="rxode2", seed=seed)
  results2a <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)

  # No dosing output
  expectedLength <- times %>% length() * dataset %>% length()
  expect_equal(results1a %>% nrow(), expectedLength)
  expect_equal(results2a %>% nrow(), expectedLength)

  results1b <- model %>% simulate(dataset, dest="rxode2", seed=seed, dosing=TRUE)
  results2b <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, dosing=TRUE)

  # With dosing output
  expectedLength <- expectedLength + (dataset@arms %>% default())@protocol@treatment %>% length() * dataset %>% length()
  expect_equal(results1b %>% nrow(), expectedLength)
  expect_equal(results2b %>% nrow(), expectedLength)

  expect_equal(results1b %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7)))
  results2b
  expect_equal(results2b %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7))) # Not working on M1 MAC (arch="aarch64")

  # Check defaut plots are working
  spaghettiPlot(results1b, "A_DEPOT") # rxode2 always puts dose before the observation for same time
  spaghettiPlot(results2b, "A_DEPOT")
  shadedPlot(results1b, "A_DEPOT")
  shadedPlot(results2b, "A_DEPOT")
})

test_that("Debug previous test with mrgsolve on M1 Mac", {
  model <- model_library$advan4_trans4

  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2) %>%
    add(Bolus(time=seq(0,6)*24, amount=0.5)) %>% # 0.5mg / kg
    add(Observations(times=times)) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  table <- dataset %>% export(dest="mrgsolve")

  mrgmod <- model %>% export(dest="mrgsolve", outvars=c("EVID", "CMT", "AMT"))
  mod <- suppressMessages(mrgsolve::mcode(model="model", code=mrgmod %>% toString(), quiet=TRUE))

  results <- mod %>% mrgsolve::data_set(data=table) %>% mrgsolve::mrgsim(obsonly=FALSE, output="df", nocb=TRUE) %>% tibble::as_tibble()
  results %>% dosingOnly()
  expect_equal(results %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7)))
})
