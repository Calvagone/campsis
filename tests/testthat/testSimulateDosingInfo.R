library(testthat)

context("Test the simulate method with argument 'dosing' set to TRUE")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Dose adaptations can be checked in CAMPSIS output if dosing is TRUE"), {
  model <- model_suite$nonmem$advan4_trans4

  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2) %>%
    add(Bolus(time=seq(0,6)*24, amount=0.5)) %>% # 0.5mg / kg
    add(Observations(times=times)) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))

  # No dosing output
  expectedLength <- times %>% length() * dataset %>% length()
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(results %>% nrow(), expectedLength)
  )
  campsisTest(simulation, test, env=environment())
  
  # With dosing output
  expectedLength <- expectedLength + (dataset@arms %>% default())@protocol@treatment %>% length() * dataset %>% length()
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, dosing=TRUE))
  test <- expression(
    expect_equal(results %>% nrow(), expectedLength),
    expect_equal(results %>% dosingOnly() %>% dplyr::pull(AMT), c(rep(50,7), rep(25,7))),
    spaghettiPlot(results, "A_DEPOT"), # RxODE always puts dose before the observation for same time
    shadedPlot(results, "A_DEPOT")
  )
  campsisTest(simulation, test, env=environment())
})
