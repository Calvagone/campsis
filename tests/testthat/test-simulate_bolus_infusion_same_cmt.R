library(testthat)

context("Simulate boluses and infusions into the same compartment")

seed <- 1
source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Bolus and infusion in CMT 1", {
  regFilename <- "bolus_infusion_same_cmt"

  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% add(InfusionDuration(compartment = 1, rhs = "5"))

  dataset <- Dataset(1) %>%
    add(Bolus(time = 5, amount = 500, compartment = 1)) %>%
    add(Infusion(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(0, 24, by = 0.5)))

  datasetRegressionTest(dataset, model, seed = seed, filename = regFilename)

  simulation <- expression(simulate(model = model, dataset = dataset, dest = destEngine, seed = seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output = "CP", filename = regFilename)
  )
  # SuppressWarnings is called to suppress the following warning in mrgsolve (>=1.7.1):
  # [mrgsolve] RATE is not -2 on a dosing record with modeled infusion duration; either set the modeled duration to zero or use the `@!check_modeled_infusions` block option for $MAIN/$PK to slience this warning.
  suppressWarnings(campsisTest(simulation, test, env = environment()))
})
