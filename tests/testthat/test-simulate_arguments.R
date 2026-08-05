library(testthat)

context("Test the simulate arguments")

source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Argument dest works well", {
  model <- model_suite$testing$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(0, 24, by = 0.5)))

  # Unknown engine
  expect_error(
    model %>% simulate(dataset = dataset, dest = "ENGINE3"),
    regexp = "Argument 'dest' must be one of: 'rxode2', 'mrgsolve' or NULL"
  )

  if (no_engine_installed()) {
    return(TRUE)
  }

  # Default engine: rxode2 (first choice) or mrgsolve (second choice)
  results <- model %>% simulate(dataset = dataset)
  expect_equal(nrow(results), 49)
})

test_that("Auto seed value vs fix seed + unspecified engine", {
  model <- model_suite$testing$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time = 0, amount = 1000, compartment = 1)) %>%
    add(Observations(times = seq(0, 24, by = 0.5)))

  if (no_engine_installed()) {
    return(TRUE)
  }

  # Fix seed
  results1 <- model %>% simulate(dataset = dataset, seed = 10)
  results2 <- model %>% simulate(dataset = dataset, seed = 10)
  expect_true(all(results1$CP == results2$CP))
  expect_equal(results1, results2)

  # mrgsolve = choice 1
  if (engine_installed("mrgsolve")) {
    expect_true(all(c("CP", "OBS_CP", "Y") %in% colnames(results1)))
  } else {
    # rxode2 = choice 2
    if (engine_installed("rxode2")) {
      expect_true(all(c("KA", "CL", "V2", "CP", "OBS_CP", "Y") %in% colnames(results1)))
    }
  }

  # Auto seed vs fixed seed
  results1 <- model %>% simulate(dataset = dataset) # Auto
  results2 <- model %>% simulate(dataset = dataset, seed = 10) # Fixed
  expect_false(all(results1$CP == results2$CP))
})
