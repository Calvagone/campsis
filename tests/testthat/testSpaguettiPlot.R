library(testthat)

context("Test spaguetti_plot S3 method")

source(file.path(getwd(), test_path(), "testUtils.R"))

#_______________________________________________________________________________
#----                              helpers                                  ----
#_______________________________________________________________________________

# Build a minimal std_campsis_tbl from a plain tibble + metadata components.
make_std_campsis_tbl <- function(data, dataset, scenarios = Scenarios()) {
  metadata <- new(
    "campsis_metadata",
    dataset    = dataset,
    dest       = "rxode2",
    scenarios  = scenarios,
    outvars    = character(0),
    outfun     = DefaultOutfun(),
    replicates = 1L
  )
  campsis:::new_campsis_tbl(data, metadata)
}

# Minimal single-arm dataset (no simulation engine required).
single_arm_dataset <- Dataset(subjects = 3) %>%
  add(Bolus(time = 0, amount = 100, compartment = 1)) %>%
  add(Observations(times = c(0, 6, 12, 18, 24)))

# Two-arm dataset.
two_arm_dataset <- Dataset() %>%
  add(
    Arm(subjects = 3, label = "100 mg") %>%
      add(Bolus(time = 0, amount = 100, compartment = 1)) %>%
      add(Observations(times = c(0, 6, 12, 18, 24)))
  ) %>%
  add(
    Arm(subjects = 3, label = "200 mg") %>%
      add(Bolus(time = 0, amount = 200, compartment = 1)) %>%
      add(Observations(times = c(0, 6, 12, 18, 24)))
  )

# Two scenarios.
two_scenarios <- Scenarios() %>%
  add(Scenario("Low dose")) %>%
  add(Scenario("High dose"))

# Fixed concentration profiles for 3 subjects x 5 time points.
make_data <- function(arms = NULL, scenarios = NULL) {
  n_subjects <- 3
  times <- c(0, 6, 12, 18, 24)
  n_times <- length(times)

  data <- tibble::tibble(
    ID   = rep(seq_len(n_subjects), each = n_times),
    TIME = rep(times, times = n_subjects),
    CONC = rep(c(10, 8, 6, 4, 2), times = n_subjects) * seq_len(n_subjects)
  )

  if (!is.null(arms)) {
    data$ARM <- rep(arms, each = n_times)
  }

  if (!is.null(scenarios)) {
    data$SCENARIO <- rep(scenarios, each = n_times)
  }

  data
}

#_______________________________________________________________________________
#----           1. colour = "auto", single arm, no scenarios                ----
#_______________________________________________________________________________

test_that("spaguetti_plot auto colour: single arm, no scenarios → no colour", {
  data <- make_data()  # no ARM / SCENARIO columns
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  # Internal helper must return NULL (nothing to colour by).
  expect_null(campsis:::.auto_colour_columns(tbl))

  # Plot is produced without error and is a ggplot.
  plot <- expect_no_error(spaguetti_plot(tbl))
  expect_s3_class(plot, "gg")

  # No colour label means no colour variable was applied.
  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----        2. colour = "auto", multiple arms and/or scenarios             ----
#_______________________________________________________________________________

test_that("spaguetti_plot auto colour: multiple arms → colour by ARM", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  # Helper must pick up ARM only.
  expect_equal(campsis:::.auto_colour_columns(tbl), "ARM")

  # Plot is produced without error.
  plot <- expect_no_error(spaguetti_plot(tbl))
  expect_s3_class(plot, "gg")

  # A colour label is present when a colour variable is applied.
  expect_false(is.null(plot$labels$colour))
})

test_that("spaguetti_plot auto colour: multiple scenarios → colour by SCENARIO", {
  data <- make_data(scenarios = c("Low dose", "Low dose", "High dose"))
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset, scenarios = two_scenarios)

  # Helper must pick up SCENARIO only.
  expect_equal(campsis:::.auto_colour_columns(tbl), "SCENARIO")

  plot <- expect_no_error(spaguetti_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

test_that("spaguetti_plot auto colour: multiple arms + scenarios → colour by both", {
  data <- make_data(
    arms      = c("100 mg", "200 mg", "100 mg"),
    scenarios = c("Low dose", "Low dose", "High dose")
  )
  tbl <- make_std_campsis_tbl(data, dataset = two_arm_dataset, scenarios = two_scenarios)

  # Helper must pick up ARM and SCENARIO.
  expect_equal(campsis:::.auto_colour_columns(tbl), c("ARM", "SCENARIO"))

  plot <- expect_no_error(spaguetti_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

#_______________________________________________________________________________
#----              3. colour = NULL (explicit, disables colouring)          ----
#_______________________________________________________________________________

test_that("spaguetti_plot colour = NULL: no colour even with multiple arms", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  # Explicitly disable colour — auto-detection must not run.
  plot <- expect_no_error(spaguetti_plot(tbl, colour = NULL))
  expect_s3_class(plot, "gg")

  # No colour label means auto-detection was bypassed.
  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----                     4. missing variable → error                       ----
#_______________________________________________________________________________

test_that("spaguetti_plot raises an informative error for missing variable", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    spaguetti_plot(tbl, variable = "MISSING"),
    regexp = "Column 'MISSING' not found"
  )
})

#_______________________________________________________________________________
#----               5. non-default variable (explicit override)             ----
#_______________________________________________________________________________

test_that("spaguetti_plot plots a non-default variable when supplied", {
  data      <- make_data()
  data$AUC  <- data$CONC * 2  # add a second numeric column
  tbl       <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(spaguetti_plot(tbl, variable = "AUC"))
  expect_s3_class(plot, "gg")
})
