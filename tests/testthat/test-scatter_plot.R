library(testthat)

context("Test scatter_plot S3 method")

source(file.path(getwd(), test_path(), "test-utils.R"))
source(file.path(getwd(), test_path(), "test-plot_utils.R"))

#_______________________________________________________________________________
#----           1. colour = "auto", single arm, no scenarios                ----
#_______________________________________________________________________________

test_that("scatter_plot auto colour: single arm, no scenarios → no colour", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_null(campsis:::.auto_colour_columns(tbl))

  plot <- expect_no_error(scatter_plot(tbl))
  expect_s3_class(plot, "gg")

  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----        2. colour = "auto", multiple arms and/or scenarios             ----
#_______________________________________________________________________________

test_that("scatter_plot auto colour: multiple arms → colour by ARM", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  expect_equal(campsis:::.auto_colour_columns(tbl), "ARM")

  plot <- expect_no_error(scatter_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

test_that("scatter_plot auto colour: multiple scenarios → colour by SCENARIO", {
  data <- make_data(scenarios = c("Low dose", "Low dose", "High dose"))
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), "SCENARIO")

  plot <- expect_no_error(scatter_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

test_that("scatter_plot auto colour: multiple arms + scenarios → colour by both", {
  data <- make_data(
    arms      = c("100 mg", "200 mg", "100 mg"),
    scenarios = c("Low dose", "Low dose", "High dose")
  )
  tbl <- make_std_campsis_tbl(data, dataset = two_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), c("ARM", "SCENARIO"))

  plot <- expect_no_error(scatter_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

#_______________________________________________________________________________
#----              3. colour = NULL (explicit, disables colouring)          ----
#_______________________________________________________________________________

test_that("scatter_plot colour = NULL: no colour even with multiple arms", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  plot <- expect_no_error(scatter_plot(tbl, colour = NULL))
  expect_s3_class(plot, "gg")

  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----                     4. missing variable → error                       ----
#_______________________________________________________________________________

test_that("scatter_plot raises an informative error for a missing variable", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    scatter_plot(tbl, variable = "MISSING"),
    regexp = "Column\\(s\\) 'MISSING' not found"
  )
})

test_that("scatter_plot error lists all missing columns when both are absent", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    scatter_plot(tbl, variable = c("X1", "X2")),
    regexp = "Column\\(s\\) 'X1'.*'X2' not found"
  )
})

#_______________________________________________________________________________
#----               5. non-default variable (explicit override)             ----
#_______________________________________________________________________________

test_that("scatter_plot plots a non-default single variable when supplied", {
  data     <- make_data()
  data$AUC <- data$CONC * 2
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(scatter_plot(tbl, variable = "AUC"))
  expect_s3_class(plot, "gg")
})

#_______________________________________________________________________________
#----               6. scatter_plot-specific: two-variable scatter          ----
#_______________________________________________________________________________

test_that("scatter_plot accepts two variables for an X vs Y scatter", {
  data     <- make_data()
  data$AUC <- data$CONC * 2
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  # Both variables present → X vs Y scatter at default time.
  plot <- expect_no_error(scatter_plot(tbl, variable = c("CONC", "AUC")))
  expect_s3_class(plot, "gg")
})

test_that("scatter_plot errors when more than two variables are supplied", {
  data     <- make_data()
  data$AUC <- data$CONC * 2
  data$VD  <- data$CONC * 3
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    scatter_plot(tbl, variable = c("CONC", "AUC", "VD")),
    regexp = "'variable' must have length 1 or 2"
  )
})

#_______________________________________________________________________________
#----              7. scatter_plot-specific: custom time point              ----
#_______________________________________________________________________________

test_that("scatter_plot filters to a supplied time point without error", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  # TIME=12 is present in the synthetic data (c(0, 6, 12, 18, 24)).
  plot <- expect_no_error(scatter_plot(tbl, time = 12))
  expect_s3_class(plot, "gg")
})

test_that("scatter_plot accepts multiple time points", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(scatter_plot(tbl, time = c(0, 12, 24)))
  expect_s3_class(plot, "gg")
})
