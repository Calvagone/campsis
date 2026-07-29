library(testthat)

context("Test spaghetti_plot S3 method")

source(file.path(getwd(), test_path(), "testUtils.R"))
source(file.path(getwd(), test_path(), "testPlotUtils.R"))

#_______________________________________________________________________________
#----           1. colour = "auto", single arm, no scenarios                ----
#_______________________________________________________________________________

test_that("spaghetti_plot auto colour: single arm, no scenarios → no colour", {
  data <- make_data()  # no ARM / SCENARIO columns
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  # Internal helper must return NULL (nothing to colour by).
  expect_null(campsis:::.auto_colour_columns(tbl))

  # Plot is produced without error and is a ggplot.
  plot <- expect_no_error(spaghetti_plot(tbl))
  expect_s3_class(plot, "gg")

  # No colour label means no colour variable was applied.
  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----        2. colour = "auto", multiple arms and/or scenarios             ----
#_______________________________________________________________________________

test_that("spaghetti_plot auto colour: multiple arms → colour by ARM", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  expect_equal(campsis:::.auto_colour_columns(tbl), "ARM")

  plot <- expect_no_error(spaghetti_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

test_that("spaghetti_plot auto colour: multiple scenarios → colour by SCENARIO", {
  data <- make_data(scenarios = c("Low dose", "Low dose", "High dose"))
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), "SCENARIO")

  plot <- expect_no_error(spaghetti_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

test_that("spaghetti_plot auto colour: multiple arms + scenarios → colour by both", {
  data <- make_data(
    arms      = c("100 mg", "200 mg", "100 mg"),
    scenarios = c("Low dose", "Low dose", "High dose")
  )
  tbl <- make_std_campsis_tbl(data, dataset = two_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), c("ARM", "SCENARIO"))

  plot <- expect_no_error(spaghetti_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
})

#_______________________________________________________________________________
#----              3. colour = NULL (explicit, disables colouring)          ----
#_______________________________________________________________________________

test_that("spaghetti_plot colour = NULL: no colour even with multiple arms", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  plot <- expect_no_error(spaghetti_plot(tbl, colour = NULL))
  expect_s3_class(plot, "gg")

  expect_null(plot$labels$colour)
})

#_______________________________________________________________________________
#----                     4. missing variable → error                       ----
#_______________________________________________________________________________

test_that("spaghetti_plot raises an informative error for missing variable", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    spaghetti_plot(tbl, variable = "MISSING"),
    regexp = "Column\\(s\\) 'MISSING' not found"
  )
})

#_______________________________________________________________________________
#----               5. non-default variable (explicit override)             ----
#_______________________________________________________________________________

test_that("spaghetti_plot plots a non-default variable when supplied", {
  data     <- make_data()
  data$AUC <- data$CONC * 2
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(spaghetti_plot(tbl, variable = "AUC"))
  expect_s3_class(plot, "gg")
})
