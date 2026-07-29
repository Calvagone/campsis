library(testthat)

context("Test shaded_plot S3 method")

source(file.path(getwd(), test_path(), "test-utils.R"))
source(file.path(getwd(), test_path(), "test-plot_utils.R"))

#_______________________________________________________________________________
#----           1. colour = "auto", single arm, no scenarios                ----
#_______________________________________________________________________________

test_that("shaded_plot auto colour: single arm, no scenarios → no colour", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  # Same helper as used by spaghetti_plot — must return NULL.
  expect_null(campsis:::.auto_colour_columns(tbl))

  plot <- expect_no_error(shaded_plot(tbl))
  expect_s3_class(plot, "gg")

  # shadedPlot sets labs(colour=, fill=) together; neither should be present.
  expect_null(plot$labels$colour)
  expect_null(plot$labels$fill)
})

#_______________________________________________________________________________
#----        2. colour = "auto", multiple arms and/or scenarios             ----
#_______________________________________________________________________________

test_that("shaded_plot auto colour: multiple arms → colour by ARM", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  expect_equal(campsis:::.auto_colour_columns(tbl), "ARM")

  plot <- expect_no_error(shaded_plot(tbl))
  expect_s3_class(plot, "gg")

  # Both colour and fill labels are set by shadedPlot when colour is active.
  expect_false(is.null(plot$labels$colour))
  expect_false(is.null(plot$labels$fill))
})

test_that("shaded_plot auto colour: multiple scenarios → colour by SCENARIO", {
  data <- make_data(scenarios = c("Low dose", "Low dose", "High dose"))
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), "SCENARIO")

  plot <- expect_no_error(shaded_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
  expect_false(is.null(plot$labels$fill))
})

test_that("shaded_plot auto colour: multiple arms + scenarios → colour by both", {
  data <- make_data(
    arms      = c("100 mg", "200 mg", "100 mg"),
    scenarios = c("Low dose", "Low dose", "High dose")
  )
  tbl <- make_std_campsis_tbl(data, dataset = two_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_colour_columns(tbl), c("ARM", "SCENARIO"))

  plot <- expect_no_error(shaded_plot(tbl))
  expect_s3_class(plot, "gg")
  expect_false(is.null(plot$labels$colour))
  expect_false(is.null(plot$labels$fill))
})

#_______________________________________________________________________________
#----              3. colour = NULL (explicit, disables colouring)          ----
#_______________________________________________________________________________

test_that("shaded_plot colour = NULL: no colour even with multiple arms", {
  data <- make_data(arms = c("100 mg", "100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  plot <- expect_no_error(shaded_plot(tbl, colour = NULL))
  expect_s3_class(plot, "gg")

  expect_null(plot$labels$colour)
  expect_null(plot$labels$fill)
})

#_______________________________________________________________________________
#----                     4. missing variable → error                       ----
#_______________________________________________________________________________

test_that("shaded_plot raises an informative error for missing variable", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_error(
    shaded_plot(tbl, variable = "MISSING"),
    regexp = "Column\\(s\\) 'MISSING' not found"
  )
})

#_______________________________________________________________________________
#----               5. non-default variable (explicit override)             ----
#_______________________________________________________________________________

test_that("shaded_plot plots a non-default variable when supplied", {
  data     <- make_data()
  data$AUC <- data$CONC * 2
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(shaded_plot(tbl, variable = "AUC"))
  expect_s3_class(plot, "gg")
})

#_______________________________________________________________________________
#----              6. shaded_plot-specific: strat_extra forwarded           ----
#_______________________________________________________________________________

test_that("shaded_plot forwards strat_extra without error", {
  # strat_extra adds a stratification column used for PI computation but not
  # mapped to colour — intended for use with facet_wrap().
  data     <- make_data()
  data$WT  <- rep(c(50, 70, 90), each = 5)
  tbl      <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(shaded_plot(tbl, strat_extra = "WT"))
  expect_s3_class(plot, "gg")
})

#_______________________________________________________________________________
#----           7. shaded_plot-specific: level and alpha forwarded          ----
#_______________________________________________________________________________

test_that("shaded_plot accepts custom level and alpha without error", {
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  plot <- expect_no_error(shaded_plot(tbl, level = 0.80, alpha = 0.10))
  expect_s3_class(plot, "gg")
})
