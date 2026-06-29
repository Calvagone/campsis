library(testthat)

context("Test vpc_plot S3 method")

source(file.path(getwd(), test_path(), "testUtils.R"))
source(file.path(getwd(), test_path(), "testPlotUtils.R"))

#_______________________________________________________________________________
#----                        VPC data factory                               ----
#_______________________________________________________________________________

#' Build a minimal VPC-format tibble (replicate × TIME × metric).
#'
#' Columns: replicate, TIME, metric, value — the format produced by PIOutfun.
#'
#' @param n_replicates number of distinct replicates (default 5)
#' @param arms         optional character vector of ARM labels, recycled to fit
#'                     all rows
#' @param scenarios    optional character vector of SCENARIO labels, recycled
#'                     to fit all rows
#' @return a tibble with VPC-format columns
make_vpc_data <- function(n_replicates = 5, arms = NULL, scenarios = NULL) {
  times   <- c(0, 6, 12, 18, 24)
  metrics <- c("low", "med", "up")

  data <- expand.grid(
    replicate = seq_len(n_replicates),
    TIME      = times,
    metric    = metrics,
    stringsAsFactors = FALSE
  )
  data$value <- seq_len(nrow(data)) * 0.5   # deterministic, no randomness
  data <- tibble::as_tibble(data)

  if (!is.null(arms))      data$ARM      <- rep_len(arms,      nrow(data))
  if (!is.null(scenarios)) data$SCENARIO <- rep_len(scenarios, nrow(data))

  data
}

#_______________________________________________________________________________
#----          1. no replicates → informative error                         ----
#_______________________________________________________________________________

test_that("vpc_plot raises an informative error when the replicate column is absent", {
  # make_data() produces a plain ID/TIME/CONC tibble with no replicate column.
  data <- make_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_false(campsis:::.is_replicated(tbl))

  expect_error(
    vpc_plot(tbl),
    regexp = "replicates > 1"
  )
})

test_that("vpc_plot raises an informative error when only one replicate is present", {
  # A replicate column is present but contains a single value — still not a
  # valid VPC input.
  data           <- make_vpc_data(n_replicates = 1)
  tbl            <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_false(campsis:::.is_replicated(tbl))

  expect_error(
    vpc_plot(tbl),
    regexp = "replicates > 1"
  )
})

#_______________________________________________________________________________
#----    2. strata = "auto" helper (.auto_strata)                           ----
#_______________________________________________________________________________

test_that(".auto_strata returns NULL when no ARM / SCENARIO columns are present", {
  data <- make_vpc_data()
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset)

  expect_null(campsis:::.auto_strata(tbl))
})

test_that(".auto_strata returns c(ARM = 'all') when multiple arms are present", {
  data <- make_vpc_data(arms = c("100 mg", "200 mg"))
  tbl  <- make_std_campsis_tbl(data, dataset = two_arm_dataset)

  expect_equal(campsis:::.auto_strata(tbl), c(ARM = "all"))
})

test_that(".auto_strata returns c(SCENARIO = 'all') when multiple scenarios are present (no ARM)", {
  data <- make_vpc_data(scenarios = c("Low dose", "High dose"))
  tbl  <- make_std_campsis_tbl(data, dataset = single_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_strata(tbl), c(SCENARIO = "all"))
})

test_that(".auto_strata prefers ARM over SCENARIO when both are present", {
  data <- make_vpc_data(
    arms      = c("100 mg", "200 mg"),
    scenarios = c("Low dose", "High dose")
  )
  tbl <- make_std_campsis_tbl(data, dataset = two_arm_dataset, scenarios = two_scenarios)

  expect_equal(campsis:::.auto_strata(tbl), c(ARM = "all"))
})
