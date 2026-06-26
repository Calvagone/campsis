
# Shared fixtures and helpers for S3 plot method tests.
# Sourced by testSpaghettiPlot.R and testShadedPlot.R.

#_______________________________________________________________________________
#----                        tbl factory                                    ----
#_______________________________________________________________________________

#' Build a minimal std_campsis_tbl from a plain tibble + metadata components.
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

#_______________________________________________________________________________
#----                        dataset fixtures                               ----
#_______________________________________________________________________________

# Single arm.
single_arm_dataset <- Dataset(subjects = 3) %>%
  add(Bolus(time = 0, amount = 100, compartment = 1)) %>%
  add(Observations(times = c(0, 6, 12, 18, 24)))

# Two arms with distinct labels.
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

# Two named scenarios.
two_scenarios <- Scenarios() %>%
  add(Scenario("Low dose")) %>%
  add(Scenario("High dose"))

#_______________________________________________________________________________
#----                        data factory                                   ----
#_______________________________________________________________________________

#' Build a deterministic tibble with 3 subjects × 5 time points.
#'
#' @param arms     optional character vector length 3, one label per subject
#' @param scenarios optional character vector length 3, one label per subject
#' @return a tibble with columns ID, TIME, CONC, and optionally ARM / SCENARIO
make_data <- function(arms = NULL, scenarios = NULL) {
  n_subjects <- 3
  times      <- c(0, 6, 12, 18, 24)
  n_times    <- length(times)

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
