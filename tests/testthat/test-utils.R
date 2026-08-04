OVERWRITE_NON_REG_FILES <- FALSE
TEST_ENGINES <- c("mrgsolve")

dataset_in_memory <- function(dataset, model = NULL, seed, doseOnly = TRUE, settings, dest) {
  table <- dataset %>% export(dest = dest, model = model, seed = seed, settings = settings)

  # Keep doses only
  if (doseOnly) {
    table <- table %>% dplyr::filter(EVID == 1)
  }

  # Convert CMT column
  if (!is.null(model)) {
    table <- table %>% dplyr::mutate(CMT = as.integer(CMT))
  }

  return(table)
}

strip_metadata <- function(x) {
  # Revert class back to standard tibble components
  class(x) <- c("tbl_df", "tbl", "data.frame")

  # Delete the attribute
  attr(x, "metadata") <- NULL

  return(x)
}

#' Test there is no regression in the exported dataset.
#'
#' @param dataset newly generated Campsis dataset
#' @param model Campsis model
#' @param seed seed that was used for export
#' @param doseOnly look only at the doses, i.e. EVID==1
#' @param filename reference file
#' @param settings export settings
#' @param dest destination engine
#' @export
dataset_regression_test <- function(
  dataset,
  model = NULL,
  seed,
  doseOnly = TRUE,
  filename,
  settings = Settings(),
  dest = "rxode2"
) {
  dataset1 <- dataset_in_memory(
    dataset = dataset,
    model = model,
    seed = seed,
    doseOnly = doseOnly,
    settings = settings,
    dest = dest
  )
  dataset1 <- dataset1 %>% dplyr::mutate_if(is.numeric, round, digits = 6)

  file <- file.path(getwd(), test_path(), "non_regression", paste0(filename, ".csv"))

  if (OVERWRITE_NON_REG_FILES) {
    write.table(dataset1, file = file, sep = ",", row.names = FALSE)
  }

  dataset2 <- read.csv(file = file) %>%
    tibble::as_tibble()

  # When model is not provided, export always returns CMT as character
  if (is.null(model) && "CMT" %in% colnames(dataset2)) {
    dataset2 <- dataset2 %>%
      dplyr::mutate(CMT = as.character(CMT))
  }

  expect_equal(dataset1, dataset2)
}

#' Test there is no regression in the simulated output.
#'
#' @param results newly generated results
#' @param output variables to compare
#' @param filename reference file (output will be appended automatically)
#' @param times filter reference results on specific times, NULL by default
#' @importFrom tibble as_tibble
#' @export
output_regression_test <- function(results, output, filename, times = NULL) {
  selectedColumns <- unique(c("ID", "TIME", output))
  results1 <- results %>%
    strip_metadata() %>%
    dplyr::select(dplyr::all_of(selectedColumns)) %>%
    dplyr::mutate_if(is.numeric, round, digits = 2)
  suffix <- paste0(output, collapse = "_") %>% tolower()

  file <- file.path(getwd(), test_path(), "non_regression", paste0(filename, "_", suffix, ".csv"))

  if (OVERWRITE_NON_REG_FILES) {
    write.table(results1, file = file, sep = ",", row.names = FALSE)
  }

  results2 <- read.csv(file = file) %>% tibble::as_tibble()
  if (!is.null(times)) {
    results2 <- results2 %>%
      dplyr::filter(TIME %in% times)
  }
  expect_equal(results1, results2)
}

#' Test there is no regression in the simulated output.
#'
#' @param results newly generated results
#' @param output variables to compare
#' @param filename reference file (output will be appended automatically)
#' @export
vpc_output_regression_test <- function(results, output, filename) {
  results <- results %>%
    strip_metadata() %>%
    dplyr::filter(.data$variable %in% output)

  results1 <- results %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    dplyr::arrange(replicate, TIME, metric)
  suffix <- paste0(output, collapse = "_") %>% tolower()

  file <- file.path(getwd(), test_path(), "non_regression", paste0(filename, "_", suffix, ".csv"))

  if (OVERWRITE_NON_REG_FILES) {
    write.table(results1, file = file, sep = ",", row.names = FALSE)
  }

  results2 <- read.csv(file = file) %>% tibble::as_tibble()

  # Re-arrange data frame for backwards compatibility
  results2 <- results2 %>%
    tibble::as_tibble() %>%
    dplyr::arrange(replicate, TIME, metric)

  expect_equal(results1, results2)
}

no_engine_installed <- function() {
  cond1 <- engine_installed("rxode2")
  cond2 <- engine_installed("mrgsolve")
  return(!(cond1 || cond2))
}

engine_installed <- function(name) {
  return(find.package(name, quiet = TRUE) %>% length() > 0)
}

campsis_test <- function(simulation, test, env) {
  # Iteration over all test engines to be tested
  for (testEngine in TEST_ENGINES) {
    env$destEngine <- testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engine_installed(testEngine)) {
      env$results <- eval(simulation, envir = env)
      eval(test, envir = env)
    }
  }
}

skip_long_tests <- function() {
  # On CRAN, default value is TRUE
  # FALSE otherwise
  return(get_campsis_option(name = "SKIP_LONG_TESTS", default = on_cran()))
}

skip_very_long_tests <- function() {
  return(get_campsis_option(name = "SKIP_VERY_LONG_TESTS", default = TRUE))
}

is_mac_os <- function() {
  # return windows, darwin, linux or sunos
  systemOs <- tolower(Sys.info()[["sysname"]])
  return(systemOs == "darwin")
}

skip_vdiffr_tests <- function() {
  # On mac, default value is TRUE (problems in vdiffr tests, see CI)
  # FALSE otherwise
  return(get_campsis_option(name = "SKIP_VDIFFR_TESTS", default = ifelse(is_mac_os(), TRUE, FALSE)))
}

convert_campsis_test <- function(env = parent.frame(), debug_engine = "mrgsolve") {
  if (!exists("simulation", envir = env) || !exists("test", envir = env)) {
    stop("Could not find 'simulation' or 'test' expressions in the provided environment.")
  }

  # Get the expressions from the specified environment
  sim_expr <- get("simulation", envir = env)
  test_expr <- get("test", envir = env)

  # Modify the simulation call
  sim_call <- sim_expr[[1]]
  sim_call$dest <- debug_engine
  sim_text <- paste0("results <- ", deparse1(sim_call))

  # Extract all lines from the test expression
  test_text_lines <- vapply(as.list(test_expr), deparse1, character(1))

  # Combine and return
  final_script <- c(sim_text, test_text_lines)
  cat(paste(final_script, collapse = "\n"))
}
