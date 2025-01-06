
# setwd("C:/prj/campsis/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsis/tests/")
# testFolder <- "C:/prj/campsis/tests/testthat/"

overwriteNonRegressionFiles <- FALSE
testFolder <- ""
testEngines <- c("rxode2", "mrgsolve")

datasetInMemory <- function(dataset, model=NULL, seed, doseOnly=TRUE, settings, dest) {
  table <- dataset %>% export(dest=dest, model=model, seed=seed, settings=settings)
  
  # Keep doses only
  if (doseOnly) {
    table <- table %>% dplyr::filter(EVID==1)
  }
  return(table)
}

#' Test there is no regression in the exported dataset.
#' 
#' @param dataset newly generated CAMPSIS dataset
#' @param model CAMPSIS model
#' @param seed seed that was used for export
#' @param doseOnly look only at the doses, i.e. EVID==1
#' @param filename reference file
#' @param settings export settings
#' @param dest destination engine
#' @export
datasetRegressionTest <- function(dataset, model=NULL, seed, doseOnly=TRUE, filename, settings=Settings(), dest="RxODE") {
  dataset1 <- datasetInMemory(dataset=dataset, model=model, seed=seed, doseOnly=doseOnly, settings=settings, dest=dest)
  dataset1 <- dataset1 %>% dplyr::mutate_if(is.numeric, round, digits=6)
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(dataset1, file=file, sep=",", row.names=FALSE)
  }
  
  dataset2 <- read.csv(file=file) %>% tibble::as_tibble()
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
outputRegressionTest <- function(results, output, filename, times=NULL) {
  selectedColumns <- unique(c("ID", "TIME", output))
  results1 <- results %>% dplyr::select(dplyr::all_of(selectedColumns)) %>% dplyr::mutate_if(is.numeric, round, digits=2)
  suffix <- paste0(output, collapse="_") %>% tolower()
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, "_", suffix, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results1, file=file, sep=",", row.names=FALSE)
  }

  results2 <- read.csv(file=file) %>% tibble::as_tibble()
  if (!is.null(times)) {
    results2 <- results2 %>% dplyr::filter(TIME %in% times)
  }
  expect_equal(results1, results2)
}

#' Test there is no regression in the simulated output.
#' 
#' @param results newly generated results
#' @param output variables to compare
#' @param filename reference file (output will be appended automatically)
#' @export
vpcOutputRegressionTest <- function(results, output, filename) {
  selectedColumns <- unique(c("replicate", "TIME", "metric", "value"))
  if ("output" %in% colnames(results)) {
    results <- results %>%
      dplyr::rename(output2="output") %>%
      dplyr::filter(output2 %in% output) %>%
      dplyr::select(-output2)
  }
  
  results1 <- results %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, round, digits=2) %>%
    dplyr::arrange(replicate, TIME, metric)
  suffix <- paste0(output, collapse="_") %>% tolower()
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, "_", suffix, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results1, file=file, sep=",", row.names=FALSE)
  }
  
  results2 <- read.csv(file=file) %>% tibble::as_tibble()
  
  # Re-arrange data frame for backwards compatibility
  results2 <- results2 %>%
    tibble::as_tibble() %>%
    dplyr::arrange(replicate, TIME, metric)
  
  expect_equal(results1, results2)
}

noEngineInstalled <- function() {
  cond1 <- engineInstalled("RxODE")
  cond2 <- engineInstalled("rxode2")
  cond3 <- engineInstalled("mrgsolve")
  return(!(cond1 || cond2 || cond3))
}

engineInstalled <- function(name) {
  return(find.package(name, quiet=TRUE) %>% length() > 0)
}

campsisTest <- function(simulation, test, env) {
  # Iteration over all test engines to be tested
  for (testEngine in testEngines) {
    env$destEngine <-  testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engineInstalled(testEngine)) {
      env$results <- eval(simulation, envir=env)
      eval(test, envir=env)
    }
  }
}

getTestName <- function(name) {
  return(paste0(name, " (", paste0(testEngines, collapse="/"), ")"))
}

skipTests <- function(name, default) {
  option <- getCampsisOption()
  if (is.null(option)) {
    return(default)
  } else {
    value <- option[[name]]
    if (is.null(value)) {
      return(default)
    } else {
      return(value)
    }
  }
}

skipLongTests <- function() {
  # On CRAN, default value is TRUE
  # FALSE otherwise
  return(skipTests(name="SKIP_LONG_TESTS", default=onCran()))
}

skipVeryLongTests <- function() {
  return(skipTests(name="SKIP_VERY_LONG_TESTS", default=TRUE))
}

isMacOs <- function() {
  # return windows, darwin, linux or sunos
  systemOs <- tolower(Sys.info()[["sysname"]])
  return(systemOs=="darwin")
} 

skipVdiffrTests <- function() {
  # On mac, default value is TRUE (problems in vdiffr tests, see CI)
  # FALSE otherwise
  return(skipTests(name="SKIP_VDIFFR_TESTS", default=ifelse(isMacOs(), TRUE, FALSE)))
}

getCampsisOption <- function() {
  return(getOption("campsis.options"))
}


