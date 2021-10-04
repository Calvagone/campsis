
# setwd("C:/prj/campsis/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsis/tests/")
# testFolder <<- "C:/prj/campsis/tests/testthat/"

datasetInMemory <- function(dataset, model, seed, doseOnly=TRUE) {
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed)
  
  # Keep doses only
  if (doseOnly) {
    table <- table %>% dplyr::filter(EVID==1)
  }
}

#' Test there is no regression in the exported dataset.
#' 
#' @param dataset newly generated CAMPSIS dataset
#' @param model CAMPSIS model
#' @param seed seed that was used for export
#' @param doseOnly look only at the doses, i.e. EVID==1
#' @param filename reference file
#' @export
datasetRegressionTest <- function(dataset, model, seed, doseOnly=TRUE, filename) {
  dataset1 <- datasetInMemory(dataset=dataset, model=model, seed=seed, doseOnly=doseOnly)
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
    results2 <- results2 %>% dplyr::filter(TIME %in% dplyr::all_of(times))
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
    results <- results %>% dplyr::rename(output2="output")
    results <- results %>% dplyr::filter(output2 %in% output)
    results <- results %>% dplyr::select(-output2)
  }
  
  results1 <- results %>% dplyr::mutate_if(is.numeric, round, digits=2)
  suffix <- paste0(output, collapse="_") %>% tolower()
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, "_", suffix, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results1, file=file, sep=",", row.names=FALSE)
  }
  
  results2 <- read.csv(file=file) %>% tibble::as_tibble()
  expect_equal(results1, results2)
}
