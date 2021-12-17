#_______________________________________________________________________________
#----                       dataset_summary class                           ----
#_______________________________________________________________________________

setClass(
  "dataset_summary",
  representation(
    covariate_names = "character",
    iov_names = "character",
    event_covariate_names = "character",
    occ_names = "character"
  )
)

#' 
#' Create a dataset summary (internal method).
#' 
#' @return dataset summary
#' @keywords internal
DatasetSummary <- function() {
  return(new("dataset_summary"))
}

#' 
#' Convert dataset to dataset summary (internal method).
#' 
#' @return dataset summary
#' @keywords internal
toDatasetSummary <- function(dataset) {
  summary <- DatasetSummary()
  summary@iov_names <- dataset %>% getIOVNames()
  summary@covariate_names <- dataset %>% getCovariateNames()
  summary@event_covariate_names <- dataset %>% getTimeVaryingCovariateNames()
  summary@occ_names <- dataset %>% getOccasionNames()
  return(summary)
}
