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
#' @return a dataset summary
#' @keywords internal
DatasetSummary <- function() {
  return(new("dataset_summary"))
}

#' 
#' Convert dataset to dataset summary (internal method).
#' 
#' @return a dataset summary
#' @keywords internal
toDatasetSummary <- function(dataset) {
  summary <- DatasetSummary()
  summary@iov_names <- dataset %>% getIOVs() %>% getNames()
  
  arm <- dataset@arms %>% default()
  bootstrap <- arm@bootstrap
  
  summary@covariate_names <- c(dataset %>% getCovariates() %>% getNames(), bootstrap %>% getNames())
  summary@event_covariate_names <- dataset %>% getEventCovariates() %>% getNames()
  summary@occ_names <- dataset %>% getOccasions() %>% getNames()
  return(summary)
}
