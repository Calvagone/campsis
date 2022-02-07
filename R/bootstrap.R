#_______________________________________________________________________________
#----                         bootstrap class                               ----
#_______________________________________________________________________________

#' 
#' Bootstrap class.
#' 
#' @slot data data frame to be bootstrapped. Column 'BS_ID' is mandatory and
#' corresponds to the original row ID from the bootstrap. It must be numeric and unique.
#' Other columns are covariates to be bootstrapped (row by row).
#' @slot replacement values can be reused or not, logical
#' @slot random values are drawn randomly, logical
#' @slot export_id tell CAMPSIS if 'BS_ID' must be exported into the dataset, logical
#' @export
setClass(
  "bootstrap",
  representation(
    data = "data.frame",
    replacement = "logical",
    random = "logical",
    export_id = "logical"
  ),
  contains="pmx_element",
  validity=function(object) {
    check1a <- NULL
    check1b <- NULL
    check1c <- NULL
    check1d <- NULL
    
    # Check BS_ID column
    if (!("BS_ID" %in% c(colnames(object@data)))) {
      check1a <- "Column 'BS_ID' is mandatory in bootstrap" 
    } else {
      if (!is.numeric(object@data$BS_ID)) {
        check1b <- "Column 'BS_ID' must be numeric" 
      } else {
        if (!all(object@data$BS_ID %% 1==0)) {
          check1c <- "Column 'BS_ID' must contain integers only" 
        }
      }
      if (unique(object@data$BS_ID) %>% length() != object@data$BS_ID %>% length()) {
        check1d <- "Column 'BS_ID' contains duplicates" 
      }
    }
    
    # Check covariable columns
    check2 <- NULL
    covariableNames <- object %>% getNames()
    covariableNames <- covariableNames[!(covariableNames %in% "BS_ID")]
    check2_lgl <- !covariableNames %>% purrr::map_lgl(.f=~object@data[,.x] %>% is.numeric())
    if (any(check2_lgl)) {
      check2 <- paste0("Column(s) ", paste0(covariableNames[check2_lgl], collapse=","), " are not numeric")
    }
    
    # Check other slots
    check3 <- expectOneForAll(object, c("replacement", "random", "export_id"))
    
    return(c(check1a, check1b, check1c, check1d,  check2, check3))
  }
)

setMethod("getName", signature = c("bootstrap"), definition = function(x) {
  return("BOOTSTRAP")
})

#'
#' Create a bootstrap object.
#'
#' @param data data frame to be bootstrapped. It must have a unique identifier column
#' named according to the specified argument 'id' (default value is 'BS_ID').
#' Other columns are covariates to bootstrap. They must all be numeric.
#' Whatever the configuration of the bootstrap, these covariates are always read
#' row by row and belong to a same individual. 
#' @param replacement values can be reused or not when drawn, logical
#' @param random values are drawn randomly, logical
#' @param export_id tell CAMPSIS if the identifier 'BS_ID' must be output or not, logical
#' @return a bootstrap object
#' @importFrom assertthat assert_that
#' @importFrom dplyr rename
#' @export
Bootstrap <- function(data, id="BS_ID", replacement=FALSE, random=FALSE, export_id=FALSE) {
  assertthat::assert_that(is(data, "data.frame"), msg="data not a data frame")
  assertthat::assert_that(id %in% colnames(data), msg=paste0("Unique identifier '", id, "' not part of data"))
  if (id != "BS_ID") {
    data <- data %>% dplyr::rename(BS_ID=id)
  }
  return(new("bootstrap", data=data, replacement=replacement, random=random, export_id=export_id))
}

#'
#' Is the given bootstrap empty.
#'
#' @param object bootstrap object
#' @return logical value TRUE/FALSE
#' @importFrom assertthat assert_that
#' @keywords internal
isEmptyBootstrap <- function(object) {
  assertthat::assert_that(is(object, "bootstrap"), msg="Not a bootstrap object")
  data <- object@data
  nData <- nrow(data)
  return(nData==0)
}

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

setMethod("getNames", signature=c("bootstrap"), definition=function(object) {
  data <- object@data
  covariableNames <- colnames(data)
  export_id <- object@export_id
  if (!export_id) {
    covariableNames <- covariableNames[!(covariableNames %in% "BS_ID")]
  }
  return(covariableNames)
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________


#' @rdname sample
setMethod("sample", signature = c("bootstrap", "integer"), definition = function(object, n) {
  data <- object@data
  replacement <- object@replacement
  random <- object@random
  nData <- nrow(data)
  
  if (isEmptyBootstrap(object)) {
    return(list())
  }
  
  if (random) {
    # Cases random=TRUE and replacement=TRUE/FALSE
    data <- data[sample.int(n=nData, size=n, replace=replacement),]
  } else if (replacement) {
    # Case random=FALSE and replacement=TRUE
    data <- data[rep(seq_len(nData), length.out=n),]
  } else {
    # Case random=FALSE, replacement=FALSE
    # Do nothing
  }
  return(object %>% getNames() %>%
           purrr::map(~Covariate(name=.x, distribution=FixedDistribution(data %>% dplyr::pull(.x)))))
})
