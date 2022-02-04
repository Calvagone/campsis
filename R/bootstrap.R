#_______________________________________________________________________________
#----                         bootstrap class                               ----
#_______________________________________________________________________________

#' 
#' Bootstrap class.
#' 
#' @slot data data frame to be bootstrapped. Column 'BS_ID' is mandatory and
#' corresponds to the original ID from the bootstrap. It must be numeric and unique.
#' Other columns are covariates to be bootstrapped (row by row).
#' @slot replacement values can be reused or not, logical
#' @slot random values are drawn randomly, logical
#' @slot output_id tell CAMPSIS if 'BS_ID' must be output or not, logical
#' @export
setClass(
  "bootstrap",
  representation(
    data = "data.frame",
    replacement = "logical",
    random = "logical",
    output_id = "logical"
  ),
  contains="pmx_element",
  validity=function(object) {
    check1 <- NULL
    if (object@data %>% nrow() > 0) {
      if (!("BS_ID" %in% c(colnames(object@data)))) {
        check1 <- "Column 'BS_ID' is mandatory in bootstrap" 
      }
    }
    check2 <- expectOneForAll(object, c("replacement", "random", "output_id"))
    return(c(check1, check2))
  }
)

setMethod("getName", signature = c("bootstrap"), definition = function(x) {
  return("BOOTSTRAP")
})

#'
#' Create a bootstrap object.
#'
#' @slot data data frame to be bootstrapped. Column 'BS_ID' is mandatory and
#' corresponds to the original ID from the bootstrap. It must be numeric and unique.
#' Other columns are covariates to be bootstrapped (row by row).
#' @param replacement values can be reused or not, logical
#' @param random values are drawn randomly, logical
#' @param output_id tell CAMPSIS if 'BS_ID' must be output or not, logical
#' @return a covariate
#' @export
Bootstrap <- function(data, replacement=FALSE, random=FALSE, output_id=FALSE) {
  return(new("bootstrap", data=data, replacement=replacement, random=random, output_id=output_id))
}

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________


#' @rdname sample
setMethod("sample", signature = c("bootstrap", "integer"), definition = function(object, n) {
  data <- object@data
  replacement <- object@replacement
  random <- object@random
  output_id <- object@output_id
  nData <- nrow(data)
  
  if (nData == 0) {
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
  
  covariableNames <- colnames(data)
  if (!output_id) {
    covariableNames <- covariableNames[!(covariableNames %in% "BS_ID")]
  }
  return(covariableNames %>%
           purrr::map(~Covariate(name=.x, distribution=FixedDistribution(data %>% dplyr::pull(.x)))))
})
