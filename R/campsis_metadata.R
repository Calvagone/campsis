#'
#' Campsis metadata class.
#'
#' @slot dataset Campsis dataset object
#' @slot dest destination simulation engine
#' @slot scenarios Campsis scenarios object
#' @slot outvars character vector of output variable names
#' @slot outfun collection of output functions
#' @slot replicates number of replicates
#' @export
setClass(
  "campsis_metadata",
  representation(
    dataset = "dataset",
    dest = "character",
    scenarios = "scenarios",
    outvars = "character",
    outfun = "outfun",
    replicates = "integer"
  )
)

new_campsis_tbl <- function(x = tibble(), metadata) {
  stopifnot(tibble::is_tibble(x))

  outfun <- metadata@outfun
  class(x) <- c(outfun@cls, class(x))
  attr(x, "metadata") <- metadata
  x
}

#' Restore a campsis_tbl object
#'
#' Internal vctrs method to restore the custom `campsis_tbl` class attributes,
#' specifically copying over the S4 metadata object, after a vector operation.
#'
#' @param x A tibble or vector to restore.
#' @param to The original `campsis_tbl` object to restore to.
#' @param ... Additional arguments passed to methods.
#'
#' @return A restored `campsis_tbl` object with appropriate metadata and classes.
#' @export
#' @importFrom vctrs vec_restore
#' @method vec_restore campsis_tbl
#' @keywords internal
vec_restore.campsis_tbl <- function(x, to, ...) {
  metadata <- attr(to, "metadata")
  # Check if metadata exists on the 'to' template object
  if (!is.null(metadata)) {
    attr(x, "metadata") <- metadata
    outfun <- metadata@outfun
    class(x) <- c(outfun@cls, class(tibble::tibble()))
  } else {
    # Fallback to standard tibble if metadata was stripped upstream
    class(x) <- class(tibble::tibble())
  }
  return(x)
}
