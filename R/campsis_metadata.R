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
#' @method vec_restore campsis_tbl
#' @keywords internal
vec_restore.campsis_tbl <- function(x, to, ...) {
  # Copy the S4 object safely over to the sliced tibble
  attr(x, "metadata") <- attr(to, "metadata")
  outfun <- attr(to, "metadata")@outfun
  class(x) <- c(outfun@cls, class(tibble::tibble()))
  x
}
