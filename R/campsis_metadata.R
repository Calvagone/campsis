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

#' @export
vec_restore.campsis_tbl <- function(x, to, ...) {
  # Copy the S4 object safely over to the sliced tibble
  attr(x, "metadata") <- attr(to, "metadata")
  outfun <- attr(to, "metadata")@outfun
  class(x) <- c(outfun@cls, class(tibble::tibble()))
  x
}