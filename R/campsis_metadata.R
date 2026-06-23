setClass(
  "campsis_metadata",
  representation(
    dataset="dataset",
    dest="character",
    scenarios="scenarios",
    outvars="character",
    outfun="outfun",
    replicates="integer"
  )
)

new_campsis_tbl <- function(x = tibble(), metadata) {
  stopifnot(tibble::is_tibble(x))
  
  class(x) <- c("campsis_tbl", class(x))
  attr(x, "metadata") <- metadata
  x
}

#' @export
vec_restore.campsis_tbl <- function(x, to, ...) {
  # Copy the S4 object safely over to the sliced tibble
  attr(x, "metadata") <- attr(to, "metadata")
  
  class(x) <- c("campsis_tbl", class(tibble()))
  x
}