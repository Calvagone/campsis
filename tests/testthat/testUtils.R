
# setwd("C:/prj/pmxmod/")
# roxygen2::roxygenise()
# setwd("C:/prj/pmxmod/tests/")
# testFolder <<- "C:/prj/pmxmod/tests/testthat/"
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# reticulate::py_config()
# version <- pharmpy["__version__"]

toFile <- function(code, path) {
  fileConn <- file(path)
  writeLines(code, fileConn)
  close(fileConn)
}

loadNonRegressionFile <- function(path) {
  return(read.table(file=path, sep="@")[,1])
}
