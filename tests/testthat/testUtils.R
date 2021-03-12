
# setwd("C:/prj/pmxsim/")
# roxygen2::roxygenise()
# setwd("C:/prj/pmxsim/tests/")
# testFolder <<- "C:/prj/pmxsim/tests/testthat/"
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# reticulate::py_config()
# version <- pharmpy["__version__"]

datasetInMemory <- function(dataset, model, seed, doseOnly=TRUE) {
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed)
  
  # Keep doses only
  if (doseOnly) {
    table <- table %>% dplyr::filter(EVID==1)
  }
}

regressionTest <- function(dataset, model, seed, doseOnly=TRUE, filename) {
  dataset1 <- datasetInMemory(dataset=dataset, model=model, seed=seed, doseOnly=doseOnly)
  dataset1 <- dataset1 %>% dplyr::mutate_if(is.numeric, round, digits=6)
  
  file <- paste0(testFolder, "non_regression/", filename)
  
  if (overwriteNonRegressionFiles) {
    write.table(dataset1, file=file, sep=",", row.names=FALSE)
  }
  
  dataset2 <- read.csv(file=file)
  expect_equal(dataset1, dataset2)
}