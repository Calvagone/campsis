library(testthat)

context("Test the simulate method with minimalist examples")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a minimalist model, no parameters/IIV/error model"), {
  regFilename <- "minimalist_model"
  model <- suppressWarnings(read.campsis(paste0(testFolder, "models/", regFilename)))
  
  dataset <- Dataset() %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    outputRegressionTest(results, output="A_CENTRAL", filename=regFilename),
    spaghettiPlot(results, "A_CENTRAL")
  )
  campsisTest(simulation, test, env=environment())
})