library(testthat)

context("Simulate models that depend on TSLD or TDOS")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Weibull model simulation works as expected"), {
  regFilename <- "weibull_model"
  model <- suppressWarnings(read.campsis(paste0(testFolder, "models/", regFilename)))
  config <- DatasetConfig(exportTDOS=TRUE)
  
  ds <- Dataset(3) %>%
    add(Bolus(time=c(0, 48), amount=100)) %>%
    add(Observations(seq(0, 96, by=4))) %>%
    add(Covariate("DOSE", 100)) %>%
    add(config)
  
  nocbvars <- "TDOS" # This is needed for mrgsolve because TDOS is considered as a time-varying covariate
  
  # Export TDOS only
  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, seed=seed, nocbvars=nocbvars))
  test <- expression(
    outputRegressionTest(results, output="CONC", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
  

  # Now export TSLD as well
  config <- DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE)
  ds <- ds %>% add(config)
  
  # Not really a simulation here...
  simulation <- expression(
    nocb <- if (destEngine %in% c("RxODE", "rxode2")) {FALSE} else {TRUE},
    ds %>% export(dest=destEngine, config=config, nocb=nocb, nocbvars=nocbvars)
  )
  test <- expression(
    results_ <- results %>% dplyr::filter(ID==1),
    expected <- if (destEngine %in% c("RxODE", "rxode2")) {
      c(c(0,0,4,8,12,16,20,24,28,32,36,40,44,0), c(0,4,8,12,16,20,24,28,32,36,40,44,48))
    } else {
      c(c(0,0,0,4,8,12,16,20,24,28,32,36,40,44), c(0,0,4,8,12,16,20,24,28,32,36,40,44))
    },
    expect_equal(results_$TSLD, expected)
  )
  campsisTest(simulation, test, env=environment())
})
