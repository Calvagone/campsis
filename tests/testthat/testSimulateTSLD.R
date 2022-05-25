library(testthat)

context("Simulate models that depend on TSLD or TDOS")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Weibull model simulation works as expected (rxode2/mrgsolve)", {
  regFilename <- "weibull_model"
  model <- suppressWarnings(read.campsis(paste0(testFolder, "models/", regFilename)))
  config <- DatasetConfig(exportTDOS=TRUE)

  ds <- Dataset(3) %>%
    add(Bolus(time=c(0, 48), amount=100)) %>%
    add(Observations(seq(0, 96, by=4))) %>%
    add(Covariate("DOSE", 100)) %>%
    add(config)

  nocbvars <- "TDOS" # This is needed for mrgsolve because TDOS is considered as a time-varying covariate
  table1 <- ds %>% export(dest="rxode2", config=config, nocb=FALSE, nocbvars=nocbvars)
  expect_equal(table1$TDOS %>% unique(), c(0, 48))

  table2 <- ds %>% export(dest="mrgsolve", config=config, nocb=TRUE, nocbvars=nocbvars)
  expect_equal(table2$TDOS %>% unique(), c(0, 48))

  results1 <- simulate(model=model, dataset=ds, dest="rxode2", seed=seed, nocbvars=nocbvars)
  spaghettiPlot(results1, "CONC")
  outputRegressionTest(results1, output="CONC", filename=regFilename)

  results2 <- simulate(model=model, dataset=ds, dest="mrgsolve", seed=seed, nocbvars=nocbvars)
  spaghettiPlot(results2, "CONC")
  outputRegressionTest(results2, output="CONC", filename=regFilename)

  # Now export TSLD as well
  config <- DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE)
  ds <- ds %>% add(config)

  table1 <- ds %>% export(dest="rxode2", config=config, nocb=FALSE, nocbvars=nocbvars)
  table1Subj1 <- table1 %>% dplyr::filter(ID==1)
  expect_equal(table1Subj1$TSLD, c(c(0,0,4,8,12,16,20,24,28,32,36,40,44,0), c(0,4,8,12,16,20,24,28,32,36,40,44,48)))

  table2 <- ds %>% export(dest="mrgsolve", config=config, nocb=TRUE, nocbvars=nocbvars)
  table2Subj1 <- table2 %>% dplyr::filter(ID==1)
  expect_equal(table2Subj1$TSLD, c(c(0,0,0,4,8,12,16,20,24,28,32,36,40,44), c(0,0,4,8,12,16,20,24,28,32,36,40,44)))
})
