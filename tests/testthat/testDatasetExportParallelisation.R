library(testthat)
library(tibble)

source(paste0("", "testUtils.R"))

context("Parallelisation of the dataset export")

test_that("Fixed covariates (including bootstrap) are correctly dealth with when parallelisation is enabled", {

  bws <- 70 + seq_len(10)
  
  dataset <- Dataset(10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(24)) %>%
    add(Covariate("BW", bws)) %>%
    add(Bootstrap(data=tibble(BS_ID=1:10, AGE=30 + BS_ID)))
  
  # Default setting (no parallelisation)
  settings1 <- Settings()
  
  table1 <- dataset %>% export(dest="RxODE", settings=settings1)
  
  # Parallelisation with 2 CPU's and slice of 5 subjects
  settings2 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=5))
  
  table2 <- dataset %>% export(dest="RxODE", settings=settings2)
  
  expect_equal(table1, table2)
  expect_equal(table1$BW %>% unique(), bws)
  expect_equal(table2$BW %>% unique(), bws)
  
  conf2 <- getSplittingConfiguration(dataset=dataset, hardware=settings2@hardware)
  expect_equal(conf2[[1]], tibble(subjects=5, arm_index=1, offset_within_arm=0, arm_offset=0))
  expect_equal(conf2[[2]], tibble(subjects=5, arm_index=1, offset_within_arm=5, arm_offset=0))
  
  # Parallelisation with 2 CPU's and slice of 3 subjects
  settings3 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=3))
  
  table3 <- dataset %>% export(dest="mrgsolve", settings=settings3)
  
  expect_equal(table1, table3)
  
  conf3 <- getSplittingConfiguration(dataset=dataset, hardware=settings3@hardware)
  expect_equal(conf3[[1]], tibble(subjects=3, arm_index=1, offset_within_arm=0, arm_offset=0))
  expect_equal(conf3[[2]], tibble(subjects=3, arm_index=1, offset_within_arm=3, arm_offset=0))
  expect_equal(conf3[[3]], tibble(subjects=3, arm_index=1, offset_within_arm=6, arm_offset=0))
  expect_equal(conf3[[4]], tibble(subjects=1, arm_index=1, offset_within_arm=9, arm_offset=0))
})


test_that("Arms are correctly dealth with when parallelisation is enabled", {

  bws_arm1 <- 70 + seq_len(10)
  bws_arm2 <- 100 + seq_len(15)
  
  arm1 <- Arm(subjects=10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", bws_arm1))
  
  arm2 <- Arm(subjects=15) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", bws_arm2))
  
  dataset <- Dataset() %>% add(c(arm1, arm2))
  
  # Default setting (no parallelisation)
  settings1 <- Settings()
  
  table1 <- dataset %>% export(dest="RxODE", settings=settings1)
  
  # Parallelisation with 2 CPU's and slice of 4 subjects
  settings2 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=4))
  
  table2 <- dataset %>% export(dest="RxODE", settings=settings2)
  
  expect_equal(table1, table2)
  expect_equal(table1$BW %>% unique(), c(bws_arm1, bws_arm2))
  expect_equal(table2$BW %>% unique(), c(bws_arm1, bws_arm2))
  
  conf2 <- getSplittingConfiguration(dataset=dataset, hardware=settings2@hardware)
  expect_equal(conf2[[1]], tibble(subjects=4, arm_index=1, offset_within_arm=0, arm_offset=0))
  expect_equal(conf2[[2]], tibble(subjects=4, arm_index=1, offset_within_arm=4, arm_offset=0))
  expect_equal(conf2[[3]], tibble(subjects=2, arm_index=1, offset_within_arm=8, arm_offset=0))
  expect_equal(conf2[[4]], tibble(subjects=4, arm_index=2, offset_within_arm=0, arm_offset=10))
  expect_equal(conf2[[5]], tibble(subjects=4, arm_index=2, offset_within_arm=4, arm_offset=10))
  expect_equal(conf2[[6]], tibble(subjects=4, arm_index=2, offset_within_arm=8, arm_offset=10))
  expect_equal(conf2[[7]], tibble(subjects=3, arm_index=2, offset_within_arm=12, arm_offset=10))
  
  # Parallelisation with 2 CPU's and slice of 15 subjects
  settings3 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=15))
  
  table3 <- dataset %>% export(dest="mrgsolve", settings=settings3)
  
  expect_equal(table1, table3)
  
  conf3 <- getSplittingConfiguration(dataset=dataset, hardware=settings3@hardware)
  expect_equal(conf3[[1]], tibble(subjects=10, arm_index=1, offset_within_arm=0, arm_offset=0))
  expect_equal(conf3[[2]], tibble(subjects=15, arm_index=2, offset_within_arm=0, arm_offset=10))
})

test_that("Dataset export may vary if parallelisation is used when covariates are sampled from distribution", {
  seed <- 1
  regFilenameDisabled <- "ds_export_parallel_disabled_wt_covariate"
  regFilenameEnabled <- "ds_export_parallel_enabled_wt_covariate"
  
  model <- model_suite$pk$`1cpt_fo`
  
  arm1 <- Arm(subjects=10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", NormalDistribution(mean=70, sd=10)))
  
  arm2 <- Arm(subjects=15) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", NormalDistribution(mean=70, sd=10)))
  
  dataset <- Dataset() %>% add(c(arm1, arm2))

  # Default setting (no parallelisation)
  settings1 <- Settings()
  setupPlanSequential()
  table1 <- dataset %>% export(dest="RxODE", settings=settings1, seed=seed)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameDisabled)
  
  # Parallelisation with 2 CPU's and slice of 15 subjects
  settings2 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=10))
  setupPlanDefault(settings2@hardware)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings2, dest="RxODE")
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings2, dest="mrgsolve")

  # Parallelisation with 3 CPU's and slice of 15 subjects
  settings3 <- Settings(Hardware(cpu=3, dataset_parallel=T, dataset_slice_size=10))
  setupPlanDefault(settings3@hardware)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings3, dest="RxODE")
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings3, dest="mrgsolve")
  
  # Back to sequential
  setupPlanSequential()
})
