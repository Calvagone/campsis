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

test_that("Time-varying covariates are correctly dealth with when parallelisation is enabled", {
  
  db <- dplyr::bind_rows(
    data.frame(ID=1, TIME=c(0, 24), VALUE=c(70, 71)),
    data.frame(ID=2, TIME=c(0, 24), VALUE=c(72, 73)),
    data.frame(ID=3, TIME=c(0, 24), VALUE=c(74, 75)),
    data.frame(ID=4, TIME=c(0, 24), VALUE=c(76, 77)),
    data.frame(ID=5, TIME=c(0, 24), VALUE=c(78, 79)),
    data.frame(ID=6, TIME=c(0, 24), VALUE=c(80, 81)),
    data.frame(ID=7, TIME=c(0, 24), VALUE=c(82, 83)),
    data.frame(ID=8, TIME=c(0, 24), VALUE=c(84, 85)),
    data.frame(ID=9, TIME=c(0, 24), VALUE=c(86, 87)),
    data.frame(ID=10, TIME=c(0, 24), VALUE=c(88, 89))
  )
  
  dataset <- Dataset(10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(24)) %>%
    add(TimeVaryingCovariate("BW", db))
  
  # Default setting (no parallelisation)
  settings1 <- Settings()
  
  table1 <- dataset %>% export(dest="RxODE", settings=settings1)
  
  # Parallelisation with 2 CPU's and slice of 5 subjects
  settings2 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=5))
  
  table2 <- dataset %>% export(dest="RxODE", settings=settings2)
  
  expect_equal(table1, table2)
  expect_equal(table1$BW %>% unique(), db$VALUE %>% unique())
  expect_equal(table2$BW %>% unique(), db$VALUE %>% unique())

  # Parallelisation with 2 CPU's and slice of 3 subjects
  settings3 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=3))
  
  table3 <- dataset %>% export(dest="mrgsolve", settings=settings3)
  expect_equal(table1, table3)
  
  # Same kind of tests but with 2 arms
  arm1 <- Arm(subjects=5) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(24)) %>%
    add(TimeVaryingCovariate("BW", db %>% dplyr::filter(ID %in% (1:5))))
  
  arm2 <- Arm(subjects=5) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(24)) %>%
    add(TimeVaryingCovariate("BW", db %>% dplyr::filter(ID %in% (6:10)) %>% dplyr::mutate(ID=ID - 5)))
  
  dataset_ <- Dataset() %>%
    add(c(arm1, arm2))
  
  table3b <- dataset_ %>% export(dest="RxODE", settings=settings3)
  
  # Identical tables except ARM column
  expect_equal(table3 %>% dplyr::select(-ARM), table3b %>% dplyr::select(-ARM))
  expect_equal(table3$ARM, rep(0, 3*10))
  expect_equal(table3b$ARM, c(rep(1, 3*5), rep(2, 3*5)))
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

test_that("Dataset export may vary if parallelisation is used when covariates/IOV are sampled from distribution", {
  seed <- 1
  regFilenameDisabled <- "ds_export_parallel_disabled_wt_iov_covariate"
  regFilenameEnabled <- "ds_export_parallel_enabled_wt_iov_covariate"
  
  model <- model_suite$pk$`1cpt_fo`
  
  arm1 <- Arm(subjects=10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", NormalDistribution(mean=50, sd=10))) %>%
    add(IOV("IOV", NormalDistribution(mean=0, sd=1)))
  
  arm2 <- Arm(subjects=15) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Covariate("BW", NormalDistribution(mean=100, sd=10))) %>%
    add(IOV("IOV", NormalDistribution(mean=0, sd=10)))
  
  dataset <- Dataset() %>% add(c(arm1, arm2))

  # Default setting (no parallelisation)
  settings1 <- Settings()
  setupPlanSequential()
  table1 <- dataset %>% export(dest="RxODE", settings=settings1, seed=seed)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameDisabled)
  
  # Parallelisation with 2 CPU's and slice of 10 subjects
  settings2 <- Settings(Hardware(cpu=2, dataset_parallel=T, dataset_slice_size=10))
  setupPlanDefault(settings2@hardware)
  table2 <- dataset %>% export(dest="RxODE", settings=settings2, seed=seed)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings2, dest="RxODE")
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings2, dest="mrgsolve")

  # Parallelisation with 3 CPU's and slice of 10 subjects
  settings3 <- Settings(Hardware(cpu=3, dataset_parallel=T, dataset_slice_size=10))
  setupPlanDefault(settings3@hardware)
  table3 <- dataset %>% export(dest="RxODE", settings=settings3, seed=seed)
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings3, dest="RxODE")
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilenameEnabled, settings=settings3, dest="mrgsolve")
  
  # Table 2 strictly identical to table 3 (since number of CPU's does not matter)
  expect_equal(table2, table3)
  
  # Table 1 equals to table 2/3 if you remove the variables that are sampled from a distribution
  expect_equal(table1 %>% dplyr::select(-dplyr::all_of(c("IOV", "BW"))),
               table3 %>% dplyr::select(-dplyr::all_of(c("IOV", "BW"))))
  
  # Back to sequential
  setupPlanSequential()
})
