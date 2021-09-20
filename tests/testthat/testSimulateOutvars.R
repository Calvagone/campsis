library(testthat)

context("Test the outvars argument of the simulate function")
seed <- 1

test_that("NULL outvars (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed)
  expect_equal(colnames(results1), c("ID","TIME","ARM","KA","CL","V2","V3","Q","S2","F","CP","OBS_CP","Y","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT"))
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  expect_equal(colnames(results2), c("ID","TIME","ARM","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT","CP","OBS_CP","Y"))
})

test_that("Not NULL outvars (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars="KA")
  expect_equal(colnames(results1), c("ID","TIME","ARM","KA","CL","V2","V3","Q","S2","F","CP","OBS_CP","Y","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT"))
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars="KA")
  expect_equal(colnames(results2), c("ID","TIME","ARM","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT","KA","CP","OBS_CP","Y"))
})

test_that("Not NULL outvars + DROP_OTHERS (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars=c("KA", "DROP_OTHERS"))
  expect_equal(colnames(results1), c("ID","TIME","ARM","KA"))
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars=c("KA", "DROP_OTHERS"))
  expect_equal(colnames(results2), c("ID","TIME","ARM","KA"))
})

test_that("Not NULL outvars from ERROR block + DROP_OTHERS (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4
  
  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))
  
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars=c("Y", "DROP_OTHERS"))
  expect_equal(colnames(results1), c("ID","TIME","ARM", "Y"))
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars=c("Y", "DROP_OTHERS"))
  expect_equal(colnames(results2), c("ID","TIME","ARM","Y"))
})

test_that("Covariates in outvars can be output well (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))
  dataset <- dataset %>% add(Covariate("WT", c(70, 71)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars=c("Y", "WT", "DROP_OTHERS"))
  expect_equal(colnames(results1), c("ID","TIME","ARM", "Y", "WT"))

  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars=c("Y", "WT", "DROP_OTHERS"))
  expect_equal(colnames(results2), c("ID","TIME","ARM","WT","Y"))
})

test_that("ETAs in outvars can be output well (RxODE/mrgsolve)", {
  model <- model_library$advan4_trans4

  dataset <- Dataset()
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=1)))

  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars=c("Y", "ETA_KA", "DROP_OTHERS"))
  expect_equal(colnames(results1), c("ID","TIME","ARM", "Y", "ETA_KA"))

  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars=c("Y", "ETA_KA", "DROP_OTHERS"))
  expect_equal(colnames(results2), c("ID","TIME","ARM","ETA_KA","Y"))
})
