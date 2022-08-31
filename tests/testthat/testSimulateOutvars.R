library(testthat)

context("Test the outvars argument of the simulate function")
seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("NULL outvars"), {
  model <- model_suite$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expected <- if (destEngine %in% c("RxODE", "rxode2")) {
      c("ID","TIME","ARM","KA","CL","V2","V3","Q","S2","F","CP","OBS_CP","Y","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT")
    } else {
      c("ID","TIME","ARM","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT","CP","OBS_CP","Y")
    },
    expect_true(all(expected %in% colnames(results)))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Not NULL outvars"), {
  model <- model_suite$nonmem$advan4_trans4
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars="KA"))
  test <- expression(
    expected <- if (destEngine %in% c("RxODE", "rxode2")) {
      c("ID","TIME","ARM","KA","CL","V2","V3","Q","S2","F","CP","OBS_CP","Y","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT")
    } else {
      c("ID","TIME","ARM","A_DEPOT","A_CENTRAL","A_PERIPHERAL","A_OUTPUT","KA","CP","OBS_CP","Y")
    },
    expect_true(all(expected %in% colnames(results)))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Not NULL outvars + DROP_OTHERS"), {
  model <- model_suite$nonmem$advan4_trans4
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars=c("KA", "DROP_OTHERS")))
  test <- expression(
    expected <- c("ID","TIME","ARM","KA"),
    expect_equal(expected, colnames(results))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Not NULL outvars from ERROR block + DROP_OTHERS"), {
  model <- model_suite$nonmem$advan4_trans4
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars=c("Y", "DROP_OTHERS")))
  test <- expression(
    expected <- c("ID","TIME","ARM","Y"),
    expect_equal(expected, colnames(results))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Covariates in outvars can be output well"), {
  model <- model_suite$nonmem$advan4_trans4

  dataset <- Dataset(2) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1))) %>%
    add(Covariate("WT", c(70, 71)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars=c("Y", "WT", "DROP_OTHERS")))
  test <- expression(
    expected <- c("ID","TIME","ARM", "Y", "WT"), # RxODE first Y then WT, mrgsolve first WT then Y
    expect_true(all(expected %in% colnames(results))),
    expect_true(all(colnames(results) %in% expected))
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("ETAs in outvars can be output well"), {
  model <- model_suite$nonmem$advan4_trans4

  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=1)))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, outvars=c("Y", "ETA_KA", "DROP_OTHERS")))
  test <- expression(
    expected <- c("ID","TIME","ARM", "Y", "ETA_KA"), # RxODE first Y then ETA_KA, mrgsolve first ETA_KA then Y
    expect_true(all(expected %in% colnames(results))),
    expect_true(all(colnames(results) %in% expected))
  )
  campsisTest(simulation, test, env=environment())
})
