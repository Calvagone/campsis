library(testthat)
library(ggplot2)

context("Study can be replicated using argument 'replicates'")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("VPC on CP, using predicate"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$other$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"

  ds <- Dataset(100) %>%
    add(Infusion(time=0, amount=1000, compartment=1, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 3*24, by=4)))

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, replicates=5, outfun=~PI(.x, output="CP"), seed=seed))
  test <- expression(
    vpcOutputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("VPC on both CP and Y, using function"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$other$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"

  fun <- function(x) {
    return(dplyr::bind_rows(
            PI(x=x, output="CP", level=0.90, gather=TRUE) %>% dplyr::mutate(output="CP"),
            PI(x=x, output="Y", level=0.90, gather=TRUE) %>% dplyr::mutate(output="Y")))
  }

  ds <- Dataset(100) %>%
    add(Infusion(time=0, amount=1000, compartment=1, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 3*24, by=4)))

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, replicates=5, outfun=fun, seed=seed))
  test <- expression(
    vpcPlot(results, scenarios="output") + facet_wrap(~output),
    vpcOutputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Study replication also works with scenarios"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$nonmem$advan2_trans1
  ds <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0,1,2,4,8,12)))

  scenarios <- Scenarios() %>%
    add(Scenario(name="Base model")) %>%
    add(Scenario(name="Increased KA", model=~.x %>% replace(Theta(name="KA", value=3)))) # 3 instead of 1

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, replicates=5,
                                    outfun=~PI(.x, output="CP"), seed=seed, scenarios=scenarios))
  test <- expression(
    expect_true(all(c("replicate", "TIME", "metric", "value", "SCENARIO") %in% colnames(results))),
    expect_true(all(results$SCENARIO %>% unique()==c("Base model", "Increased KA"))),
    vpcPlot(results, scenarios="SCENARIO") + facet_wrap(~SCENARIO)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Try/catch works as expected if one replicate fails"), {
  if (skipLongTest) return(TRUE)
  model <- model_suite$nonmem$advan2_trans2

  # Add high uncertainty on THETA_KA (variance of 1)
  varcov <- matrix(1)
  row.names(varcov) <- "THETA_KA"
  colnames(varcov) <- "THETA_KA"

  model@parameters@varcov <- varcov

  dataset <- Dataset(10) %>%
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(c(0,1,2,4,8,10000)))

  test <- expression(
    if (destEngine %in% c("RxODE", "rxode2")) {
      # Simulation with RxODE
      # An warning is thrown by RxODE for the first replicate
      results <- expect_warning(simulate(model=model, dataset=dataset, seed=13, replicates=3, outvars="KA", dest=destEngine),
                                      regexp="some ID\\(s\\) could not solve the ODEs correctly")
      expect_equal(results$replicate %>% unique(), c(1,2,3))
      expect_true(any(is.na(results$CP))) # Some NA's in replicate 1
      expect_false(any(is.na(results %>% dplyr::filter(replicate != 1) %>% dplyr::pull(CP))))
    },
    if (destEngine %in% c("mrgsolve")) {
      # Simulation with mrgsolve
      # An error is thrown by mrgsolve for the first replicate and caught by the try/catch statement
      results <- simulate(model=model, dataset=dataset, seed=13, replicates=3, outvars="KA", dest=destEngine)
      expect_equal(results$replicate %>% unique(), c(2,3)) # Replicate 1 has error
      expect_false(any(is.na(results$CP)))
    }
  )
  campsisTest(expression(), test, env=environment())
})

test_that(getTestName("Replicates can be simulated in parallel"), {
  if (skipLongTest) return(TRUE)
  # progressr::handlers(global=TRUE)
  # progressr::handlers(campsis_handler())
  regFilename <- "replicates_in_parallel"

  model <- model_suite$pk$`1cpt_fo` %>%
    add(Equation("EPSILON", "EPS_RUV_FIX"), pos=Position(ErrorRecord()))

  dataset <- Dataset(25) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(12)))

  # Running 25 replicates with 2 CPU's
  settings <- Settings(Hardware(cpu=2, replicate_parallel=TRUE))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, replicates=25, seed=seed, settings=settings))
  test <- expression(
    expect_equal(results$EPSILON %>% unique() %>% length(), 625), # Check RUV is unique
    outputRegressionTest(results, output="CONC", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())

  # Running 25 replicates with only 1 CPU
  setupPlanSequential()
  settings <- Settings()

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, replicates=25, seed=seed, settings=settings))
  test <- expression(
    expect_equal(results$EPSILON %>% unique() %>% length(), 625), # Check RUV is unique
    outputRegressionTest(results, output="CONC", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
