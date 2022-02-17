library(testthat)
library(ggplot2)

context("Study can be replicated using argument 'replicates'")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("VPC on CP (predicate)", {
  if (skipLongTest) return(TRUE)
  model <- model_library$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"
  
  ds <- Dataset(100)
  for (day in 0:2) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 3*24, by=4)))

  results1 <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, outfun=~PI(.x, output="CP"), seed=seed)
  vpcPlot(results1)
  results2 <- model %>% simulate(dataset=ds, dest="mrgsolve", replicates=5, outfun=~PI(.x, output="CP"), seed=seed)
  vpcPlot(results2)
  
  vpcOutputRegressionTest(results1, output="CP", filename=regFilename)
  vpcOutputRegressionTest(results2, output="CP", filename=regFilename)
})

test_that("VPC on both CP and Y (function)", {
  if (skipLongTest) return(TRUE)
  model <- model_library$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  regFilename <- "full_uncertainty"

  fun <- function(x) {
    return(dplyr::bind_rows(
            PI(x=x, output="CP", level=0.90, gather=TRUE) %>% dplyr::mutate(output="CP"),
            PI(x=x, output="Y", level=0.90, gather=TRUE) %>% dplyr::mutate(output="Y")))
  }

  ds <- Dataset(100)
  for (day in 0:2) {
    ds <- ds %>% add(Infusion(time=day*24, amount=1000, compartment=1))
  }
  ds <- ds %>% add(Observations(times=seq(0, 3*24, by=4)))

  results1 <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5, outfun=fun, seed=seed)
  vpcPlot(results1, scenarios="output") + facet_wrap(~output)

  # vpcOutputRegressionTest(results1, output="Y", filename=regFilename) # Not a good test because seed is controlled by RxODE
  vpcOutputRegressionTest(results1, output="CP", filename=regFilename)
})

test_that("Study replication also works with scenarios", {
  if (skipLongTest) return(TRUE)
  model <- model_library$advan2_trans1
  ds <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0,1,2,4,8,12)))
  
  scenarios <- Scenarios() %>%
    add(Scenario(name="Base model")) %>%
    add(Scenario(name="Increased KA", model=~.x %>% replace(Theta(name="KA", value=3)))) # 3 instead of 1
  
  results <- model %>% simulate(dataset=ds, dest="RxODE", replicates=5,
                                outfun=~PI(.x, output="CP"),
                                seed=seed, scenarios=scenarios)
  expect_true(all(c("replicate", "TIME", "metric", "value", "SCENARIO") %in% colnames(results)))
  expect_true(all(results$SCENARIO %>% unique()==c("Base model", "Increased KA")))
  
  vpcPlot(results, scenarios="SCENARIO") + facet_wrap(~SCENARIO)
})

test_that("Try/catch works as expected if one replicate fails", {
  if (skipLongTest) return(TRUE)
  model <- model_library$advan2_trans2
  
  # Add high uncertainty on THETA_KA (variance of 1)
  varcov <- matrix(1)
  row.names(varcov) <- "THETA_KA"
  colnames(varcov) <- "THETA_KA"
  
  model@parameters@varcov <- varcov
  
  dataset <- Dataset(10) %>% 
    add(Bolus(time=0, amount=10, compartment=1)) %>%
    add(Observations(c(0,1,2,4,8,10000)))
  
  # Simulation with mrgsolve
  # An error is thrown by mrgsolve for the first replicate and caught by the try/catch statement
  results_mrgsolve <- simulate(model=model, dataset=dataset, seed=13, replicates=3, outvars="KA", dest="mrgsolve")
  expect_equal(results_mrgsolve$replicate %>% unique(), c(2,3)) # Replicate 1 has error
  expect_false(any(is.na(results_mrgsolve$CP)))
  
  # Simulation with RxODE
  # An warning is thrown by RxODE for the first replicate
  results_rxode <- expect_warning(simulate(model=model, dataset=dataset, seed=13, replicates=3, outvars="KA", dest="RxODE"),
                                  regexp="some ID\\(s\\) could not solve the ODEs correctly")
  expect_equal(results_rxode$replicate %>% unique(), c(1,2,3))
  expect_true(any(is.na(results_rxode$CP))) # Some NA's in replicate 1
  expect_false(any(is.na(results_rxode %>% dplyr::filter(replicate != 1) %>% dplyr::pull(CP))))
})
