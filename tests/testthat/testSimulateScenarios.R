library(testthat)

context("Test the simulate method with scenarios")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate scenarios - make few changes on dataset"), {
  
  model <- model_suite$nonmem$advan4_trans4
  regFilename <- "scenarios_few_changes_on_dataset"
  
  dataset <- Dataset() %>%
    add(Observations(times=seq(0, 24, by=3)))
  
  scenarios <- Scenarios() %>% 
    add(Scenario("1000 mg SD", dataset=~.x %>% setSubjects(2) %>% add(Bolus(time=0, 1000)))) %>%
    add(Scenario("1500 mg SD", dataset=~.x %>% setSubjects(2) %>% add(Bolus(time=0, 1500)))) %>%
    add(Scenario("2000 mg SD", dataset=~.x %>% setSubjects(4) %>% add(Bolus(time=0, 2000))))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, scenarios=scenarios, seed=seed, outvars="KA"))
  test <- expression(
    spaghettiPlot(results, "CP", "SCENARIO") + ggplot2::facet_wrap(~SCENARIO),
    outputRegressionTest(results, output=c("SCENARIO","CP"), filename=regFilename),
    expect_equal(results$SCENARIO %>% unique(), c("1000 mg SD", "1500 mg SD", "2000 mg SD")),
    
    # Check some 'properties' of the scenarios:
    results$EPS_PROP <- (results$OBS_CP/results$CP) - 1,
    res_scenario1 <- results %>% dplyr::filter(SCENARIO=="1000 mg SD"),
    res_scenario2 <- results %>% dplyr::filter(SCENARIO=="1500 mg SD"),
    res_scenario3 <- results %>% dplyr::filter(SCENARIO=="2000 mg SD"),
    
    # 1) ID's start at 1 in each scenario
    expect_equal(res_scenario1$ID %>% unique(), c(1,2)),
    expect_equal(res_scenario2$ID %>% unique(), c(1,2)),
    expect_equal(res_scenario3$ID %>% unique(), c(1,2,3,4)),
    
    # 2) Same seed is used in each scenario -> same ETA's if same number of subjects
    expect_equal(res_scenario1$KA %>% unique(), res_scenario2$KA %>% unique()),
    
    # 3) Same seed is used in each scenario -> same residual variability if same number of subjects & same protocol
    expect_equal(res_scenario1$EPS_PROP, res_scenario2$EPS_PROP)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Simulate scenarios - make few changes on model"), {
  model <- model_suite$nonmem$advan4_trans4
  regFilename <- "scenarios_few_changes_on_model"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0,1,2,3,4,5,6,12,24))) 
  
  scenarios <- Scenarios() %>% 
    add(Scenario("THETA_KA=1", model=~.x %>% replace(Theta(name="KA", value=1)))) %>%
    add(Scenario("THETA_KA=3", model=~.x %>% replace(Theta(name="KA", value=3)))) %>%
    add(Scenario("THETA_KA=6", model=~.x %>% replace(Theta(name="KA", value=6))))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, scenarios=scenarios, seed=seed))
  test <- expression(
    spaghettiPlot(results, "CP", "SCENARIO") + ggplot2::facet_wrap(~SCENARIO),
    outputRegressionTest(results, output=c("SCENARIO","CP"), filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
