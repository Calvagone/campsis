library(testthat)

context("Test all default plots available in Campsis")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Scatter plot works as expected"), {
  model <- model_suite$pk$`1cpt_fo`
  thetaVc <- model %>% find(Theta("VC"))
  thetaCl <- model %>% find(Theta("CL"))
  
  # Add correlation between VC and CL
  model <- model %>%
    add(Omega(name="VC_CL", index=thetaVc@index, index2=thetaCl@index, value=0, type="cor"))

  dataset <- Dataset(subjects=500) %>% 
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Observations(times=0:24))

  scenarios <- Scenarios() %>%
    add(Scenario(name="Correlation=0.5", model=~.x %>% replace(Omega(name="VC_CL", value=0.5, type="cor")))) %>%
    add(Scenario(name="Correlation=0.9", model=~.x %>% replace(Omega(name="VC_CL", value=0.9, type="cor"))))
    
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, scenarios=scenarios, outvars=c("VC", "CL")))
  
  test <- expression(
    shadedPlot(results, "CONC", scenarios="SCENARIO"),
    scatterPlot(results, c("VC")), # 1D scatter plot (of little interest)
    scatterPlot(results, c("VC", "CL")), # No color
    scatterPlot(results, c("VC", "CL"), scenarios="SCENARIO"), # Stratify by SCENARIO value
    scatterPlot(results, c("VC", "CL"), scenarios="SCENARIO", time=24), # Same plot, parameters do not change over time
    
    scenarioA <- results %>% dplyr::filter(SCENARIO=="Correlation=0.5" & TIME==0),
    scenarioB <- results %>% dplyr::filter(SCENARIO=="Correlation=0.9" & TIME==0),
    
    # Back to ETA's
    corA <- cor(x=log(scenarioA$VC/60), y=log(scenarioA$CL/3)),
    corB <- cor(x=log(scenarioB$VC/60), y=log(scenarioB$CL/3)),
    
    # Check these correlations (round to 1 decimal digit)
    # The higher N, the closer the correlation will be to its true value
    expect_equal(round(corA, digits=1), 0.50),
    expect_equal(round(corB, digits=1), 0.90)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Shaded plot works as expected"), {
  model <- model_suite$pk$`1cpt_fo`
  
  dataset <- Dataset(subjects=100) %>% 
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Observations(times=0:24))
  
  scenarios <- Scenarios() %>%
    add(Scenario(name="E", model=~.x %>% replace(Theta(name="VC", value=100)))) %>%
    add(Scenario(name="D", model=~.x %>% replace(Theta(name="VC", value=200)))) %>%
    add(Scenario(name="C", model=~.x %>% replace(Theta(name="VC", value=300)))) %>%
    add(Scenario(name="B", model=~.x %>% replace(Theta(name="VC", value=400)))) %>%
    add(Scenario(name="A", model=~.x %>% replace(Theta(name="VC", value=500))))
  
  results <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1, scenarios=scenarios)
  shadedPlot(results, "CONC", scenarios="SCENARIO")
  spaghettiPlot(results, "CONC", scenarios="SCENARIO")
})

test_that(getTestName("Grouping by ARM and stratifying by WT should work"), {
  
  model <- model_suite$pk$'1cpt_fo' %>%
    replace(Equation("CL", "TVCL * exp(ETA_CL) * pow(WT/70,0.75)"))
  
  arm1 <- Arm(subjects=50, label="Arm 1") %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=24, addl=0)) %>%
    add(Covariate("WT", c(rep(50,25), rep(100,25)))) %>%
    add(Observations(seq(0,24,by=1)))
  
  arm2 <- Arm(subjects=50, label="Arm 2") %>%
    add(Bolus(time=0, amount=2000, compartment=1, ii=24, addl=0)) %>%
    add(Covariate("WT", c(rep(50,25), rep(100,25)))) %>%
    add(Observations(seq(0,24,by=1)))
  
  dataset <- Dataset() %>%
    add(c(arm1, arm2)) %>%
    add(DatasetConfig(exportTSLD=TRUE, exportTDOS=TRUE))

  results <- simulate(model=model, dataset=dataset, seed=1, dest="mrgsolve", outvars="WT")
  
  spaghettiPlot(results, "CONC", c("ARM", "WT")) +
    ggplot2::facet_wrap(~WT)
  
  shadedPlot(results, "CONC", c("ARM", "WT")) +
    ggplot2::facet_wrap(~WT)
  
})
