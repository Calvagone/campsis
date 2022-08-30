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
