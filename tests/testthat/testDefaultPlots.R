library(testthat)

context("Test all default plots available in Campsis")

seed <- 1
source(file.path(getwd(), test_path(), "testUtils.R"))

test_that("Scatter plot works as expected", {
  model <- model_suite$testing$pk$`1cpt_fo`
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
    shadedPlot(results, "CONC", "SCENARIO"),
    scatterPlot(results, c("VC")), # 1D scatter plot (of little interest)
    plot1 <- expect_no_error(scatterPlot(results, c("VC", "CL"))), # No color
    plot2 <- expect_no_error(scatterPlot(results, c("VC", "CL"), "SCENARIO")), # Stratify by SCENARIO value
    scatterPlot(results, c("VC", "CL"), "SCENARIO", time=24), # Same plot, parameters do not change over time

    scenarioA <- results %>% dplyr::filter(SCENARIO=="Correlation=0.5" & TIME==0),
    scenarioB <- results %>% dplyr::filter(SCENARIO=="Correlation=0.9" & TIME==0),

    # Back to ETA's
    corA <- cor(x=log(scenarioA$VC/60), y=log(scenarioA$CL/3)),
    corB <- cor(x=log(scenarioB$VC/60), y=log(scenarioB$CL/3)),

    # Check these correlations (round to 1 decimal digit)
    # The higher N, the closer the correlation will be to its true value
    expect_equal(round(corA, digits=1), 0.50),
    expect_equal(round(corB, digits=1), 0.90),
    if (!skipVdiffrTests()) {
      vdiffr::expect_doppelganger(sprintf("scatterPlot / no colour / %s", destEngine), plot1)
      vdiffr::expect_doppelganger(sprintf("scatterPlot / colour: SCENARIO / %s", destEngine), plot2)
    }
  )
  campsisTest(simulation, test, env=environment())
})

test_that("Shaded and spaghetti plots work as expected", {
  model <- model_suite$testing$pk$`1cpt_fo`

  dataset <- Dataset(subjects=20) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Observations(times=0:24))

  scenarios <- Scenarios() %>%
    add(Scenario(name="E", model=~.x %>% replace(Theta(name="VC", value=100)))) %>%
    add(Scenario(name="D", model=~.x %>% replace(Theta(name="VC", value=200)))) %>%
    add(Scenario(name="C", model=~.x %>% replace(Theta(name="VC", value=300)))) %>%
    add(Scenario(name="B", model=~.x %>% replace(Theta(name="VC", value=400)))) %>%
    add(Scenario(name="A", model=~.x %>% replace(Theta(name="VC", value=500))))

  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed, scenarios=scenarios))

  test <- expression(
    plot1 <- expect_no_error(shadedPlot(results, "CONC", "SCENARIO")),
    plot2 <- expect_no_error(spaghettiPlot(results, "CONC", "SCENARIO")),
    if (!skipVdiffrTests()) {
      vdiffr::expect_doppelganger(sprintf("shadedPlot / colour: SCENARIO / %s", destEngine), plot1)
      vdiffr::expect_doppelganger(sprintf("spaghettiPlot / colour: SCENARIO / %s", destEngine), plot2)
    }
  )
  campsisTest(simulation, test, env=environment())
})

test_that("Grouping by ARM and stratifying by WT should work", {
  model <- model_suite$testing$pk$'1cpt_fo' %>%
    replace(Equation("CL", "TVCL * exp(ETA_CL) * pow(WT/70,0.75)")) %>%
    replace(Equation("VC", "TVVC * exp(ETA_VC) * WT/70"))

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

  simulation <- expression(simulate(model=model, dataset=dataset, seed=seed, dest=destEngine, outvars="WT"))

  test <- expression(
    # Colour by ARM and stratify by WT
    plot1 <- expect_no_error(spaghettiPlot(results, "CONC", c("ARM")) +
      ggplot2::facet_wrap(~WT)),

    # Colour by ARM and stratify by WT
    plot2 <- expect_no_error(shadedPlot(results, "CONC", c("ARM"), "WT") +
      ggplot2::facet_wrap(~WT)),

    # Colour by both ARM and WT columns
    plot3 <- expect_no_error(shadedPlot(results, "CONC", c("ARM","WT")) +
      ggplot2::facet_wrap(~WT)),

    if (!skipVdiffrTests()) {
      vdiffr::expect_doppelganger(sprintf("spaghettiPlot / colour: ARM / strat: WT / %s", destEngine), plot1)
      vdiffr::expect_doppelganger(sprintf("shadedPlot / colour: ARM / strat: WT / %s", destEngine), plot2)
      vdiffr::expect_doppelganger(sprintf("shadedPlot / colour: ARM,WT / strat: WT / %s", destEngine), plot3)
    }
  )
  campsisTest(simulation, test, env=environment())
})
