
library(testthat)

context("Test all methods from the dataset class")

test_that("Add entry, order, filter (simple example)", {
  
  dataset <- new("dataset") 
  
  # Add doses
  dataset <- dataset %>% add(new("bolus", time=0, amount=100))
  dataset <- dataset %>% add(new("bolus", time=24, amount=100))
  dataset <- dataset %>% add(new("bolus", time=48, amount=100))


  # Add observations
  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% add(new("observation", time=t))
  }
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  expect_equal(nrow(table), 16)
})

test_that("Two arms example", {
  
  # Create 2 arms
  arm1 <- new("arm", id=as.integer(1), subjects=as.integer(4))
  arm2 <- new("arm", id=as.integer(2), subjects=as.integer(3))

  # Add doses in respective arms
  arm1 <- arm1 %>% add(new("bolus", time=0, amount=100))
  arm2 <- arm2 %>% add(new("bolus", time=0, amount=200))
    
  # Add observations
  for (t in seq(0, 48, by=4)) {
    obs <- new("observation", time=t)
    arm1 <- arm1 %>% add(obs)
    arm2 <- arm2 %>% add(obs)
  }
  
  # Create dataset
  dataset <- new("dataset")
  dataset <- dataset %>% add(arm1)
  dataset <- dataset %>% add(arm2)
  
  # Arms number
  expect_equal(length(dataset@arms), 2)
  
  covariates <- dataset@arms@list[[1]]@covariates
  covDf <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
    data <- (covariate %>% sample(n=length(ids)))@sampled_values
    matrix <- matrix(data=data, ncol=1)
    colnames(matrix) <- covariate@name
    matrix %>% tibble::as_tibble()
  })
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  expect_equal(nrow(table), 98)
  
})

test_that("Export using config", {
  
  dataset <- new("dataset") 
  
  # Add doses
  dataset <- dataset %>% add(new("bolus", time=0, amount=100))
  dataset <- dataset %>% add(new("bolus", time=24, amount=100))
  dataset <- dataset %>% add(new("bolus", time=48, amount=100))
  
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    dataset <- dataset %>% add(new("observation", time=t))
  }
  
  # Export to RxODE
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  table <- dataset %>% export(dest="RxODE",
                              config=config)
  
  expect_true(all(c(1,2) %in% table$CMT))
  
})

test_that("Export constant covariates work well (N=1, N=2)", {
  
  dataset <- new("dataset") 
  
  # Add doses
  dataset <- dataset %>% add(new("bolus", time=0, amount=100))
  dataset <- dataset %>% add(new("bolus", time=24, amount=100))
  dataset <- dataset %>% add(new("bolus", time=48, amount=100))
  
  # Add covariate
  dataset <- dataset %>% add(new("constant_covariate", name="WT", value=70))
  dataset <- dataset %>% add(new("constant_covariate", name="HT", value=180))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    dataset <- dataset %>% add(new("observation", time=t))
  }
  
  # Export to RxODE N=1
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  table <- dataset %>% export(dest="RxODE",
                              config=config)
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  
  # Export to RxODE N=2
  arm <- dataset@arms %>% default()
  arm@subjects <- as.integer(2)
  dataset@arms <- dataset@arms %>% replace(arm)
  
  table <- dataset %>% export(dest="RxODE",
                              config=config)
  
  expect_true(all(table$WT==70))
  expect_true(all(table$HT==180))
  
})

test_that("Export fixed covariates work well (N=3)", {

  arm <- new("arm", id=as.integer(1), subjects=as.integer(3))
  
  # Add doses
  arm <- arm %>% add(new("bolus", time=0, amount=100))
  arm <- arm %>% add(new("bolus", time=24, amount=100))
  arm <- arm %>% add(new("bolus", time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(new("fixed_covariate", name="WT", values=c(65, 70, 75)))
  arm <- arm %>% add(new("fixed_covariate", name="HT", values=c(175, 180, 185)))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(new("observation", time=t))
  }
  
  dataset <- new("dataset")
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  table <- dataset %>% export(dest="RxODE",
                              config=config)
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, data.frame(ID=c(1,2,3), WT=c(65,70,75), HT=c(175,180,185)))
})


test_that("Export function covariates work well (N=3)", {
  
  arm <- new("arm", id=as.integer(1), subjects=as.integer(3))
  
  # Add doses
  arm <- arm %>% add(new("bolus", time=0, amount=100))
  arm <- arm %>% add(new("bolus", time=24, amount=100))
  arm <- arm %>% add(new("bolus", time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(new("function_covariate", name="WT", fun="rnorm", args=list(mean=70, sd=10)))
  arm <- arm %>% add(new("function_covariate", name="HT", fun="rnorm", args=list(mean=180, sd=20)))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(new("observation", time=t))
  }
  
  dataset <- new("dataset")
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE N=1
  set.seed(1)
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  table <- dataset %>% export(dest="RxODE", config=config)
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct() %>% dplyr::mutate(WT=round(WT), HT=round(HT))
  expect_equal(subTable, data.frame(ID=c(1,2,3), WT=c(64,72,62), HT=c(212,187,164)))
})

test_that("Export boostrap covariates work well (N=8)", {
  
  arm <- new("arm", id=as.integer(1), subjects=as.integer(8))
  
  # Add doses
  arm <- arm %>% add(new("bolus", time=0, amount=100))
  arm <- arm %>% add(new("bolus", time=24, amount=100))
  arm <- arm %>% add(new("bolus", time=48, amount=100))
  
  # Add covariate
  arm <- arm %>% add(new("bootstrap_covariate", name="WT", data=c(65, 70, 75), random=TRUE, replacement=TRUE))
  arm <- arm %>% add(new("bootstrap_covariate", name="HT", data=c(175, 180, 185), random=TRUE, replacement=TRUE))
  
  # Add observations
  for (t in seq(0, 48, by=10)) {
    arm <- arm %>% add(new("observation", time=t))
  }
  
  dataset <- new("dataset")
  dataset <- dataset %>% add(arm)
  
  # Export to RxODE
  set.seed(1)
  config <- new("dataset_config", def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(2))
  table <- dataset %>% export(dest="RxODE", config=config)
  
  subTable <- table %>% dplyr::select(ID, WT, HT) %>% dplyr::distinct()
  expect_equal(subTable, data.frame(ID=c(1,2,3,4,5,6,7,8), WT=c(65,75,65,70,65,75,75,70), HT=c(180,185,185,175,175,175,180,180)))
})

