library(testthat)

context("Test the bootstrap object")

data <- data.frame(BS_ID=c(1,2,3), WT=c(10,20,30), AGE=c(3,6,9))

test_that("Create a bootstrap element", {
  bootstrap <- Bootstrap(data=data)
  
  expect_equal(bootstrap@data, data)
  expect_equal(bootstrap@replacement, FALSE)
  expect_equal(bootstrap@random, FALSE)
})

test_that("Sample from bootstrap (random=FALSE, replacement=FALSE)", {
  bootstrap <- Bootstrap(data=data)
  covariates <- bootstrap %>% sample(3L)
  expect_equal(covariates[[1]], Covariate("WT", c(10,20,30)))
  expect_equal(covariates[[2]], Covariate("AGE", c(3,6,9)))
})

test_that("Sample from bootstrap (random=FALSE, replacement=TRUE)", {
  bootstrap <- Bootstrap(data=data, replacement=TRUE)
  covariates <- bootstrap %>% sample(5L)
  expect_equal(covariates[[1]], Covariate("WT", c(10,20,30,10,20)))
  expect_equal(covariates[[2]], Covariate("AGE", c(3,6,9,3,6)))
})

test_that("Sample from bootstrap (random=TRUE, replacement=FALSE)", {
  set.seed(2)
  bootstrap <- Bootstrap(data=data, random=TRUE, replacement=FALSE)
  covariates <- bootstrap %>% sample(3L)
  expect_equal(covariates[[1]], Covariate("WT", c(10,30,20)))
  expect_equal(covariates[[2]], Covariate("AGE", c(3,9,6)))
  expect_equal(bootstrap %>% getNames(), c("WT", "AGE"))
})

test_that("Sample from bootstrap (random=TRUE, replacement=TRUE)", {
  set.seed(2)
  bootstrap <- Bootstrap(data=data, random=TRUE, replacement=TRUE) 
  covariates <- bootstrap %>% sample(5L)
  expect_equal(covariates[[1]], Covariate("WT", c(10,30,20,20,10)))
  expect_equal(covariates[[2]], Covariate("AGE", c(3,9,6,6,3)))
})

test_that("Add bootstrap element into a dataset and export", {
  ds <- Dataset(3) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Bootstrap(data=data))
  
  table <- ds %>% export(dest="RxODE")
  expect_equal(table$WT, c(10,20,30))
  expect_equal(table$AGE, c(3,6,9))
  expect_false("BS_ID" %in% colnames(table))
})

test_that("Add bootstrap element into a dataset and export (random=TRUE, replacement=TRUE)", {

  ds <- Dataset(5) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Bootstrap(data=data))
  
  expect_error(ds %>% export(dest="RxODE"), regexp="A fixed distribution should have exactly 5 values, not 3")
  
  ds <- Dataset(5) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Bootstrap(data=data, random=TRUE, replacement=TRUE, output_id=TRUE))
  
  table <- ds %>% export(dest="RxODE", seed=2)
  expect_equal(table$WT, c(10,30,20,20,10))
  expect_equal(table$AGE, c(3,9,6,6,3))
  expect_equal(table$BS_ID, c(1,3,2,2,1))
})
