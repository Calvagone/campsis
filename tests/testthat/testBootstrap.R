library(testthat)

context("Test the bootstrap object")

data <- data.frame(BS_ID=c(1,2,3), WT=c(10,20,30), AGE=c(3,6,9))

test_that("Create a few correct/incorrect bootstraps", {
  # Simple working bootstrap
  bootstrap <- Bootstrap(data=data)
  expect_equal(bootstrap@data, data %>% tibble::as_tibble())
  expect_equal(bootstrap@replacement, FALSE)
  expect_equal(bootstrap@random, FALSE)

  # Check the same bootstrap in its tibble version is working as well
  bootstrap <- Bootstrap(data=data %>% tibble::as_tibble())
  expect_equal(bootstrap@data, data %>% tibble::as_tibble())

  # Simple working bootstrap, custom ID
  data2 <- data %>% dplyr::rename(MY_CUSTOM_ID=BS_ID)
  expect_error(Bootstrap(data=data2), regexp="Unique identifier 'BS_ID' not part of data")
  bootstrap <- Bootstrap(data=data2, id="MY_CUSTOM_ID")
  expect_equal(bootstrap@data, data %>% tibble::as_tibble())

  # BS_ID not unique
  data3 <- data
  data3$BS_ID <- c(1,1,2)
  expect_error(Bootstrap(data=data3), regexp="Column 'BS_ID' contains duplicates")

  # BS_ID not numeric
  data4 <- data
  data4$BS_ID <- c(1,2,"THREE")
  expect_error(Bootstrap(data=data4), regexp="Column 'BS_ID' must be numeric")

  # BS_ID not integer
  data5 <- data
  data5$BS_ID <- c(1,2,3.5)
  expect_error(Bootstrap(data=data5), regexp="Column 'BS_ID' must contain integers only")

  # Covariates not numeric
  data6 <- data
  data6$WT <- c("10","20","30")
  data6$AGE <- c("3","6","9")
  expect_error(Bootstrap(data=data6), regexp="Column\\(s\\) WT,AGE are not numeric")
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

  table <- ds %>% export(dest="rxode2")
  expect_equal(table$WT, c(10,20,30))
  expect_equal(table$AGE, c(3,6,9))
  expect_false("BS_ID" %in% colnames(table))
})

test_that("Add bootstrap element into a dataset and export (random=TRUE, replacement=TRUE)", {

  ds <- Dataset(5) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Bootstrap(data=data))

  expect_error(ds %>% export(dest="rxode2"), regexp="A fixed distribution should have exactly 5 values, not 3")

  ds <- Dataset(5) %>%
    add(Bolus(time=0, amount=100, compartment=1)) %>%
    add(Bootstrap(data=data, random=TRUE, replacement=TRUE, export_id=TRUE))

  table <- ds %>% export(dest="rxode2", seed=2)
  expect_equal(table$WT, c(10,30,20,20,10))
  expect_equal(table$AGE, c(3,9,6,6,3))
  expect_equal(table$BS_ID, c(1,3,2,2,1))
})
