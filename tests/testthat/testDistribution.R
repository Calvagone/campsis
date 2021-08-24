library(testthat)

context("Test some of the distributions")

test_that("Normal distribution", {
  set.seed(1)
  dist <- NormalDistribution(mean=5, sd=1)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  hist(values)
  expect_equal(round(mean(values), 1), 5.1)
  expect_equal(round(sd(values), 1), 0.9)
})

test_that("Log-normal distribution", {
  set.seed(1)
  dist <- LogNormalDistribution(meanlog=log(5), sdlog=0.20)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  hist(values)
  expect_equal(round(mean(log(values)), 1), 1.6)
  expect_equal(round(sd(log(values)), 1), 0.2)
})

test_that("Uniform distribution", {
  set.seed(1)
  dist <- UniformDistribution(min=5, max=25)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  hist(values)
  expect_equal(round(mean(values), 1), 15.4)
})

test_that("Discrete distribution", {
  set.seed(1)
  dist <- DiscreteDistribution(x=c(1,2,3), prob=c(10,20,40))
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  hist(values)
  expect_equal(sum(values==1), 13)
  expect_equal(sum(values==2), 32)
  expect_equal(sum(values==3), 55)
})
