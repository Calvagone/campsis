library(testthat)

context("Test some of the distributions")

test_that("Normal distribution", {
  set.seed(1)
  dist <- NormalDistribution(mean=5, sd=1)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(round(mean(values), 1), 5.1)
  expect_equal(round(sd(values), 1), 0.9)
})

test_that("Log-normal distribution", {
  set.seed(1)
  dist <- LogNormalDistribution(meanlog=log(5), sdlog=0.20)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(round(mean(log(values)), 1), 1.6)
  expect_equal(round(sd(log(values)), 1), 0.2)
})

test_that("Uniform distribution", {
  set.seed(1)
  dist <- UniformDistribution(min=5, max=25)
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(round(mean(values), 1), 15.4)
})

test_that("Discrete distribution", {
  set.seed(1)
  dist <- DiscreteDistribution(x=c(1,2,3), prob=c(10,20,40))
  dist <- dist %>% sample(n=as.integer(100))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(sum(values==1), 13)
  expect_equal(sum(values==2), 32)
  expect_equal(sum(values==3), 55)
})

test_that("Binomial distribution", {
  set.seed(1)
  dist <- BinomialDistribution(trials=2, prob=0.5)
  dist <- dist %>% sample(n=as.integer(1000))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(sum(values==0), 244) # Close to 0.5*0.5*100=25% as expected
  expect_equal(sum(values==1), 508) # Close to 0.5*100=50% as expected
  expect_equal(sum(values==2), 248) # Close to 0.5*0.5*100=25% as expected
  
  dist <- BinomialDistribution(trials=1, prob=0.2)
  dist <- dist %>% sample(n=as.integer(1000))
  values <- dist@sampled_values
  #hist(values)
  expect_equal(sum(values==0), 790) # Close to 0.8*100=80% as expected
  expect_equal(sum(values==1), 210) # Close to 0.2*100=20% as expected
})
