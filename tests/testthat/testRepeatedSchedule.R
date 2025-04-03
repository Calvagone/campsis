library(testthat)

context("Test the available repeated schedules")

test_that("Cyclic schedule can be used to repeat the original base schedule", {
  schedule <- CyclicSchedule(duration=24*7, repetitions=2)
  expect_equal(schedule@duration, 24*7)
  expect_equal(schedule@repetitions, 2)
  expected <- c(0,24,48,168,192,216,336,360,384)
  expect_equal(c(0,24,48) %>% repeatSchedule(schedule), expected)
  
  times <- Bolus(time=0, amount=100, ii=24, addl=2, rep=schedule) %>%
    unwrapTreatment() %>%
    purrr::map_dbl(~.x@time)
  expect_equal(times, expected)
})

test_that("'Repeat-at' schedule can be used to repeat the original base schedule", {
  schedule <- RepeatAtSchedule(times=c(168, 336))
  expect_equal(schedule@times, c(0, 168, 336))
  expected <- c(0,24,48,168,192,216,336,360,384)
  expect_equal(c(0,24,48) %>% repeatSchedule(schedule), expected)
  
  times <- Infusion(time=0, amount=100, ii=24, addl=2, rep=schedule) %>%
    unwrapTreatment() %>%
    purrr::map_dbl(~.x@time)
  expect_equal(times, expected)
})
