# library(testthat)
# 
# context("Test all methods from the simulation progress class")
# 
# simulateProgressBar <- function(progress) {
#   progress@pb <- progress::progress_bar$new(total=100)
#   progress_ <- progress
#   for (replicate in seq_len(progress@replicate)) {
#     progress_@replicate <-  as.integer(replicate)
#     for (scenario in seq_len(progress@scenario)) {
#       progress_@scenario <- as.integer(scenario)
#       for (iteration in seq_len(progress@iteration)) {
#         progress_@iteration <- as.integer(iteration)
#         for (slice in seq_len(progress@slices)) { # With s
#           if (progress_@iteration)
#           progress_@slice <- as.integer(slice)
#           progress@pb$tick(progress_ %>% computeIncrementalProgress())
#         }
#       }
#     }
#   }
#   return(progress@pb$.__enclos_env__$private$current)
# }
# 
# test_that("Check progress (no replicates, no scenarios, no events)", {
#   progress <- SimulationProgress()
#   
#   # Assume 4 slices of subjects are simulated
#   progress@slices <- 4L
#   
#   # No progress is expected at the beginning
#   expect_equal(progress %>% simulateProgressBar(), 0)
#   
#   # First slice of subject is simulated
#   progress@slice <- 1L
#   expect_equal(progress %>% simulateProgressBar(), 25)
#   
#   # Third slice of subject is simulated
#   progress@slice <- 3L
#   expect_equal(progress %>% simulateProgressBar(), 75)
# })
# 
# test_that("Check progress (no replicates, no scenarios, 3 iterations)", {
#   progress <- SimulationProgress(iterations=3)
#   
#   # Assume 4 slices of subjects are simulated
#   progress@slices <- 4L
#   
#   # No progress is expected at the beginning
#   expect_equal(progress %>% simulateProgressBar(), 0)
#   
#   # First slice of subject is simulated in iteration 1
#   progress@slice <- 1L
#   expect_equal(progress %>% simulateProgressBar(), 1/12*100)
#   
#   # Last slice of subject is simulated in iteration 1
#   progress@slice <- 4L
#   expect_equal(progress %>% simulateProgressBar(), 4/12*100)
#   
#   # First slice of subject is simulated in iteration 2
#   progress@slice <- 1L
#   progress@iteration <- 2L
#   expect_equal(progress %>% simulateProgressBar(), 5/12*100)
# })