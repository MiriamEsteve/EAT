context("training / test")

simulated <- eat:::X2Y2.sim(N = 50, border = 0.1)
training <- simulated[1:30, 1:4]
test <- simulated[31:50, 1:4]

# Test 1: training and test size is different

test_that("Training and test have different size", {
  subtest <- test[, c(1 ,3)]
  expect_error(bestEAT(training, subtest, x = 1:2, y = 3:4))
})

# Test 2: training and test variable names

test_that("Training and test have different variable names", {
  subtest <- test
  names(subtest) <- c("a", "b", "c", "d")
  expect_error(bestRFEAT(training, test, x = 1:2, y = 3:4))
})