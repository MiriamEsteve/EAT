context("EAT")

simulated <- eat:::X2Y2.sim(N = 50, border = 0.1)
model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))

# Test 1: return an EAT object type with 5 elements

test_that("Return an object EAT of length 5", {
  expect_s3_class(model, "EAT")
  expect_equal(length(model), 5)
})

# Test 2: tree is a list

test_that("Tree is a list", {
  expect_type(model[["tree"]], "list")
})


# Test 3: data --> dataframe, matrix or list

test_that("Acceptable data: dataframe matrix or list", {
  
  data1 <- data.frame(simulated) # data.frame
  data2 <- as.matrix(simulated) # matrix
  data3 <- list(x1 = data1$x1, # list
                x2 = data1$x2,
                y1 = data1$y1,
                y2 = data1$y2)

  set.seed(10)
  EAT1 <- EAT(data = data1, x = c(1, 2), y = c(3, 4))
  
  set.seed(10)
  EAT2 <- EAT(data = data2, x = c(1, 2), y = c(3, 4))
  
  set.seed(10)
  EAT3 <- EAT(data = data3, x = c(1, 2), y = c(3, 4))
  
  expect_identical(EAT1, EAT2)
  expect_identical(EAT1, EAT3)
})

# Test 4: indexes bad defined

test_that("Indexes bad defined", {
  expect_error(EAT(data = simulated, x = c(1, 8), y = c(3, 4)))
})

# Test 5: Pareto Dominance property

test_that("Pareto-dominance property", {
  expect_true(checkEAT(model[["tree"]]))
})

