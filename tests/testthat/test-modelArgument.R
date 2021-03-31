context("Model argument")

simulated <- eat:::X2Y2.sim(N = 50, border = 0.1)

EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))

scores <- efficiencyEAT(data = simulated, x = c(1, 2), y = c(3, 4), 
                        object = EAT_model, scores_model = "BCC.OUT", 
                        digits = 2, FDH = TRUE, na.rm = TRUE)

# Test 1: model not allowed

test_that("Model not allowed in model argument", {
  expect_error(efficiencyDensity(scores = scores[, 5:6], model = c("EAT", "QMR")))
})

# Test 2: model score not allowed

test_that("Model score not allowed in scores_model argument", {
  expect_error(efficiencyJitter(object = EAT_model, scores_EAT = scores_EAT$EAT_BCC_out, scores_model = "SSD"))
})