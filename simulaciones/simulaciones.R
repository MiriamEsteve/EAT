# 6 possible inputs: 1, 3, 6, 9, 12, 15
# 3 possible sizes: 50, 100, 150
# 10 scenarios for each union --> 10 * 6 * 3 = 180 scen

errores <- data.frame(id = rep(NA, 180),
                      nX = rep(NA, 180),
                      N = rep(NA, 180),
                      esc = rep(NA, 180),
                      MSE_EAT = rep(NA, 180),
                      BIAS_EAT = rep(NA, 180),
                      BIASabs_EAT = rep(NA, 180),
                      MSE_RFEAT = rep(NA, 180),
                      BIAS_RFEAT = rep(NA, 180),
                      BIASabs_RFEAT = rep(NA, 180),
                      MSE_FDH = rep(NA, 180),
                      BIAS_FDH = rep(NA, 180),
                      BIASabs_FDH = rep(NA, 180)) 

id <- 0

for (nX in c(1, 3, 6, 9, 12, 15)){
  for (N in c(50, 100, 150)){
    for(esc in 1:10){
      
      # Data generation
      
      data <- eat:::sgle.out_scenario(N, nX)
      
      # Input and output indexes
      x <- 1:nX
      y <- nX + 1
      
      # Update id
      print(paste("#### ID ####:", id))
      id <- id + 1
      
      errores[id, 1] <- id  # id
      errores[id, 2] <- nX  # number of inputs: 1-3-6-9-12-15
      errores[id, 3] <- N   # number of observations: 50-100-150 
      errores[id, 4] <- esc # scenario: 1-10
      
      # EAT MODEL
      
      EAT <- EAT(data = data,
                   x = x,
                   y = y)
      
      # RFEAT MODEL
      
      RFEAT <- RFEAT(data = data,
                     x = x,
                     y = y,
                     m = 50)
      
      # Transform to data.frame for predictions
      
      if (nX == 1){
        newdata <- as.data.frame(data[, nX])
        names(newdata) <- "x1"
      } else {
        newdata <- as.data.frame(data[, 1:nX])
      }
      
      # Predictions for EAT
      
      predictions <- predict_EAT(EAT, newdata)
      dif <- predictions[, "y_pred"] - data[, "yD"]
      
      # MSE
      errores[id, 5] <- round(sum(dif ^ 2) / N, 3)
      # Bias
      errores[id, 6] <- round(sum(dif) / N, 3)
      # Bias absolute
      errores[id, 7] <- round(sum(abs(predictions[, "y_pred"] - data[, "yD"])) / N, 3)
      
      # Predictions for RFEAT
      
      predictions <- predict_RFEAT(RFEAT, newdata)
      dif <- predictions[, "y_pred"] - data[, "yD"]
      
      # MSE
      errores[id, 8] <- round(sum(dif ^ 2) / N, 3)
      # Bias
      errores[id, 9] <- round(sum(dif) / N, 3)
      # Bias absolute
      errores[id, 10] <- round(sum(abs(predictions[, "y_pred"] - data[, "yD"])) / N, 3)
      
      # Predictions for FDH
      
      predictions <- eat:::FDH(data, x, y)
      dif <- predictions[, "y_pred"] - data[, "yD"]
      
      # MSE
      errores[id, 11] <- round(sum(dif ^ 2) / N, 3)
      # Bias
      errores[id, 12] <- round(sum(dif) / N, 3)
      # Bias absolute
      errores[id, 13] <- round(sum(abs(predictions[, "y_pred"] - data[, "yD"])) / N, 3)
    
    }
  }
}

library(rio)
export(errores, file = "resultados.xlsx")
save(errores, file = "errores.RData")
