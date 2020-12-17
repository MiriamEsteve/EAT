# We simulate a scenario of 15 DMUs with 9 inputs and 1 output.

scores <- data.frame(EAT_BCC_out = rep(NA, 15),
                     EAT_BCC_in = rep(NA, 15),
                     EAT_DDF = rep(NA, 15),
                     EAT_RSL_out = rep(NA, 15),
                     EAT_RSL_in = rep(NA, 15),
                     EAT_WAM = rep(NA, 15),
                     RFEAT_eff = rep(NA, 15),
                     FDH_BCC_out = rep(NA, 15),
                     FDH_BCC_in = rep(NA, 15),
                     FDH_DDF = rep(NA, 15),
                     FDH_RSL_out = rep(NA, 15),
                     FDH_RSL_in = rep(NA, 15),
                     FDH_WAM = rep(NA, 15)) 

# Save seed for reproducibility
set.seed(1234)

# Data generation

data <- eat:::sgle.out_scenario(15, 9)

# Input and output indexes
x <- 1:9
y <- 10

# EAT MODEL
      
EAT <- EAT(data = data,
           x = x,
           y = y)

# EAT BCC out
    
EAT_BCC_out <- efficiency_EAT(data, x, y,
                              EAT,
                              scores_model = "EAT_BCC_out")

scores[, 1] <- EAT_BCC_out$EAT_BCC_out
    
# EAT BCC in

EAT_BCC_in <- efficiency_EAT(data, x, y,
                             EAT,
                             scores_model = "EAT_BCC_in")

scores[, 2] <- EAT_BCC_in$EAT_BCC_in

# EAT DFF
    
EAT_DDF <- efficiency_EAT(data, x, y,
                          EAT,
                          scores_model = "EAT_DDF")
  
scores[, 3] <- EAT_DDF$EAT_DDF

# EAT RSL out

EAT_RSL_out <- efficiency_EAT(data, x, y,
                              EAT,
                              scores_model = "EAT_RSL_out")

scores[, 4] <- EAT_RSL_out$EAT_RSL_out

# EAT RSL in

EAT_RSL_in <- efficiency_EAT(data, x, y,
                             EAT,
                             scores_model = "EAT_RSL_in")

scores[, 5] <- EAT_RSL_in$EAT_RSL_in

# EAT WAM
    
EAT_WAM <- efficiency_EAT(data, x, y,
                          EAT,
                          scores_model = "EAT_WAM")

scores[, 6] <- EAT_WAM$EAT_WAM

# RFEAT MODEL

RFEAT <- RFEAT(data = data,
               x = x,
               y = y,
               m = 50)

# RFEAT efficiency

RFEAT_eff <- efficiency_RFEAT(data, x, y,
                              RFEAT)

scores[, 7] <- RFEAT_eff$scoreRF
      
# FDH MODEL

# FDH BCC out

FDH_BCC_out <- efficiency_FDH(data, x, y,
                              scores_model = "FDH_BCC_out")

scores[, 8] <- FDH_BCC_out$FDH_BCC_out

# FDH BCC in

FDH_BCC_in <- efficiency_FDH(data, x, y,
                             scores_model = "FDH_BCC_in")

scores[, 9] <- FDH_BCC_in$FDH_BCC_in

# FDH DDF

FDH_DDF <- efficiency_FDH(data, x, y,
                          scores_model = "FDH_DDF")

scores[, 10] <- FDH_DDF$FDH_DDF

# FDH RSL out

FDH_RSL_out <- efficiency_FDH(data, x, y,
                              scores_model = "FDH_RSL_out")

scores[, 11] <- FDH_RSL_out$FDH_RSL_out

# FDH RSL in

FDH_RSL_in <- efficiency_FDH(data, x, y,
                             scores_model = "FDH_RSL_in")

scores[, 12] <- FDH_RSL_in$FDH_RSL_in

# FDH WAM

FDH_WAM <- efficiency_FDH(data, x, y,
                          scores_model = "FDH_WAM")

scores[, 13] <- FDH_WAM$FDH_WAM

scores <- cbind(data[, -11], scores)

library(rio)
export(scores, file = "scores.xlsx")
save(scores, file = "scores.RData")
