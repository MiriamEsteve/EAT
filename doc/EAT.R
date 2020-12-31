## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  warning = FALSE,
  message = FALSE
)

## ----seed---------------------------------------------------------------------
# We save the seed for reproducibility of the results
set.seed(120)

## ----library------------------------------------------------------------------
library(eat)
data("PISAindex")

## ----training_test------------------------------------------------------------
# We split into training and test datasets to assess the models

# Observations in the dataset
n <- nrow(PISAindex)

# Training indexes
t_index <- sample(1:n, n * 0.8)

# Training set
training <- PISAindex[t_index, ]

# Test set
test <- PISAindex[-t_index, ]

## ----EAT, eval = F------------------------------------------------------------
#  EAT(data, x, y,
#      fold = 5,
#      numStop = 5,
#      na.rm = TRUE)

## ----frontier, eval = F-------------------------------------------------------
#  frontier(object,
#           train.data = FALSE,
#           train.color = "black",
#           pch = 19,
#           rwn = FALSE,
#           size = 1.5)

## ----single_scenario, collapse = FALSE----------------------------------------
# Input indexes
input <- 6

# Output indexes
output <- 3

# Modeling
single_model <- EAT(data = training, 
                    x = input, 
                    y = output)

print(single_model)

## ----single_scenario_frontier, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
frontier <- frontier(object = single_model,
                     train.data = TRUE,
                     train.color = "#F8766D",
                     rwn = TRUE,
                     size = 1.5)

plot(frontier)

## ----single_scenario_frontier_test, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
frontier +
  ggplot2::geom_point(data = test, ggplot2::aes(x = NBMC, y = S_PISA))

## ----science_model, collapse = FALSE------------------------------------------
input <- 6:18

output <- 3

science_model <- EAT(data = training, x = input, y = output)

print(science_model)

## ----plotEAT, fig.width = 10.5, fig.height = 9, fig.align = 'center', eval = FALSE----
#  plotEAT(object)

## ----science_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center'-----
plotEAT(object = science_model)

## ----ranking, eval = F--------------------------------------------------------
#  rankingEAT(object,
#             r = 2,
#             barplot = TRUE,
#             threshold = 70)

## ----science_importance, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
rankingEAT(object = science_model,
           r = 2,
           barplot = TRUE,
           threshold = 70)

## ----science_var_reduced_model, collapse = FALSE------------------------------
# Input indexes
input <- c(6, 8, 12, 16, 18)

# Output index
output <- 3

science_var_reduced_model <- EAT(data = training, x = input, y = output)

print(science_var_reduced_model)

## ----compare_models, collapse = FALSE-----------------------------------------
cat("Science model --> RMSE: ", science_model[["model"]][["RMSE"]], "| length: ", science_model[["model"]][["nodes"]])

cat("Science variables model reduced --> RMSE: ", science_var_reduced_model[["model"]][["RMSE"]], "| length: ", science_var_reduced_model[["model"]][["nodes"]])

## ----science_reduced_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center'----
plotEAT(object = science_var_reduced_model)

## ----science_reduced_model_second, collapse = FALSE---------------------------
# Input indexes
input <- c(6, 8, 12, 16, 18)

# Output index
output <- 3

science_var_reduced_numStop <- EAT(data = training, 
                                   x = input, 
                                   y = output,
                                   numStop = 8)

print(science_var_reduced_numStop)

## ----compare_models2, collapse = FALSE----------------------------------------
cat("Science model --> RMSE: ", science_model[["model"]][["RMSE"]], "| length: ", science_model[["model"]][["nodes"]])

cat("Science variables model reduced --> RMSE: ", science_var_reduced_model[["model"]][["RMSE"]], "| length: ", science_var_reduced_model[["model"]][["nodes"]])

cat("Science variable model reduced NumStop --> RMSE: ", science_var_reduced_numStop[["model"]][["RMSE"]], "| length: ", science_var_reduced_numStop[["model"]][["nodes"]])

## ----science_reduced_model_second_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
plotEAT(object = science_var_reduced_numStop)

## ----smr_model, collapse = FALSE----------------------------------------------
# Input indexes
input <- 6:18

# Output indexes
output <- 3:5

smr_model <- EAT(data = training, 
                 x = input, 
                 y = output,
                 numStop = 7)

print(smr_model)

## ----smr_model_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
plotEAT(object = smr_model)

## ----predict, eval = F--------------------------------------------------------
#  predictEAT(object, newdata)

## ----prediction, collapse = F-------------------------------------------------
predictions_EAT <- predictEAT(object = smr_model,
                             newdata = test[, input])

## ----efficiency, eval = FALSE-------------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                r = 4)
#  
#  efficiencyFDH(data, x, y,
#                score_model,
#                r = 4)

## ----scores, collapse = FALSE-------------------------------------------------
scores_EAT <- efficiencyEAT(data = training,
                            x = 6, 
                            y = 3,
                            object = single_model, 
                            scores_model = "EAT_BCC_out",
                            r = 4)

scores_FDH <- efficiencyFDH(data = training,
                            x = 6, 
                            y = 3,
                            scores_model = "FDH_BCC_out",
                            r = 4)

## ----efficiency_jitter, eval = FALSE------------------------------------------
#  efficiencyJitter(object, scores_EAT,
#                   scores_model,
#                   lwb = NULL, upb = NULL)

## ----jitter_single, collapse = FALSE, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
efficiencyJitter(object = single_model,
                 scores_EAT = scores_EAT$EAT_BCC_out,
                 scores_model = "EAT_BCC_out",
                 lwb = 1.2)

## ----frontier_comparar, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
plot(frontier)

## ----efficiency_density, eval = F---------------------------------------------
#  efficiencyDensity(scores_EAT,
#                    scores_FDH = NULL)
#  

## ----density_single, collapse = FALSE, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
efficiencyDensity(scores_EAT = scores_EAT$EAT_BCC_out,
                  scores_FDH = scores_FDH$FDH_BCC_out)


## ----RF, eval = FALSE---------------------------------------------------------
#  RFEAT(data, x, y,
#        numStop = 5, m = 50,
#        s_mtry = "Breiman",
#        na.rm = TRUE)

## ----RF_data------------------------------------------------------------------
input <- 6:18
output <- 3:5

forest <- RFEAT(data = training, 
                x = input, y = output,
                numStop = 7, m = 5,
                s_mtry = "Breiman",
                na.rm = TRUE)

## ----predict_EAT, eval = FALSE------------------------------------------------
#  predictRFEAT(object, newdata)

## ----predict_EAT_ex-----------------------------------------------------------
predictions_RFEAT <- predictRFEAT(forest, test[, input])

## ----EAT_vs_RFEAT-------------------------------------------------------------
names(predictions_EAT)[14:16] <- c("S_EAT", "R_EAT", "M_EAT")
names(predictions_RFEAT)[14:16] <- c("S_RFEAT", "R_RFEAT", "M_RFEAT")

EAT_vs_RFEAT <- cbind(test[, 3:5], predictions_EAT[, 14:16], predictions_RFEAT[, 14:16])

print(EAT_vs_RFEAT)

## ----eff_scores, eval = FALSE-------------------------------------------------
#  efficiencyRFEAT(data = training,
#                  x = input,
#                  y = output,
#                  object = forest)

## ----scores_RF----------------------------------------------------------------
scoresRF <- efficiencyRFEAT(data = training,
                            x = input,
                            y = output,
                            object = forest)

## ----ranking_RFEAT, eval = FALSE----------------------------------------------
#  rankingRFEAT(object, r = 2,
#                barplot = TRUE)

## ----ranking_RFEAT_ex, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
rankingRFEAT(object = forest, r = 2,
             barplot = TRUE)

## ----continent, eval = F------------------------------------------------------
#  # Continent is a character vector, so we transform it into a factor class
#  PISAindex$Continent <- as.factor(PISAindex$Continent)

## ----preprocess_factor, error = TRUE, collapse = FALSE, eval = F--------------
#  # Input indexes
#  input <- c(3, 7:19)
#  
#  # Output indexes
#  output <- 6
#  
#  reading_model <- EAT(data = PISAindex, x = input, y = output)

## ----GDP_PPP_category, eval = FALSE, eval = F---------------------------------
#  PISAindex$GDP_PPP_cat <- cut(PISAindex$GDP_PPP,
#                              breaks = c(0, 16.686, 31.419, 47.745, Inf),
#                              include.lowest = T,
#                              labels = c("Low", "Medium", "High", "Very high"))
#  
#  class(PISAindex$GDP_PPP_cat)
#  
#  # It is very important indicate order = T, before EAT function
#  
#  PISAindex$GDP_PPP_cat <- factor(PISAindex$GDP_PPP_cat, order = T)
#  
#  class(PISAindex$GDP_PPP_cat)

## ----categorized_model, eval = FALSE------------------------------------------
#  # Input indexes
#  input <- c(7:18, 20)
#  
#  # Output indexes
#  output <- 6
#  
#  categorized_model <- EAT(data = PISAindex, x = input, y = output,
#                           numStop = 15)

## ----narm, error = TRUE, collapse = FALSE, eval = F---------------------------
#  # Input indexes
#  x <- 7:19
#  
#  # Output indexes
#  y <- 5
#  
#  reading_model <- EAT(data = PISAindex, x = x, y = y,
#                       na.rm = F)

