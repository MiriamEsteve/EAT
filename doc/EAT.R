## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  warning = FALSE,
  message = FALSE
)

## ----table, echo = FALSE------------------------------------------------------
library(dplyr)

functions <- data.frame("Purpose" = c(rep("modeling", 2), 
                                      rep("tuning", 2), 
                                      rep("plotting", 2),
                                      rep("efficiency scoring", 2), 
                                      rep("scores plotting", 2),
                                      rep("prediction", 3), 
                                      rep("ranking", 2)), 
                        "Function name" = c("EAT", "RFEAT", 
                                            "bestEAT", "bestRFEAT", 
                                            "frontier", "plotEAT", 
                                            "efficiencyEAT", "efficiencyRFEAT",
                                            "efficiencyDensity", "efficiencyJitter",
                                            "predictEAT", "predictRFEAT", "predictFDH",
                                            "rankingEAT", "rankingRFEAT"), 
                        "Usage" = c("Apply Efficiency Analysis Trees technique to a data frame.",
                                    "Apply Random Forest + Efficiency Analysis technique to a data frame.",
                                    "Tune an EAT model.",
                                    "Tune a RFEAT model.",
                                    "Graph the estimated frontier through an EAT model in a low dimensional scenario.
                                    (FDH estimated frontier is optional)",
                                    "Graph a tree structure through an EAT model.",
                                    "Calculate DMU efficiency scores through an EAT model (through an FDH model is optional).",
                                    "Calculate DMU efficiency scores through an RFEAT model (through an FDH model is optional).",
                                    "Graph a density plot for a vector of efficiency scores (EAT, FDH and RFEAT are available).",
                                    "Graph a jitter plot for a vector of efficiency scores calculated through an EAT model.",
                                    "Predict the output through an EAT model.",
                                    "Predict the output through a RFEAT model.",
                                    "Predict the output through a FDH model.",
                                    "Calculate the variable importance through an EAT model.",
                                    "Calculate the variable importance through a RFEAT model.")
                        )

kableExtra::kable(functions) %>%
  kableExtra::kable_styling("striped", full_width = F) %>%
  kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
  kableExtra::row_spec(c(1:2, 5:6, 9:10, 14:15), background = "#DBFFD6") %>%
  kableExtra::row_spec(c(3:4, 7:8, 11:13), background = "#FFFFD1") 

## ----seed---------------------------------------------------------------------
# We save the seed for reproducibility of the results
set.seed(120)

## ----library------------------------------------------------------------------
library(eat)
data("PISAindex")

## ----EAT, eval = F------------------------------------------------------------
#  EAT(data, x, y,
#      fold = 5,
#      numStop = 5,
#      na.rm = TRUE)

## ----frontier, eval = F-------------------------------------------------------
#  frontier(object,
#           FDH = FALSE,
#           train.data = FALSE,
#           train.color = "black",
#           pch = 19,
#           rwn = FALSE,
#           size = 1.5)

## ----single.output, collapse = FALSE------------------------------------------
input <- 15

output <- 3

single_model <- EAT(data = PISAindex, 
                    x = input, 
                    y = output)

## ----print.single.output, collapse = FALSE------------------------------------
print(single_model)

## ----node.charac, collapse = FALSE--------------------------------------------
single_model[["tree"]][[5]]

## ----single.output.frontier, fig.width = 9.7, fig.height = 6, fig.align = 'center'----
frontier <- frontier(object = single_model,
                     FDH = TRUE, 
                     train.data = TRUE,
                     rwn = TRUE)

plot(frontier)

## ----multioutput.scenario, collapse = FALSE-----------------------------------
input <- 6:18

output <- 3:5

multioutput_model <- EAT(data = PISAindex, 
                         x = input, 
                         y = output)

## ----ranking, eval = F--------------------------------------------------------
#  rankingEAT(object,
#             r = 2,
#             barplot = TRUE,
#             threshold = 70)

## ----multioutput.importance, fig.width = 9.7, fig.height = 6, fig.align = 'center'----
rankingEAT(object = multioutput_model,
           r = 2,
           barplot = TRUE,
           threshold = 70)

## ----plotEAT, eval = FALSE----------------------------------------------------
#  plotEAT(object)

## ----model.graph1, collapse = FALSE-------------------------------------------
input <- c(6, 7, 8, 12, 17)
output <- 3:5

best_multioutput <- EAT(data = PISAindex, 
                        x = input, 
                        y = output,
                        numStop = 8,
                        fold = 6)

## ----graph1, fig.width = 9.7, fig.height = 7.5, fig.align = 'center'----------
plotEAT(object = best_multioutput)

## ----model.graph2, collapse = FALSE-------------------------------------------
input <- c(6, 8, 12, 16, 18)
output <- 3

best_multioutput2 <- EAT(data = PISAindex, 
                        x = input, 
                        y = output)

## ----graph2, fig.width = 9.7, fig.height = 7.5, fig.align = 'center'----------
plotEAT(object = best_multioutput2)

## ----training_test------------------------------------------------------------
n <- nrow(PISAindex) # Observations in the dataset

t_index <- sample(1:n, n * 0.7) # Training indexes

training <- PISAindex[t_index, ] # Training set

test <- PISAindex[-t_index, ] # Test set

## ----bestEAT, eval = F--------------------------------------------------------
#  bestEAT(training, test,
#          x, y,
#          numStop,
#          fold,
#          na.rm)

## ----eat.tuning1, collapse = FALSE, eval = FALSE------------------------------
#  bestEAT(training = training,
#          test = test,
#          x = 6:18,
#          y = 3:5,
#          numStop = c(3, 5, 7, 10),
#          fold = c(3, 5, 7))

## ----eat.tuning2, collapse = FALSE, eval = FALSE------------------------------
#  bestEAT(training = training,
#          test = test,
#          x = c(6, 7, 8, 12, 17),
#          y = 3:5,
#          numStop = c(3, 5, 7, 10),
#          fold = c(3, 5, 7))

## ----efficiency, eval = FALSE-------------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                r = 4)

## ----scores, collapse = FALSE, eval = FALSE-----------------------------------
#  scores_EAT <- efficiencyEAT(data = training,
#                              x = 6,
#                              y = 3,
#                              object = single_model,
#                              scores_model = "EAT_BCC_out",
#                              r = 4)
#  
#  scores_FDH <- efficiencyFDH(data = training,
#                              x = 6,
#                              y = 3,
#                              scores_model = "FDH_BCC_out",
#                              r = 4)

## ----efficiency_jitter, eval = FALSE------------------------------------------
#  efficiencyJitter(object, scores_EAT,
#                   scores_model,
#                   lwb = NULL, upb = NULL)

## ----jitter_single, collapse = FALSE, fig.width = 10.5, fig.height = 8, fig.align = 'center', eval = FALSE----
#  efficiencyJitter(object = single_model,
#                   scores_EAT = scores_EAT$EAT_BCC_out,
#                   scores_model = "EAT_BCC_out",
#                   lwb = 1.2)

## ----frontier_comparar, fig.width = 10.5, fig.height = 6, fig.align = 'center', eval = FALSE----
#  plot(frontier)

## ----efficiency_density, eval = F---------------------------------------------
#  efficiencyDensity(scores_EAT,
#                    scores_FDH = NULL)
#  

## ----density_single, collapse = FALSE, fig.width = 10.5, fig.height = 8, fig.align = 'center', eval = FALSE----
#  efficiencyDensity(scores_EAT = scores_EAT$EAT_BCC_out,
#                    scores_FDH = scores_FDH$FDH_BCC_out)
#  

## ----predict, eval = F--------------------------------------------------------
#  predictEAT(object, newdata)

## ----prediction, collapse = F, eval = FALSE-----------------------------------
#  predictions_EAT <- predictEAT(object = smr_model,
#                               newdata = test[, input])

## ----RF, eval = FALSE---------------------------------------------------------
#  RFEAT(data, x, y,
#        numStop = 5, m = 50,
#        s_mtry = "Breiman",
#        na.rm = TRUE)

## ----RF_data, eval = FALSE----------------------------------------------------
#  input <- 6:18
#  output <- 3:5
#  
#  forest <- RFEAT(data = training,
#                  x = input, y = output,
#                  numStop = 7, m = 5,
#                  s_mtry = "Breiman",
#                  na.rm = TRUE)

## ----predict_EAT, eval = FALSE------------------------------------------------
#  predictRFEAT(object, newdata)

## ----predict_EAT_ex, eval = FALSE---------------------------------------------
#  predictions_RFEAT <- predictRFEAT(forest, test[, input])

## ----EAT_vs_RFEAT, eval = FALSE-----------------------------------------------
#  names(predictions_EAT)[14:16] <- c("S_EAT", "R_EAT", "M_EAT")
#  names(predictions_RFEAT)[14:16] <- c("S_RFEAT", "R_RFEAT", "M_RFEAT")
#  
#  EAT_vs_RFEAT <- cbind(test[, 3:5], predictions_EAT[, 14:16], predictions_RFEAT[, 14:16])
#  
#  print(EAT_vs_RFEAT)

## ----eff_scores, eval = FALSE-------------------------------------------------
#  efficiencyRFEAT(data = training,
#                  x = input,
#                  y = output,
#                  object = forest)

## ----scores_RF, eval = FALSE--------------------------------------------------
#  scoresRF <- efficiencyRFEAT(data = training,
#                              x = input,
#                              y = output,
#                              object = forest)

## ----ranking_RFEAT, eval = FALSE----------------------------------------------
#  rankingRFEAT(object, r = 2,
#                barplot = TRUE)

## ----ranking_RFEAT_ex, fig.width = 10.5, fig.height = 8, fig.align = 'center', eval = FALSE----
#  rankingRFEAT(object = forest, r = 2,
#               barplot = TRUE)

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

