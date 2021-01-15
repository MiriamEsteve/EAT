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

## ----efficiencyEAT, eval = FALSE----------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                r = 4,
#                FDH = TRUE,
#                na.rm = TRUE)

