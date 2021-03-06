## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  warning = FALSE,
  message = FALSE
)

## ----table, echo = FALSE------------------------------------------------------
library(dplyr)

functions <- data.frame("Purpose" = c(rep("Model", 2),
                                      rep("Summarize", 5),
                                      rep("Tune", 2), 
                                      rep("Graph", 3),
                                      rep("Calculate efficiency scores", 3), 
                                      rep("Graph efficiency scores", 2),
                                      rep("Predict", 2), 
                                      rep("Rank", 2)), 
                        "Function name" = c("EAT", "RFEAT",
                                            "print", "summary", "size", "frontier.levels", "descrEAT",
                                            "bestEAT", "bestRFEAT", 
                                            "frontier", "plotEAT", "plotRFEAT",
                                            "efficiencyEAT", "efficiencyCEAT", "efficiencyRFEAT",
                                            "efficiencyDensity", "efficiencyJitter",
                                            "predictEAT", "predictRFEAT",
                                            "rankingEAT", "rankingRFEAT"), 
                        "Usage" = c("Apply Efficiency Analysis Trees technique to a data set. Return an EAT object.",
                                    "Apply Random Forest + Efficiency Analysis Trees technique to a data set. Return a RFEAT object.",
                                    "For an EAT object: print the tree structure of an EAT model. 
                                    For a RFEAT object: print a brief summary of a RFEAT model.",
                                    "For an EAT object: return a summary for the leaf nodes, general information about the model and the error and
                                    threshold for each split and surrogate split.",
                                    "Return the number of leaf nodes for an EAT model.",
                                    "Return the frontier output levels at the leaf nodes for an EAT model.",
                                    "Return measures of centralization and dispersion with respect to the outputs for the nodes of an EAT model.",
                                    "Tune an EAT model.",
                                    "Tune a RFEAT model.",
                                    "Plot the estimated frontier through an EAT model in a low dimensional scenario
                                    (FDH estimated frontier is optional).",
                                    "Plot the tree structure of an EAT model.",
                                    "Shows a line graph with the OOB error on the y-axis, calculated from a forest made up of k trees (x-axis).",
                                    "Calculate DMU efficiency scores through an EAT (and optionally through a FDH) model.",
                                    "Calculate DMU efficiency scores through a convex EAT (and optionally through a DEA) model.",
                                    "Calculate DMU efficiency scores through a RFEAT (and optionally through a FDH) model.",
                                    "Graph a density plot for a data frame of efficiency scores (EAT, FDH, CEAT, DEA and RFEAT are available).",
                                    "Graph a jitter plot for a vector of efficiency scores calculated through an EAT model 
                                    (EAT or CEAT scores are accepted).",
                                    "Predict the output through an EAT model.",
                                    "Predict the output through a RFEAT model.",
                                    "Calculate variable importance scores through an EAT model.",
                                    "Calculate variable importance scores through a RFEAT model.")
)

kableExtra::kable(functions) %>%
  kableExtra::kable_styling("striped", full_width = F) %>%
  kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
  kableExtra::row_spec(c(1:2, 8:9, 13:15, 18:19), background = "#DBFFD6") %>%
  kableExtra::row_spec(c(3:7, 10:12, 16:17, 20:21), background = "#FFFFD1") 

## ----seed---------------------------------------------------------------------
# We save the seed for reproducibility of the results
set.seed(120)

## ----library------------------------------------------------------------------
library(eat)
data("PISAindex")

## ----EAT, eval = FALSE--------------------------------------------------------
#  EAT(data, x, y,
#      fold = 5,
#      numStop = 5,
#      max.depth = NULL,
#      max.leaves = NULL,
#      na.rm = TRUE)

## ----single.output, collapse = FALSE------------------------------------------
single_model <- EAT(data = PISAindex, 
                    x = 15, # input 
                    y = 3) # output

## ----print.single.output, collapse = FALSE------------------------------------
print(single_model)

## ----summary.single.output, collapse = FALSE----------------------------------
summary(single_model)

## ----size.single.output, collapse = FALSE-------------------------------------
size(single_model)

## ----frt.single.output, collapse = FALSE--------------------------------------
frontier.levels(single_model)

## ----perf.single.output, collapse = FALSE-------------------------------------
descriptiveEAT <- descrEAT(single_model)

descriptiveEAT

## ----node.charac, collapse = FALSE--------------------------------------------
single_model[["tree"]][[5]]

## ----table2, echo = FALSE-----------------------------------------------------
types <- data.frame("Variable" = c("Independent variables (inputs)", 
                                   "Dependent variables (outputs)"),
                    "Integer" = c("x", "x"),
                    "Numeric" = c("x", "x"),
                    "Factor" = c("", ""),
                    "Ordered factor" = c("x", ""))

kableExtra::kable(types, align = rep("c", 5)) %>%
  kableExtra::kable_styling("striped", full_width = F)

## ----continent----------------------------------------------------------------
# Transform Continent to Factor
PISAindex_factor_Continent <- PISAindex
PISAindex_factor_Continent$Continent <- as.factor(PISAindex_factor_Continent$Continent)

## ----preprocess_factor, error = TRUE, collapse = FALSE------------------------
error_model <- EAT(data = PISAindex_factor_Continent, 
                   x = c(2, 15), # input
                   y = 3) # output

## ----GDP_PPP_category, collapse = FALSE---------------------------------------
# Cateogirze GDP_PPP into 4 groups: Low, Medium, High, Very High.  
PISAindex_GDP_PPP_cat <- PISAindex
PISAindex_GDP_PPP_cat$GDP_PPP_cat <- cut(PISAindex_GDP_PPP_cat$GDP_PPP,
                                         breaks = c(0, 16.686, 31.419, 47.745, Inf),
                                         include.lowest = T,
                                         labels = c("Low", "Medium", "High", "Very high"))

class(PISAindex_GDP_PPP_cat$GDP_PPP_cat) # "factor" --> error

# It is necessary to indicate order = TRUE, before applying the EAT function

PISAindex_GDP_PPP_cat$GDP_PPP_cat <- factor(PISAindex_GDP_PPP_cat$GDP_PPP_cat, 
                                            order = TRUE)

class(PISAindex_GDP_PPP_cat$GDP_PPP_cat) # "ordered" "factor" --> correct

## ----categorized_model--------------------------------------------------------
categorized_model <- EAT(data = PISAindex_GDP_PPP_cat, 
                         x = c(15, 19), # input
                         y = 3) # output

## ----frontier, eval = FALSE---------------------------------------------------
#  frontier(object,
#           FDH = FALSE,
#           observed.data = FALSE,
#           observed.color = "black",
#           pch = 19,
#           size = 1,
#           rwn = FALSE,
#           max.overlaps = 10)

## ----single.output.frontier, fig.width = 7.2, fig.height = 6------------------
frontier <- frontier(object = single_model,
                     FDH = TRUE, 
                     observed.data = TRUE,
                     rwn = TRUE)

plot(frontier)

## ----single.output.max.depth, collapse = FALSE--------------------------------
single_model_md <- EAT(data = PISAindex, 
                       x = 15, # input 
                       y = 3, # output
                       max.leaves = 5) 

## ----size.single.output_md, collapse = FALSE----------------------------------
size(single_model_md)

## ----pred.single.output_md, collapse = FALSE----------------------------------
single_model_md[["model"]][["y"]]

## ----single.output.frontier_md, fig.width = 7.2, fig.height = 6---------------
frontier_md <- frontier(object = single_model_md,
                        observed.data = TRUE)

plot(frontier_md)

## ----multioutput.scenario, collapse = FALSE-----------------------------------
multioutput_model <- EAT(data = PISAindex, 
                         x = 6:18, # input 
                         y = 3:5 # output
                         ) 

## ----ranking, eval = FALSE----------------------------------------------------
#  rankingEAT(object,
#             barplot = TRUE,
#             threshold = 70,
#             digits = 2)

## ----multioutput.importance, fig.width = 7.2, fig.height = 6------------------
rankingEAT(object = multioutput_model,
           barplot = TRUE,
           threshold = 70,
           digits = 2)

## ----plotEAT, eval = FALSE----------------------------------------------------
#  plotEAT(object)

## ----model.graph1, collapse = FALSE-------------------------------------------
reduced_model1 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), # input
                      y = 3:5, # output
                      numStop = 9)

## ----graph1, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model1)

# Leaf nodes: 8
# Depth: 6

## ----model.graph2, collapse = FALSE-------------------------------------------
reduced_model2 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), # input
                      y = 3:5, # output
                      numStop = 9,
                      max.depth = 5)

## ----graph2, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model2)

# Leaf nodes: 6
# Depth: 5

## ----model.graph3, collapse = FALSE-------------------------------------------
reduced_model3 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), # input
                      y = 3:5, # output
                      numStop = 9,
                      max.leaves = 4)

## ----graph3, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model3)

# Leaf nodes: 4
# Depth: 3

## ----training_test------------------------------------------------------------
n <- nrow(PISAindex) # Observations in the dataset
selected <- sample(1:n, n * 0.7) # Training indexes
training <- PISAindex[selected, ] # Training set
test <- PISAindex[- selected, ] # Test set

## ----bestEAT, eval = FALSE----------------------------------------------------
#  bestEAT(training, test,
#          x, y,
#          numStop = 5,
#          fold = 5,
#          max.depth = NULL,
#          max.leaves = NULL,
#          na.rm = TRUE)

## ----eat.tuning, collapse = FALSE---------------------------------------------
bestEAT(training = training, 
        test = test,
        x = c(6, 7, 8, 12, 17),
        y = 3:5,
        numStop = c(3, 5, 7),
        fold = c(5, 7))

## ----bestEAT_model, collapse = FALSE------------------------------------------
bestEAT_model <- EAT(data = PISAindex,
                     x = c(6, 7, 8, 12, 17),
                     y = 3:5,
                     numStop = 7,
                     fold = 5)

## ----summary.bestEAT_model, collapse = FALSE----------------------------------
summary(bestEAT_model)

## ----efficiencyEAT, eval = FALSE----------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                digits = 3,
#                FDH = TRUE,
#                na.rm = TRUE)

## ----scoresEAT, collapse = FALSE----------------------------------------------
# single_model <- EAT(data = PISAindex, 
                    # x = 15,
                    # y = 3)

scores_EAT <- efficiencyEAT(data = PISAindex,
                            x = 15, 
                            y = 3,
                            object = single_model, 
                            scores_model = "BCC.OUT",
                            digits = 3,
                            FDH = TRUE,
                            na.rm = TRUE)

## ----scoresEAT2, collapse = FALSE---------------------------------------------
scores_EAT2 <- efficiencyEAT(data = PISAindex,
                             x = 15, 
                             y = 3,
                             object = single_model, 
                             scores_model = "BCC.INP",
                             digits = 3,
                             FDH = TRUE,
                             na.rm = TRUE)

## ----efficiencyCEAT, eval = FALSE---------------------------------------------
#  efficiencyCEAT(data, x, y,
#                 object,
#                 score_model,
#                 digits = 3,
#                 DEA = TRUE,
#                 na.rm = TRUE)

## ----scoresCEAT, collapse = FALSE---------------------------------------------
scores_CEAT <- efficiencyCEAT(data = PISAindex,
                              x = 15, 
                              y = 3,
                              object = single_model, 
                              scores_model = "BCC.INP",
                              digits = 3,
                              DEA = TRUE,
                              na.rm = TRUE)

## ----efficiency_jitter, eval = FALSE------------------------------------------
#  efficiencyJitter(object, df_scores,
#                   scores_model,
#                   lwb = NULL, upb = NULL)

## ----jitter_single, collapse = FALSE, fig.width = 7.2, fig.height = 5---------
efficiencyJitter(object = single_model,
                 df_scores = scores_EAT$EAT_BCC_OUT,
                 scores_model = "BCC.OUT",
                 lwb = 1.2)

## ----jitter_single2, collapse = FALSE, fig.width = 7.2, fig.height = 5--------
efficiencyJitter(object = single_model,
                 df_scores = scores_EAT2$EAT_BCC_INP,
                 scores_model = "BCC.INP",
                 upb = 0.65)

## ----frontier_comparar, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
# frontier <- frontier(object = single_model,
                     # FDH = TRUE, 
                     # observed.data = TRUE,
                     # rwn = TRUE)

plot(frontier)

## ----efficiency_density, eval = FALSE-----------------------------------------
#  efficiencyDensity(df_scores,
#                    model = c("EAT", "FDH"))
#  

## ----density_single, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(df_scores = scores_EAT[, 3:4],
                  model = c("EAT", "FDH"))

efficiencyDensity(df_scores = scores_CEAT[, 3:4],
                  model = c("CEAT", "DEA"))


## ----cursed.scores, collapse = FALSE------------------------------------------
# multioutput_model <- EAT(data = PISAindex, 
                         # x = 6:18, 
                         # y = 3:5
                         # ) 

cursed_scores <- efficiencyEAT(data = PISAindex,
                               x = 6:18, 
                               y = 3:5,
                               object = multioutput_model,
                               scores_model = "BCC.OUT",
                               digits = 3,
                               FDH = TRUE)

## ----cursed.density, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(df_scores = cursed_scores[, 17:18],
                  model = c("EAT", "FDH"))


## ----RF, eval = FALSE---------------------------------------------------------
#  RFEAT(data, x, y,
#        numStop = 5, m = 50,
#        s_mtry = "BRM",
#        na.rm = TRUE)

## ----RFmodel------------------------------------------------------------------
forest <- RFEAT(data = PISAindex, 
                x = 6:18, # input 
                y = 3:5, # output
                numStop = 5, 
                m = 30,
                s_mtry = "BRM",
                na.rm = TRUE)

## ----print.RFEAT, collapse = FALSE--------------------------------------------
print(forest)

## ----plot.RFEAT, collapse = FALSE, fig.width = 7.2, fig.height = 6------------
plotRFEAT(forest)

## ----rankingRFEAT, eval = FALSE-----------------------------------------------
#  rankingRFEAT(object,
#               barplot = TRUE,
#               digits = 2,
#  )

## ----RFmodel2-----------------------------------------------------------------
forestReduced <- RFEAT(data = PISAindex, 
                       x = c(6, 7, 8, 12, 17), 
                       y = 3:5,
                       numStop = 5, 
                       m = 30,
                       s_mtry = "BRM",
                       na.rm = TRUE)

## ----rankingRFEAT_forestReduced, fig.width = 7.2, fig.height = 6--------------
rankingRFEAT(object = forestReduced, 
             barplot = TRUE,
             digits = 2)

## ----bestRFEAT, eval = FALSE--------------------------------------------------
#  bestRFEAT(training, test,
#            x, y,
#            numStop = 5,
#            m = 50,
#            s_mtry = c("5", "BRM"),
#            na.rm = TRUE)

## ----tuning.bestRFEAT, collapse = FALSE---------------------------------------
# n <- nrow(PISAindex)
# selected <- sample(1:n, n * 0.7)
# training <- PISAindex[selected, ]
# test <- PISAindex[- selected, ]

bestRFEAT(training = training,
          test = test,
          x = c(6, 7, 8, 12, 17),
          y = 3:5,
          numStop = c(5, 10), # set of possible numStop
          m = c(20, 30), # set of possible m
          s_mtry = c("1", "BRM")) # set of possible s_mtry 

## ----bestModelRFEAT, collapse = FALSE-----------------------------------------
bestRFEAT_model <- RFEAT(data = PISAindex,
                         x = c(6, 7, 8, 12, 17),
                         y = 3:5,
                         numStop = 5,
                         m = 20,
                         s_mtry = "BRM")

## ----eff_scores, eval = FALSE-------------------------------------------------
#  efficiencyRFEAT(data, x, y,
#                  object,
#                  digits = 2,
#                  FDH = TRUE)

## ----scores_RF----------------------------------------------------------------
scoresRF <- efficiencyRFEAT(data = PISAindex,
                            x = c(6, 7, 8, 12, 17), # input
                            y = 3:5, # output
                            object = bestRFEAT_model,
                            FDH = TRUE)

## ----predictEAT, eval = FALSE-------------------------------------------------
#  predictEAT(object, newdata, x)

## ----predictRFEAT, eval = FALSE-----------------------------------------------
#  predictRFEAT(object, newdata, x)

## ----predictions, collapse = FALSE--------------------------------------------
# bestEAT_model <- EAT(data = PISAindex,
                     # x = c(6, 7, 8, 12, 17),
                     # y = 3:5,
                     # numStop = 5,
                     # fold = 5)

# bestRFEAT_model <- EAT(data = PISAindex,
                       # x = c(6, 7, 8, 12, 17),
                       # y = 3:5,
                       # numStop = 3,
                       # m = 30,
                       # s_mtry = 'BRM')

predictions_EAT <- predictEAT(object = bestEAT_model,
                              newdata = PISAindex,
                              x = c(6, 7, 8, 12, 17))

predictions_RFEAT <- predictRFEAT(object = bestRFEAT_model,
                                  newdata = PISAindex,
                                  x = c(6, 7, 8, 12, 17))

## ----EAT_vs_RFEAT_vs_FDH, collapse = FALSE, echo = FALSE----------------------
predictions <- cbind(PISAindex[, 3], PISAindex[, 4], PISAindex[, 5], 
                     predictions_EAT[, 6], predictions_EAT[, 7], predictions_EAT[, 8],
                     predictions_RFEAT[, 6], predictions_RFEAT[, 7], predictions_RFEAT[, 8]) %>%
  as.data.frame()

names(predictions) = c("S_PISA", "R_PISA", "M_PISA",
                       "S_EAT", "R_EAT", "M_EAT",
                       "S_RFEAT", "R_RFEAT", "M_RFEAT")

kableExtra::kable(predictions) %>%
  kableExtra::kable_styling("striped", full_width = F) %>%
  kableExtra::column_spec(c(1, 2, 3), background = "#DBFFD6") %>%
  kableExtra::column_spec(c(4, 5, 6), background = "#FFFFD1") %>%
  kableExtra::column_spec(c(7, 8, 9), background = "#FFCCF9")


## ----newDF, collapse = FALSE--------------------------------------------------
new <- data.frame(NBMC = c(90, 95, 93),
                  WS = c(87, 92, 99),
                  S = c(93, 90, 90),
                  HW = c(90, 91, 92),
                  AAE = c(88, 91, 89))

predictions_EAT <- predictEAT(object = bestEAT_model,
                              newdata = new,
                              x = 1:5)

