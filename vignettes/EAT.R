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
                                      rep("efficiency scoring", 3), 
                                      rep("scores plotting", 2),
                                      rep("prediction", 3), 
                                      rep("ranking", 2)), 
                        "Function name" = c("EAT", "RFEAT", 
                                            "bestEAT", "bestRFEAT", 
                                            "frontier", "plotEAT", 
                                            "efficiencyEAT", "efficiencyCEAT", "efficiencyRFEAT",
                                            "efficiencyDensity", "efficiencyJitter",
                                            "predictEAT", "predictRFEAT", "predictFDH",
                                            "rankingEAT", "rankingRFEAT"), 
                        "Usage" = c("Apply Efficiency Analysis Trees technique to a data frame.",
                                    "Apply Random Forest + Efficiency Analysis technique to a data frame.",
                                    "Tune an EAT model.",
                                    "Tune a RFEAT model.",
                                    "Graph the estimated frontier through an EAT model in a low dimensional scenario
                                    (FDH estimated frontier is optional).",
                                    "Graph a tree structure through an EAT model.",
                                    "Calculate DMU efficiency scores through an EAT model (through an FDH model is optional).",
                                    "Calculate DMU efficiency scores through a convexified  EAT model (through an DEA model is optional).",
                                    "Calculate DMU efficiency scores through an RFEAT model (through an FDH model is optional).",
                                    "Graph a density plot for a vector of efficiency scores (EAT, FDH, DEA and RFEAT are available).",
                                    "Graph a jitter plot for a vector of efficiency scores calculated through an EAT model.",
                                    "Predict the output through an EAT model.",
                                    "Predict the output through a RFEAT model.",
                                    "Predict the output through a FDH model.",
                                    "Calculate variable importance scores through an EAT model.",
                                    "Calculate variable importance scores through a RFEAT model.")
                        )

kableExtra::kable(functions) %>%
  kableExtra::kable_styling("striped", full_width = F) %>%
  kableExtra::collapse_rows(columns = 1, valign = "middle") %>%
  kableExtra::row_spec(c(1:2, 5:6, 10:11, 15:16), background = "#DBFFD6") %>%
  kableExtra::row_spec(c(3:4, 7:9, 12:14), background = "#FFFFD1") 

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

## ----node.charac, collapse = FALSE--------------------------------------------
single_model[["tree"]][[5]]

## ----table2, echo = FALSE-----------------------------------------------------
types <- data.frame("Variable" = c("Independent variables (inputs)", "Dependent variables (outputs)"),
                    "Integer" = c("x", "x"),
                    "Double" = c("x", "x"),
                    "Numeric" = c("x", "x"),
                    "Factor" = c("", ""),
                    "Ordered factor" = c("x", ""))

kableExtra::kable(types, align = rep("c", 6)) %>%
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

# It is necessary to indicate order = T, before the EAT function

PISAindex_GDP_PPP_cat$GDP_PPP_cat <- factor(PISAindex_GDP_PPP_cat$GDP_PPP_cat, order = T)

class(PISAindex_GDP_PPP_cat$GDP_PPP_cat) # "ordered" "factor" --> correct

## ----categorized_model--------------------------------------------------------
categorized_model <- EAT(data = PISAindex_GDP_PPP_cat, 
                         x = c(15, 19), # input
                         y = 3) # output

## ----frontier, eval = F-------------------------------------------------------
#  frontier(object,
#           FDH = FALSE,
#           train.data = FALSE,
#           train.color = "black",
#           pch = 19,
#           rwn = FALSE,
#           size = 1.5)

## ----single.output.frontier, fig.width = 7.2, fig.height = 6------------------
frontier <- frontier(object = single_model,
                     FDH = TRUE, 
                     train.data = TRUE,
                     rwn = TRUE)

plot(frontier)

## ----multioutput.scenario, collapse = FALSE-----------------------------------
multioutput_model <- EAT(data = PISAindex, 
                         x = 6:18, # input 
                         y = 3:5) # output

## ----ranking, eval = FALSE----------------------------------------------------
#  rankingEAT(object,
#             r = 2,
#             barplot = TRUE,
#             threshold = 70)

## ----multioutput.importance, fig.width = 7.2, fig.height = 6------------------
rankingEAT(object = multioutput_model,
           r = 2,
           barplot = TRUE,
           threshold = 70)

## ----plotEAT, eval = FALSE----------------------------------------------------
#  plotEAT(object)

## ----model.graph1, collapse = FALSE-------------------------------------------
best_multioutput <- EAT(data = PISAindex, 
                        x = c(6, 7, 8, 12, 17), # input
                        y = 3:5, # output
                        numStop = 8,
                        fold = 6)

## ----graph1, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = best_multioutput)

## ----model.graph2, collapse = FALSE-------------------------------------------
best_multioutput2 <- EAT(data = PISAindex, 
                        x = c(6, 8, 12, 16, 18), # input
                        y = 3) # output

## ----graph2, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = best_multioutput2)

## ----training_test------------------------------------------------------------
n <- nrow(PISAindex) # Observations in the dataset
t_index <- sample(1:n, n * 0.7) # Training indexes
training <- PISAindex[t_index, ] # Training set
test <- PISAindex[-t_index, ] # Test set

## ----bestEAT, eval = FALSE----------------------------------------------------
#  bestEAT(training, test,
#          x, y,
#          numStop,
#          fold,
#          na.rm)

## ----eat.tuning1, collapse = FALSE--------------------------------------------
bestEAT(training = training, 
        test = test,
        x = 6:18,
        y = 3:5,
        numStop = c(3, 5, 7, 10),
        fold = c(5, 7))

## ----eat.tuning2, collapse = FALSE--------------------------------------------
bestEAT(training = training, 
        test = test,
        x = c(6, 7, 8, 12, 17),
        y = 3:5,
        numStop = c(3, 5, 7, 10),
        fold = c(5, 7))

## ----bestmodel, collapse = FALSE----------------------------------------------
bestmodel <- EAT(data = PISAindex,
                 x = c(6, 7, 8, 12, 17),
                 y = 3:5,
                 numStop = 3,
                 fold = 5)

## ----summary.bestmodel, collapse = FALSE--------------------------------------
summary(bestmodel)

## ----efficiencyEAT, eval = FALSE----------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                r = 2,
#                FDH = TRUE,
#                na.rm = TRUE)

## ----scoresEAT, collapse = FALSE----------------------------------------------
scores_EAT <- efficiencyEAT(data = PISAindex,
                            x = 15, 
                            y = 3,
                            object = single_model, 
                            scores_model = "BCC_out",
                            r = 3,
                            FDH = TRUE)

## ----scoresEAT2, collapse = FALSE---------------------------------------------
scores_EAT2 <- efficiencyEAT(data = PISAindex,
                            x = 15, 
                            y = 3,
                            object = single_model, 
                            scores_model = "BCC_in",
                            r = 3)

## ----efficiencyCEAT, eval = FALSE---------------------------------------------
#  efficiencyCEAT(data, x, y,
#                 object,
#                 score_model,
#                 r = 3,
#                 DEA = TRUE,
#                 na.rm = TRUE)

## ----scoresCEAT, collapse = FALSE---------------------------------------------
efficiencyCEAT(data = PISAindex,
               x = 15, 
               y = 3,
               object = single_model, 
               scores_model = "BCC_out",
               r = 3,
               DEA = TRUE)

## ----efficiency_jitter, eval = FALSE------------------------------------------
#  efficiencyJitter(object, scores_EAT,
#                   scores_model,
#                   lwb = NULL, upb = NULL)

## ----jitter_single, collapse = FALSE, fig.width = 7.2, fig.height = 5---------
efficiencyJitter(object = single_model,
                 scores_EAT = scores_EAT$EAT_BCC_out,
                 scores_model = "BCC_out",
                 lwb = 1.2)

## ----jitter_single2, collapse = FALSE, fig.width = 7.2, fig.height = 5--------
efficiencyJitter(object = single_model,
                 scores_EAT = scores_EAT2$EAT_BCC_in,
                 scores_model = "BCC_in",
                 upb = 0.65)

## ----frontier_comparar, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
plot(frontier)

## ----efficiency_density, eval = F---------------------------------------------
#  efficiencyDensity(scores_EAT,
#                    scores_FDH = NULL,
#                    scores_RFEAT = NULL)
#  

## ----density_single, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(scores_EAT = scores_EAT$EAT_BCC_out,
                  scores_FDH = scores_EAT$FDH_BCC_out,
                  scores_RFEAT = NULL)


## ----cursed.scores, collapse = FALSE------------------------------------------
cursed_scores <- efficiencyEAT(data = PISAindex,
                               x = 6:18, 
                               y = 3:5,
                               object = multioutput_model, 
                               scores_model = "BCC_out",
                               r = 3,
                               FDH = TRUE)

## ----cursed.density, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(scores_EAT = cursed_scores$EAT_BCC_out,
                  scores_FDH = cursed_scores$FDH_BCC_out)


## ----RF, eval = FALSE---------------------------------------------------------
#  RFEAT(data, x, y,
#        numStop = 5, m = 50,
#        s_mtry = "Breiman",
#        na.rm = TRUE)

## ----RFmodel------------------------------------------------------------------
forest <- RFEAT(data = PISAindex, 
                x = 6:18, # input 
                y = 5, # output
                numStop = 3, 
                m = 50,
                s_mtry = "Breiman",
                na.rm = TRUE)


## ----print.RFEAT, collapse = FALSE--------------------------------------------
print(forest)

## ----rankingRFEAT, eval = FALSE-----------------------------------------------
#  rankingRFEAT(object, r = 2,
#                barplot = TRUE)

## ----rankingRFEAT_ex, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
rankingRFEAT(object = forest, r = 2,
             barplot = TRUE)

## ----bestRFEAT, eval = FALSE--------------------------------------------------
#  bestRFEAT(training, test,
#            x, y,
#            numStop,
#            m,
#            s_mtry,
#            na.rm = TRUE)

## ----tuning.bestRFEAT, collapse = FALSE---------------------------------------
bestRFEAT(training = training, 
        test = test,
        x = 6:18,
        y = 3:5,
        numStop = c(3, 5, 10),
        m = c(30, 40, 50, 60),
        s_mtry = c("Breiman", "2", "5"))

## ----eff_scores, eval = FALSE-------------------------------------------------
#  efficiencyRFEAT(data, x, y,
#                  object,
#                  r = 2,
#                  FDH = TRUE)

## ----scores_RF----------------------------------------------------------------
scoresRF <- efficiencyRFEAT(data = PISAindex,
                            x = 6:18, # input
                            y = 5, # output
                            object = forest,
                            FDH = TRUE)

## ----predictEAT, eval = F-----------------------------------------------------
#  predictEAT(object, newdata)

## ----predictRFEAT, eval = FALSE-----------------------------------------------
#  predictRFEAT(object, newdata)

## ----predictFDH, eval = FALSE-------------------------------------------------
#  predictFDH(data, x, y)

## ----models, collapse = FALSE-------------------------------------------------
input <- c(6, 7, 8, 12, 17)
output <- 3:5

which(is.na(PISAindex), arr.ind = TRUE)

# FDH does not accept NA rows, so we exclude ESP

EAT_model <- EAT(data = PISAindex[- 32, ],
                 x = input,
                 y = output)

RFEAT_model <- RFEAT(data = PISAindex[- 32, ],
                     x = input,
                     y = output)

## ----predictions, collapse = FALSE--------------------------------------------
predictions_EAT <- predictEAT(object = EAT_model,
                              newdata = PISAindex[- 32, input])

predictions_RFEAT <- predictRFEAT(object = RFEAT_model,
                                  newdata = PISAindex[- 32, input])

predictions_FDH <- predictFDH(data = PISAindex[- 32, ],
                              x = input,
                              y = output)

