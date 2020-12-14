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

## ----EAT_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center', eval = FALSE----
#  EAT_plot(object)

## ----science_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center'-----
EAT_plot(object = science_model)

## ----ranking, eval = F--------------------------------------------------------
#  ranking(object,
#          r = 2,
#          barplot = TRUE,
#          threshold = 70)

## ----science_importance, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
ranking(object = science_model,
        r = 2,
        barplot = TRUE,
        threshold = 70)

## ----science_var_reduced_model, collapse = FALSE------------------------------
# Input indexes
input <- c(6, 8, 12, 16, 18)

# Output index
output <- 3

science_var_reduced_model <- EAT(data = training, x = input, y = output)

## ----compare_models, collapse = FALSE-----------------------------------------
cat("Science model --> RMSE: ", science_model[["model"]][["RMSE"]], "| length: ", science_model[["model"]][["nodes"]])

cat("Science variables model reduced --> RMSE: ", science_var_reduced_model[["model"]][["RMSE"]], "| length: ", science_var_reduced_model[["model"]][["nodes"]])

## ----science_reduced_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center'----
EAT_plot(object = science_var_reduced_model)

## ----science_reduced_model_second, collapse = FALSE---------------------------
# Input indexes
input <- c(6, 8, 12, 16, 18)

# Output index
output <- 3

science_var_reduced_numStop <- EAT(data = training, 
                                   x = input, 
                                   y = output,
                                   numStop = 8)

## ----compare_models2, collapse = FALSE----------------------------------------
cat("Science model --> RMSE: ", science_model[["model"]][["RMSE"]], "| length: ", science_model[["model"]][["nodes"]])

cat("Science variables model reduced --> RMSE: ", science_var_reduced_model[["model"]][["RMSE"]], "| length: ", science_var_reduced_model[["model"]][["nodes"]])

cat("Science variable model reduced NumStop --> RMSE: ", science_var_reduced_numStop[["model"]][["RMSE"]], "| length: ", science_var_reduced_numStop[["model"]][["nodes"]])

## ----science_reduced_model_second_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
EAT_plot(object = science_var_reduced_numStop)

## ----smr_model, collapse = FALSE----------------------------------------------
# Input indexes
input <- 6:18

# Output indexes
output <- 3:5

smr_model <- EAT(data = training, 
                 x = input, 
                 y = output,
                 numStop = 6)

## ----smr_model_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
EAT_plot(object = smr_model)

## ----predict, eval = F--------------------------------------------------------
#  predict_EAT(object, newdata)

