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
t_index <- sample(1:n, n * 0.7)

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
#  frontier(data, tree,
#           x, y,
#           rownames = FALSE)

## ----single_scenario, collapse = FALSE----------------------------------------
# Input indexes
input <- 6

# Output indexes
output <- 3

single_model <- EAT(data = training, 
                    x = input, 
                    y = output)

## ----single_scenario_frontier, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
frontier(data = training, 
         tree = single_model, 
         x = input, 
         y = output, 
         rownames = T)

## ----science_model, collapse = FALSE------------------------------------------
input <- 6:18

output <- 3

science_model <- EAT(data = training, x = input, y = output)

## ----EAT_plot, eval = F-------------------------------------------------------
#  EAT_plot(tree, data,
#           x, y)

## ----science_plot, fig.width = 10.5, fig.height = 9, fig.align = 'center'-----
EAT_plot(tree = science_model, data = training, 
         x = input, y = output)

## ----Breiman, eval = F--------------------------------------------------------
#  M_Breiman(data, tree,
#           x, y,
#           r = 2,
#           threshold = 70)

## ----science_importance, fig.width = 10.5, fig.height = 6, fig.align = 'center'----
M_Breiman(data = training, tree = science_model, 
          x = input, y = output,
          threshold = 75)

## ----science_reduced_model, collapse = FALSE----------------------------------
# Input indexes
input <- c(6, 7, 8, 11, 13, 14, 16, 18)

# Output index
output <- 3

science_reduced_model <- EAT(data = training, x = input, y = output)

## ----science_reduced_model_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
EAT_plot(tree = science_reduced_model, data = training, 
         x = input, y = output)

## ----science_reduced_model_second, collapse = FALSE---------------------------

science_reduced_model_second <- EAT(data = training, x = input, y = output,
                                    numStop = 8)

## ----science_reduced_model_second_plot, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
EAT_plot(tree = science_reduced_model_second, data = training, 
         x = input, y = output)

## ----multioutput--------------------------------------------------------------
# Input indexes
input <- 6:18

# Output indexes
output <- 3:5

multioutput_tree <- EAT(data = training, x = input, y = output,
                        numStop = 8)

## ----plot_multioutput, fig.width = 10.5, fig.height = 8, fig.align = 'center'----
EAT_plot(tree = multioutput_tree, data = training, 
         x = input, y = output)

## ----predict, eval = F--------------------------------------------------------
#  predict(tree, data,
#          x, y)

## ----prediction, collapse = F-------------------------------------------------
predictions <- eat::predict(tree = multioutput_tree,
                            data = test,
                            x = input,
                            y = output)

class(predictions)

## ----efficiency, eval = FALSE-------------------------------------------------
#  efficiency_scores(data, tree,
#                    x, y,
#                    scores_model)

## ----scores,  fig.width = 10.5, fig.height = 8, fig.align = 'center', collapse = FALSE----
efficiency <- efficiency_scores(data = training, 
                                tree = multioutput_tree, 
                                x = input,
                                y = output, 
                                scores_model = "EAT_BCC_output")

efficiency[[2]]
efficiency[[3]]

## ----continent----------------------------------------------------------------
# Continent is a character vector, so we transform it into a factor class
PISAindex$Continent <- as.factor(PISAindex$Continent)

## ----preprocess_factor, error = TRUE, collapse = FALSE------------------------
# Input indexes
input <- c(3, 7:19)

# Output indexes
output <- 6

reading_model <- EAT(data = PISAindex, x = input, y = output)

## ----GDP_PPP_category, eval = FALSE-------------------------------------------
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

## ----narm, error = TRUE, collapse = FALSE-------------------------------------
# Input indexes
x <- 7:19

# Output indexes
y <- 5

reading_model <- EAT(data = PISAindex, x = x, y = y, 
                     na.rm = F)

