#' @title Tuning an Efficiency Analysis Trees model
#'
#' @description This funcion calculates the root mean square error (RMSE) for an Efficiency Analysis Trees model built with a set of given hyperparameters. 
#'
#' @param training Training dataframe or matrix containing the variables for model construction.
#' @param test Test dataframe or matrix containing the variables for model assessment.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Vector. Set of minimum number of observations in a node for a split to be attempted.
#' @param fold Vector. Set of number of folds in which the dataset to apply cross-validation during the pruning is divided.
#' @param max.depth Integer. Depth of the tree.
#' @param max.leaves Integer. Maximum number of leaf nodes.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#' 
#' @importFrom dplyr arrange %>%
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data("PISAindex")
#' 
#' n <- nrow(PISAindex) # Observations in the dataset
#' selected <- sample(1:n, n * 0.7) # Training indexes
#' training <- PISAindex[selected, ] # Training set
#' test <- PISAindex[-selected, ] # Test set
#' 
#' bestEAT(training = training, 
#'         test = test,
#'         x = 6:9,
#'         y = 3,
#'         numStop = c(3, 5, 7),
#'         fold = c(5, 7, 10))
#' }
#'
#' @return Dataframe in which each row corresponds to a given set of hyperparameters and the root mean square error (RMSE).
bestEAT <- function(training, test, x, y, numStop = 5, fold = 5, max.depth = NULL, 
                    max.leaves = NULL, na.rm = TRUE) {

  training <- preProcess(training, x, y, na.rm = na.rm)[[2]]
  test <- preProcess(test, x, y, na.rm = na.rm)[[2]]
  
  if (!identical(sort(names(training)), sort(names(test)))) {
    stop("Different variable names in training and test set")
  }
  
  # Reorder index 'x' and 'y' in data
  x <- 1:(ncol(training) - length(y))
  y <- (length(x) + 1):ncol(training)
  
  # Grid of hyperparameters
  
  if (is.null(max.depth) && is.null(max.leaves)) {
    hp <- expand.grid(numStop = numStop,
                      fold = fold,
                      RMSE = NA)
    
  } else if(!is.null(max.depth)){
    hp <- expand.grid(numStop = numStop,
                      fold = fold,
                      max.depth = max.depth,
                      RMSE = NA)
  } else {
    hp <- expand.grid(numStop = numStop,
                      fold = fold,
                      max.leaves = max.leaves,
                      RMSE = NA)
  }
  
  for (i in 1:nrow(hp)) {
    
    # max.depth & max.leaves
    if (is.null(max.depth) && is.null(max.leaves)){
      max.leaves <- NULL
      max.depth <- NULL
      
    } else if (!is.null(max.depth)) {
      max.leaves <- NULL
      max.depth <- hp[i, "max.depth"]
    
    } else if (!is.null(max.leaves)) {
      max.leaves <- hp[i, "max.leaves"]
      max.depth <- NULL 
      
    } else {
      max.leaves <- hp[i, "max.leaves"]
      max.depth <- hp[i, "max.depth"]
    }
    
    EATmodel <- EAT(data = training, x = x, y = y, numStop = hp[i, "numStop"],
                    fold = hp[i, "fold"], max.depth = max.depth, 
                    max.leaves = max.leaves, na.rm = na.rm)
    
    x.t <- EATmodel[["data"]][["x"]]
    y.t <- EATmodel[["data"]][["y"]]
    
    # RMSE
    
    data.p <- as.data.frame(test[, x.t])
    pred <- data.frame()
    for (j in 1:nrow(data.p)){
      pred <- rbind(pred, predictor(EATmodel[["tree"]], data.p[j, ]))
    }
    
    predictions <- cbind(data.p, pred)

    RMSE <- sqrt(sum((test[, y.t] - predictions[, y.t]) ^ 2) / nrow(test))
    
    hp[i, "RMSE"] <- round(RMSE, 2)
    hp[i, "leaves"] <- EATmodel[["model"]][["leaf_nodes"]]
    
  }
  
  hp <- hp %>% arrange(RMSE)
  
  return(hp)
  
}

#' @title Tuning a Random Forest + Efficiency Analysis Trees model
#'
#' @description This funcion calculates the root mean square error (RMSE) for a Random Forest + Efficiency Analysis Tree model built with a set of given hyperparameters. 
#'
#' @param training Training dataframe or matrix containing the variables for model construction.
#' @param test Test dataframe or matrix containing the variables for model assessment.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Vector. Set of minimum number of observations in a node for a split to be attempted.
#' @param m Vector. Set of number of trees to be built.
#' @param s_mtry Character vector. Set of options for selecting the number of inputs to be selected in each split.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#' 
#' @examples
#' \dontrun{
#' data("PISAindex")
#' 
#' n <- nrow(PISAindex) # Observations in the dataset
#' selected <- sample(1:n, n * 0.7) # Training indexes
#' training <- PISAindex[selected, ] # Training set
#' test <- PISAindex[- selected, ] # Test set
#' 
#' bestRFEAT(training = training, 
#'           test = test,
#'           x = 6:9,
#'           y = 3,
#'           numStop = c(3, 5),
#'           m = c(20, 30),
#'           s_mtry = c("BRM", "1"))
#' }
#' 
#' @export
#'
#' @return Dataframe in which each row corresponds to a given set of hyperparameters and the root mean square error (RMSE).
bestRFEAT <- function(training, test, x, y, numStop = 5, m = 50, 
                      s_mtry = c("5", "BRM"), na.rm = TRUE) {
  
  training <- preProcess(training, x, y, na.rm = na.rm)[[2]]
  test <- preProcess(test, x, y, na.rm = na.rm)[[2]]
  
  if (!identical(sort(names(training)), sort(names(test)))) {
    stop("Different variable names in training and test sets")
  }
  
  # Reorder index 'x' and 'y' in data
  x <- 1:(ncol(training) - length(y))
  y <- (length(x) + 1):ncol(training)
  
  hp <- expand.grid(numStop = numStop,
                    m = m,
                    s_mtry = s_mtry,
                    RMSE = NA)
  
  for (i in 1:nrow(hp)) {
    
    RFEATmodel <- RFEAT(data = training, x = x, y = y, numStop = hp[i, "numStop"],
                        m = hp[i, "m"], s_mtry = as.character(hp[i, "s_mtry"]), na.rm = na.rm)
    
    x.t <- RFEATmodel[["data"]][["x"]]
    y.t <- RFEATmodel[["data"]][["y"]]
    
    # RMSE
    
    data.p <- as.data.frame(test[, x.t])
    pred <- data.frame()
    for (j in 1:nrow(data.p)){
      pred <- rbind(pred, RF_predictor(RFEATmodel[["forest"]], data.p[j, ]))
    }
    
    predictions <- cbind(data.p, pred)
    
    RMSE <- sqrt(sum((test[, y.t] - predictions[, y.t]) ^ 2) / nrow(test))
    
    hp[i, "RMSE"] <- round(RMSE, 2)
    
  }
  
  hp <- hp %>% arrange(RMSE)
  
  return(hp)
}