#' @title Model Prediction for Efficiency Analysis Trees.
#'
#' @description This function predicts the expected output by an \code{EAT} object.
#'
#' @param object An \code{EAT} object.
#' @param newdata \code{data.frame}. Set of input variables to predict on.
#' @param x Inputs index.
#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom dplyr %>%
#'
#' @return \code{data.frame} with the predicted values.
#' 
#' @examples 
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' predict(object = EAT_model, newdata = simulated, x = c(1, 2))
#' }
#' 
#' @export
predict.EAT <- function(object, newdata, x, ...) {
  
  if (!inherits(object, "EAT")){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
  }
  
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)[x]
  
  if (!identical(sort(train_names), sort(test_names))) {
    stop("Different variable names in training set and test set.")
  }
  
  # Select variables and reorder as in training data
  newdata <- newdata[, x, drop = FALSE][train_names]
  
  y <- object[["data"]][["y"]] 
  
  tree <- object[["tree"]]
  
  predictions <- c()
  
  for(register in 1:nrow(newdata)){
    ti <- 1
    
    while (tree[[ti]][["SL"]] != -1) {
      if (newdata[register, ][tree[[ti]][["xi"]]] < as.data.frame(tree[[ti]][["s"]])) {
        ti <- posIdNode(tree, tree[[ti]][["SL"]])
      } else {
        ti <- posIdNode(tree, tree[[ti]][["SR"]])
      }
    }
    predictions <- append(predictions, unlist(tree[[ti]][["y"]]))
  }
  
  predictions <- matrix(predictions, ncol = length(y), byrow = T) %>%
    as.data.frame()
  
  names(predictions) <- paste(object[["data"]][["output_names"]],"_pred", sep = "")
  
  return(predictions)
}

#' @title Model prediction for Random Forest + Efficiency Analysis Trees model.
#'
#' @description This function predicts the expected output by a \code{RFEAT} object.
#'
#' @param object A \code{RFEAT} object.
#' @param newdata \code{data.frame}. Set of input variables to predict on.
#' @param x Inputs index.
#' @param ... further arguments passed to or from other methods.
#'
#' @importFrom dplyr %>%
#'
#' @return \code{data.frame} with the predicted values.
#' 
#' @examples 
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' RFEAT_model <- RFEAT(data = simulated, x = c(1, 2), y = c(3, 4))
#' 
#' predict(object = RFEAT_model, newdata = simulated, x = c(1, 2))
#' }
#' @export
predict.RFEAT <- function(object, newdata, x, ...) {
  
  if (!inherits(object, "RFEAT")){
    stop(paste(deparse(substitute(object)), "must be a RFEAT object"))
  }
  
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)[x]
  
  if (!identical(sort(train_names), sort(test_names))) {
    stop("Different variable names in training and test set")
  }
  
  # Select variables and reorder as in training data
  newdata <- newdata[, x, drop = FALSE][train_names]
  
  y <- object[["data"]][["y"]] 
  forest <- object[["forest"]]
  m <- object[["control"]][["m"]]
  
  predictions <- data.frame()
  
  for(register in 1:nrow(newdata)){
    y_result <- rep(list(list()), m)
    
    for(tree in 1:m){
      y_result[[tree]] <- predictor(forest[[tree]], newdata[register, ])
    }
    
    y_result <- as.data.frame(matrix(unlist(y_result), nrow = length(unlist(y_result[1]))))
    y_result <- apply(y_result, 1, "mean")
    
    predictions <- rbind(predictions, y_result)
    
  }
  
  names(predictions) <-  paste(object[["data"]][["output_names"]], "_pred", sep = "")
  
  return(predictions)
}