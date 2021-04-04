#' @title Model Prediction for Efficiency Analysis Trees.
#'
#' @description This function predicts the expected output by an EAT object.
#'
#' @param object An EAT object.
#' @param newdata Dataframe. Set of input variables to predict on.
#' @param x Inputs index.
#'
#' @importFrom dplyr %>%
#'
#' @return Data frame with the original data and the predicted values.
#' 
#' @examples
#' 
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' predictEAT(object = EAT_model, newdata = simulated, x = c(1,2))
#' 
#' @export
predictEAT <- function(object, newdata, x) {
  
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
    
  }
  
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)[x]
  
  if (!identical(sort(train_names), sort(test_names))) {
   stop("Different variable names in training set and test set.")
  }
  
  newdata <- as.data.frame(newdata[, x])
  
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
  
  predictions <- cbind(newdata, predictions)
  
  return(predictions)
}


#' @title Model prediction for Random Forest + Efficiency Analysis Trees model.
#'
#' @description This function predicts the expected output by a RFEAT object.
#'
#' @param object A RFEAT object.
#' @param newdata Dataframe. Set of input variables to predict on.
#' @param x Inputs index
#'
#' @importFrom dplyr %>%
#'
#' @return Data frame with the original data and the predicted values.
#' 
#' @examples 
#' \dontrun{
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' RFEAT_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' predictRFEAT(object = RFEAT_model, newdata = simulated, x = c(1,2))
#' }
#' 
#' @export
predictRFEAT <- function(object, newdata, x) {
  
  if (class(object) != "RFEAT"){
    stop(paste(deparse(substitute(object)), "must be a RFEAT object"))
    
  }
  
  newdata <- newdata[,x]
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)
  
  if (!identical(sort(train_names), sort(test_names))) {
    stop("Different variable names in training and test set")
  }
  
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
  
  predictions <- cbind(newdata, predictions)
  
  return(predictions)
}