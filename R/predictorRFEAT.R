#' @title Random Forest + Efficiency Analysis Trees Predictor
#'
#' @description This function predicts the expected value based on a set of inputs.
#'
#' @param forest \code{list} containing the individual Efficiency Analysis Trees.
#' @param xn Row indexes in data.
#'
#' @return Vector of predictions.
RF_predictor <- function(forest, xn){
  m <- length(forest)
  y_result <- rep(list(list()), m)
  
  for(tree in 1:m){
    y_result[[tree]] <- predictor(forest[[tree]], xn)
  }
  
  y_result <- as.data.frame(matrix(unlist(y_result), nrow = length(unlist(y_result[1]))))
  y_result <- apply(y_result, 1, "mean")
  
  return(y_result)
}