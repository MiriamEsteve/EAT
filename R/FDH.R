#' @title FDH: Free Disposal Hull
#'
#' @description This function generates a FDH model
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#'
#' @return List structure with the train and the test set.
FDH <- function(data, x, y) {
  
  data <- preProcess(data, x, y, na.rm = TRUE)
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  data$y_pred <- 0
  
  for (j in 1:nrow(data)){
  yMax <- - Inf
  
    for (n in 1:nrow(data)) {
      newMax <- TRUE
      for (i in x){
        if (data[n, i] > data[j, x]){
          newMax <- FALSE
          break
        }
      
        if (newMax && yMax < data[n, y]){
            yMax <- data[n, y]
        }
      }
    }
  data[j, "y_pred"] <- yMax
  }
  return(data)
}