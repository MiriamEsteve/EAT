#' @title Model prediction for Free Disposal Hull
#'
#' @description This function predicts the expected output by a Free Disposal Hull model.
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' 
#' @export
#' 
#' @examples
#'  
#' simulated <- eat:::Y1.sim(N = 50, nX = 1)
#' predictFDH(simulated, x = 1, y = 2)
#' 
#' @return Data frame with the original data and the predicted values through a Free Disposal Hull model.
predictFDH <- function(data, x, y) {
  
  if(class(data) == "list") {
    data <- as.data.frame(data[-1]) 
    
    print_results <- FALSE
    
  } else {
    print_results <- TRUE
  }
  
  data <- preProcess(data, x, y, na.rm = TRUE)[[2]]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  predictions <- matrix(ncol = length(y), byrow = T) %>%
    as.data.frame()
  
  names(predictions) <- names(data)[y]
  
  for (j in 1:nrow(data)){
  yMax <- - Inf
  
    for (n in 1:nrow(data)) {
      newMax <- TRUE
      for (i in x){
        if (data[n, i] > data[j, i]){
          newMax <- FALSE
          break
        }
        if (newMax && yMax < data[n, y]){
            yMax <- data[n, y]
        }
      }
    }
  predictions <- rbind(predictions, yMax)
  }
  
  input_names <- names(data)[x]
  output_names <- paste(names(predictions), "_pred", sep = "")
  
  data <- data.frame(cbind(data[, x], predictions[- 1, ]))
  names(data) <- c(input_names, output_names)
  
  if (print_results) {
    print(data[, (length(x) + 1):length(data)])
  }
  
  return(data)
}