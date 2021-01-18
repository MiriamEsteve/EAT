#' @title Model prediction for FDH
#'
#' @description This function predicts the expected output by an FDH model.
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' 
#' @export
#' 
#' @examples
#'  
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' predictFDH(simulated, x = c(1, 2), y = c(3, 4))
#' 
#' @return Data frame with the original data and the predicted values by and FDH model.
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