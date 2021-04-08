#' @title Random Forest + Efficiency Analysis Trees Plot
#'
#' @description Plot a graph with the Out-of-Bag error for the training dataset and a forest consisting of k trees.
#'
#' @param object A RFEAT object.
#'
#' @importFrom ggplot2 aes geom_point geom_line xlab ylab
#' 
#' @return Line plot with the OOB error and the number of trees in the forest.
#' 
#' @examples 
#' \donttest{
#' simulated <- Y1.sim(N = 150, nX = 6)
#' RFmodel <- RFEAT(data = simulated, x = 1:6, y = 7, numStop = 10,
#'                   m = 50, s_mtry = "BRM", na.rm = TRUE)
#' plotRFEAT(RFmodel)
#' }
#' 
#' @export
plotRFEAT <- function(object) {
  if (class(object) != "RFEAT"){
    stop(paste(deparse(substitute(object)), "must be a RFEAT object"))
    
  }
  
  m <- object[["control"]][["m"]]
  
  data <- object[["data"]][["df"]]
  forest <- object[["forest"]]
  
  y <- object[["data"]][["y"]]
  nY <- length(y)
  output_names <- object[["data"]][["output_names"]]
  
  x <- object[["data"]][["x"]]
  
  # Actual values
  actual_original <- data.matrix(data[, y])
  colnames(actual_original) <- output_names
  
  N <- nrow(data)
  
  # Errors for each m
  errors <- matrix(nrow = m, ncol = 1)
  
  OOB <- object[["OOB"]]
  
  pred <- vector(mode = "list", length = nY)
  
  for (k in 1:m) {
    for (i in 1:N) {
      if (OOB[[k]][[i]]) {
        pred <- mapply(append, pred, predictor(forest[[k]], data[i, x]), SIMPLIFY = FALSE)
      } else {
        pred <- mapply(append, pred, NA, SIMPLIFY = FALSE)
      }
    }
  }
  
  # Each element of pred to a matrix
  pred <- lapply(pred, matrix, nrow = N, ncol = m)

  for (l in 1:m) {
    # predictions for each l
    predicted_k <- matrix(unlist(lapply(pred, function(x) rowMeans(x[, 1:l, drop = FALSE], 
                                                                   na.rm = TRUE))), 
                          ncol = nY)
    
    # add obs index
    predicted_k <- cbind(predicted_k, 1:N)

    # filter: !is.na for first row since NaN rows coincide
    predicted_k <- predicted_k[!is.na(predicted_k[, 1]), ]
    
    # actual
    actual <- matrix(actual_original[predicted_k[, ncol(predicted_k)], ], ncol = nY)
    
    # drop obs column and keep same actual columns
    predicted_k <- matrix(predicted_k[, - ncol(predicted_k)], ncol = ncol(actual))
    
    # MSE between actual y predicted
    MSE <- sum(sapply((actual - predicted_k) ^ 2, sum)) / nrow(actual)
    
    # RMSE
    errors[l] <- sqrt(MSE)
  }

  errors <- as.data.frame(errors)
  names(errors) <- "RMSE"
  
  ggplot(errors, aes(x = 1:m, y = RMSE)) +
    geom_point() +
    geom_line() +
    xlab("k") + 
    ylab("Out-of-Bag error")
  
}


