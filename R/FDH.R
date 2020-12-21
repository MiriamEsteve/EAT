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
#' @return Data frame with the original data and the predicted values by and FDH model.
predict_FDH <- function(data, x, y) {
  
  data <- preProcess(data, x, y, na.rm = TRUE)[[2]]
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

#' @title FDH Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU by an FDH model.
#' 
#' @param data Dataframe for which the efficiency score is calculated.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param scores_model Mathematic programming model to calculate scores. 
#' \itemize{
#' \item{\code{FDH_BCC_out}} BBC model. Output orientation.
#' \item{\code{FDH_BCC_in}}  BBC model. Input orientation.
#' \item{\code{FDH_DDF}}     Directional distance model.
#' \item{\code{FDH_RSL_out}} Rusell model. Output orientation
#' \item{\code{FDH_RSL_in}}  Rusell model. Input orientation.
#' \item{\code{FDH_WAM}}     Weighted Additive model.
#' }
#' @param r Integer. Decimal units for scores.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#' @export
#'
#' @return Dataframe with input variables and efficiency scores by a FDH model.
efficiency_FDH <- function(data, x, y, 
                           scores_model, r = 4, na.rm = TRUE) {
  
  if (!scores_model %in% c("FDH_BCC_out", "FDH_BCC_in", "FDH_DDF", 
                           "FDH_RSL_out", "FDH_RSL_in", "FDH_WAM")){
    stop(paste(scores_model, "is not available. Please, check help(efficiency_EAT)"))
  }
  
  if (!is.data.frame(data)){
    stop("data must be a data.frame")
  } 
  
  data <- preProcess(data, x, y, na.rm = T)[[2]]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  j <- N_leaves <- nrow(data)
  FDH_scores <- matrix(nrow = j, ncol = 1)
  x_k <- atreeTk <- as.matrix(data[, x])
  y_k <- ytreeTk <- as.matrix(data[, y])
  nX <- length(x)
  nY <- length(y)
  
  if (scores_model == "FDH_BCC_out"){
    scores <- EAT_BCC_out(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    
  } else if (scores_model == "FDH_BCC_in"){
    scores <- EAT_BCC_in(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    
  } else if (scores_model == "FDH_DDF"){
    scores <- EAT_DDF(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    
  } else if (scores_model == "FDH_RSL_out"){
    scores <- EAT_RSL_out(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    
  } else if (scores_model == "FDH_RSL_in"){
    scores <- EAT_RSL_in(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    
  } else if (scores_model == "FDH_WAM"){
    scores <- EAT_WAM(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
  }
  
  scores <- as.data.frame(scores)
  names(scores) <- scores_model
  rownames(scores) <- rownames(data)
  
  descriptive <- scores %>%
    summarise("Mean" = round(mean(scores[, 1]), 2),
              "Std. Dev." = round(sd(scores[, 1]), 2),
              "Min" = round(min(scores[, 1]), 2),
              "Q1" = round(quantile(scores[, 1])[[2]], 2),
              "Median" = round(median(scores[, 1]), 2),
              "Q3" = round(quantile(scores[, 1])[[3]], 2),
              "Max" = round(max(scores[, 1]), 2)
    )
  
  print(round(scores, r))
  
  scores_df <- cbind(data, round(scores, r)) 
  
  print(kable(descriptive), "pipe")
  
  invisible(scores_df)
}