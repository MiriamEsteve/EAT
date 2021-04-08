#' @title Random Forest + Efficiency Analysis Trees Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU through a Random Forest + Efficiency Analysis Trees model and the Banker Charnes and Cooper mathematical programming model with output orientation.
#'
#' @param data Dataframe for which the efficiency score is calculated.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param object A RFEAT object.
#' @param digits Integer. Decimal units for scores.
#' @param FDH Logical. If \code{TRUE}, FDH scores are computed.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'
#' @importFrom dplyr %>% mutate summarise
#' @importFrom stats median quantile sd
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' RFEAT_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4))
#'
#' efficiencyRFEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = RFEAT_model, 
#'                 digits = 2, FDH = TRUE, na.rm = TRUE)
#' }
#'
#' @return Dataframe with input variables and efficiency scores through a Random Forest + Efficiency Analysis Trees model.
efficiencyRFEAT <- function(data, x, y, object, digits = 3, FDH = TRUE, na.rm = TRUE){
  
  if (class(object) != "RFEAT"){
    stop(paste(deparse(substitute(object)), "must be a RFEAT object."))
    
  } 
  
  if (digits < 0) {
    stop(paste('digits =', digits, 'must be greater than 0.'))
  }
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  rwn_data <- preProcess(data, x, y, na.rm = na.rm)
  
  rwn <- rwn_data[[1]]
  data <- rwn_data[[2]]
  
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)

  if (!identical(sort(train_names), sort(names(data)))) {
    stop("Different variable names in training and data")
  }
  
  N <- nrow(data)
  nY <- length(y)
  
  # Forest values return by RFEAT()
  forest <- object[["forest"]]
  
  data <- data %>% 
    mutate(scoreRF = rep(0, N))
  
  yRF <- rep(list(list()), nY)
  y_result <- as.data.frame(matrix(ncol = nY, nrow = N))
  
  for(xn in 1:N){
    yRF <- RF_predictor(forest, data[xn, ])
    
    if(typeof(yRF[[1]]) != "double")
      yRF <- RF[[1]]
    
    for(d in 1:nY){
      y_result[xn, d] <- round(yRF[[d]] / data[xn, y[[d]]], 6)
    }
    data$scoreRF[xn] <- min(y_result[xn, ])
  }
  
  scoreRF <- as.data.frame(data$scoreRF)
  names(scoreRF) <- "RFEAT_BCC_OUT"
  rownames(scoreRF) <- rwn
  
  descriptive <- scoreRF %>%
    summarise("Model" = "RFEAT",
              "Mean" = round(mean(scoreRF[, 1]), digits),
              "Std. Dev." = round(sd(scoreRF[, 1]), digits),
              "Min" = round(min(scoreRF[, 1]), digits),
              "Q1" = round(quantile(scoreRF[, 1])[[2]], digits),
              "Median" = round(median(scoreRF[, 1]), digits),
              "Q3" = round(quantile(scoreRF[, 1])[[3]], digits),
              "Max" = round(max(scoreRF[, 1]), digits)
    )
  
  if (FDH == TRUE) {
    
    j <- nrow(data)
    scores <- matrix(nrow = j, ncol = 1)
    x_k <- as.matrix(data[, x])
    y_k <- as.matrix(data[, y])
    nX <- length(x)
    nY <- length(y)
    
    scores_FDH <- EAT_BCC_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
    FDH_model <- "FDH_BCC_OUT"
    
    scores_FDH <- as.data.frame(scores_FDH)
    names(scores_FDH) <- FDH_model
    rownames(scores_FDH) <- rwn
    
    descriptive_FDH <- scores_FDH %>%
      summarise("Model" = "FDH",
                "Mean" = round(mean(scores_FDH[, 1]), digits),
                "Std. Dev." = round(sd(scores_FDH[, 1]), digits),
                "Min" = round(min(scores_FDH[, 1]), digits),
                "Q1" = round(quantile(scores_FDH[, 1])[[2]], digits),
                "Median" = round(median(scores_FDH[, 1]), digits),
                "Q3" = round(quantile(scores_FDH[, 1])[[3]], digits),
                "Max" = round(max(scores_FDH[, 1]), digits)
      )
    
    scores_df <- cbind(data, round(scoreRF, digits), round(scores_FDH, digits))
    print(scores_df[, c(ncol(scores_df) - 1, ncol(scores_df))])
    
    cat("\n")
    print(descriptive, row.names = FALSE)
    cat("\n")
    print(descriptive_FDH, row.names = FALSE)
    
    invisible(scores_df)
    
  } else {
    
    scores_df <- cbind(data, round(scoreRF, digits))
    
    print(paste("BCC output orientation programming model"))
    cat("\n")
    print(round(scores_df[, ncol(scores_df)], digits))
    
    cat("\n") 
    print(descriptive, row.names = FALSE)
    
    
    invisible(scores_df)
    
  }
}
