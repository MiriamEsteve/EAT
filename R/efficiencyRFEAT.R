#' @title Efficiency Scores computed through a Random Forest + Efficiency Analysis Trees model.
#'
#' @description This function calculates the efficiency scores for each DMU through a Random Forest + Efficiency Analysis Trees model and the Banker Charnes and Cooper mathematical programming model with output orientation. Efficiency level at 1.
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param object A \code{RFEAT} object.
#' @param digits Decimal units for scores.
#' @param FDH \code{logical}. If \code{TRUE}, FDH scores are computed.
#' @param na.rm \code{logical}. If \code{TRUE}, \code{NA} rows are omitted.
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
#' @return \code{data.frame} introduced as argument with efficiency scores computed through a Random Forest + Efficiency Analysis Trees model.
efficiencyRFEAT <- function(data, x, y, object, digits = 3, FDH = TRUE, na.rm = TRUE){
  
  if (!is(object, "RFEAT")) {
    stop(paste(deparse(substitute(object)), "must be a RFEAT object."))
    
    } else if (digits < 0) {
    stop(paste('digits =', digits, 'must be greater than 0.'))
  }
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  data <- preProcess(data, x, y, na.rm = na.rm)
  
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
  rownames(scoreRF) <- row.names(data)
  
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
    rownames(scores_FDH) <- row.names(data)
    
    descriptive[2, ] <- scores_FDH %>%
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
