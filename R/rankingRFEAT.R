#' @title Ranking of variables by Random Forest + Efficiency Analysis Trees.
#'
#' @description This function calculates variable importance through a Random Forest + Efficiency Analysis Trees model.
#'
#' @param object A RFEAT object.
#' @param r Integer. Decimal units.
#' @param barplot Logical. If \code{TRUE}, a barplot with importance scores is displayed.
#'
#' @return Dataframe with the importance scores. If \code{barplot = TRUE}, it is returned a list containing the scores and the barplot.
#' 
#' @examples 
#' 
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' RFEAT_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' rankingRFEAT(object = RFEAT_model,
#'              r = 2,
#'              barplot = TRUE)
#' 
#' @export   
rankingRFEAT <- function(object, r = 2, barplot = TRUE) {
  
  if (class(object) != "RFEAT"){
    stop(paste(deparse(substitute(object)), "must be an RFEAT object"))
    
  } 
  
  if(length(object[["data"]][["x"]]) < 2){
    stop("More than two predictors are necessary")
  }
  
  scores <- imp_var_RFEAT(object = object, r = r)
  
  if (barplot == T){
    barplot <- barplot_importance(scores, threshold = NULL)
    return(list(scores = scores, barplot = barplot))
    
  } else {
    return(scores)
  }
  
}

#' @title Importance variable through Random Forest + Efficiency Analysis Trees
#'
#' @description Importance variable through Random Forest + Efficiency Analysis Trees.
#'
#' @param object A RFEAT object
#' @param r Integer. Decimal units.
#' 
#' @importFrom dplyr %>% arrange
#' @return Vector of input importance scores
imp_var_RFEAT <- function(object, r = 2){
  
  err <- object[["error"]]
  data <- object[["data"]][["df"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  numStop <- object[["control"]][["numStop"]]
  m <- object[["control"]][["m"]]
  s_mtry <- object[["control"]][["s_mtry"]]
  
  imp <- data.frame()
  
  for(xi in x){
    # Barajar xi
    df_xi <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
    rf_err_xi <- RFEAT(df_xi, x, y, numStop, m, s_mtry)
    err_xi <- rf_err_xi[["error"]]
    imp <- rbind(imp, (100 * ((err_xi - err)/err)))
  }
  
  rownames(imp) <- object[["data"]][["input_names"]]
  colnames(imp) <- "Importance"
  
  imp <- imp %>%
    arrange(desc(Importance))
  
  return(imp)
}