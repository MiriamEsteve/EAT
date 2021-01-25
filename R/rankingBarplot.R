#' @title Barplot Variable Importance
#'
#' @description This function generates a barplot with the importance of each predictor.
#'
#' @param m Dataframe with the importance of each predictor.
#' @param threshold Importance score value in which a line should be graphed.
#'
#' @importFrom ggplot2 ggplot geom_col xlab aes geom_hline
#' @importFrom stats reorder
#'
#' @return Barplot representing each variable in the x-axis and its importance in the y-axis.
barplot_importance <- function(m, threshold) {
  barplot_importance <- ggplot(m, aes(x = reorder(row.names(m), -Importance), 
                                      y = Importance)) +
    geom_col() +
    xlab("Variable")
  
  if (!is.null(threshold)) {
    barplot_importance <- barplot_importance +
      geom_hline(yintercept = threshold, color = "#F8766D", size = 1.25)
  }
  
  return(barplot_importance)
}