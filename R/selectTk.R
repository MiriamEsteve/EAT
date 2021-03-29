#' @title SERules
#'
#' @description Based on Validation tests over BestTivs, a new range of scores is obtained to find new pruned trees.
#'
#' @param N Number of rows in data.
#' @param Lv Test set.
#' @param y Column output indexes in data.
#' @param fold Parts in which the original data set is divided to perform Cross-Validation.
#' @param Tk_score Best pruned tree score.
#' @param BestTivs List of best pruned trees for each training set.
#'
#' @return Value to get a range where new pruning is found.
SERules <- function(N, Lv, y, fold, Tk_score, BestTivs) {
  s2 <- 0.0
  nY <- length(y)

  # SERule (makes again Cross-Validation... only Validation)
  for (v in 1:fold) {
    for (reg in 1:nrow(Lv[[v]])) {
      pred <- predictor(BestTivs[[v]][["tree"]], Lv[[v]][reg, ])

      for (j in 1:nY) {
        dif1 <- (Lv[[v]][reg, y[[j]]] - pred[[j]])^2

        s2 <- s2 + (dif1 - Tk_score)^2
      }
    }
  }

  SE <- (s2 / (N * nY) / (N * nY))^(1 / 2)

  return(SE)
}


#' @title Select Tk
#'
#' @description This function tries to find a new pruned tree with a shorter length and a score in the range generated for SE.
#'
#' @param Tk Best pruned tree score.
#' @param tree_alpha_list List with all the possible pruning and its associated alpha and scores. 
#' @param SE Value to get a range where new prunings is found.
#'
#' @return The same best tree or a new suitable one.
selectTk <- function(Tk, tree_alpha_list, SE) {

  # Select the definitive tree: the one with the smallest size with a SE clearance score

  margin <- Tk[["score"]] + SE

  # Select final tree
  for (lst in 2:length(tree_alpha_list)) {
    if ((tree_alpha_list[[lst]][["score"]] <= margin) & (length(tree_alpha_list[[lst]][["tree"]]) < length(Tk[["tree"]]))) {
      Tk <- tree_alpha_list[[lst]]
    }
  }

  return(Tk)
}
