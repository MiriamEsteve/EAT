#' @title Check Efficiency Analysis Trees.
#'
#' @description This function verifies if a specific tree keeps to Pareto-dominance properties.
#'
#' @param tree A \code{list} containing the EAT nodes.
#'
#' @return Message indicating if the tree is acceptable or warning in case of breaking any Pareto-dominance relationship.
checkEAT <- function(tree) {
  resComp <- NULL
  check <- 1
  nY <- length(tree[[1]][["y"]])

  for (t1 in 1:(length(tree) - 1)) {
    if (tree[[t1]][["SL"]] == -1) {
      for (t2 in (t1 + 1):length(tree)) {
        if (tree[[t2]][["SL"]] == -1) {
          resComp <- comparePareto(tree[[t1]], tree[[t2]])

          for (j in 1:nY) {
            if (resComp == -1 && tree[[t1]][["y"]][[j]] > tree[[t2]][["y"]][[j]]) {
              print(paste("ERR (", resComp, ")"))
              print(tree[[t1]][["id"]])
              print(tree[[t2]][["id"]])
              check <- 0
            } else if (resComp == 1 && tree[[t1]][["y"]][[j]] < tree[[t2]][["y"]][[j]]) {
              print(paste("ERR (", resComp, ")"))
              print(tree[[t1]][["id"]])
              print(tree[[t2]][["id"]])
              check <- 0
            }
          }
        }
      }
    }
  }

  if (check == 0) {
    warning("The tree does not fullfill the Pareto-Dominance conditions")
  } else {
    return(TRUE)
  }
}
