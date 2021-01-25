#' @title Pruning Scores
#'
#' @description This function calculates the score for each pruning of tree_alpha_list.
#'
#' @param N Number of rows in data.
#' @param Lv_notLv List with train and test sets.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param fold Parts in which is divided the original data set to do Cross-Validation.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param Tk Best pruned tree.
#' @param tree_alpha_list List with all the possible pruning and its alpha associated.
#'
#' @return List with the best pruning for each fold, the pruning with a lower score and tree_alpha_list with scores updated.
scores <- function(N, Lv_notLv, x, y, fold, numStop, Tk, tree_alpha_list) {
  Lv <- Lv_notLv[[1]] # Test
  notLv <- Lv_notLv[[2]] # Training

  # Best sub-trees of each v. This list will have the fold size

  BestTivs <- vector("list", fold)

  # Trees for RCV

  TAiv <- treesForRCV(notLv, x, y, fold, numStop)

  # tree_alpha_list until -1
  for (t in 1:(length(tree_alpha_list) - 1)) {
    alphaIprim <- (tree_alpha_list[[t]][["alpha"]] * tree_alpha_list[[t + 1]][["alpha"]]) ^ (1 / 2)

    score_BestTivsAux <- RCV(N, Lv, y, alphaIprim, fold, TAiv)
    tree_alpha_list[[t]][["score"]] <- score_BestTivsAux[[1]]
    BestTivsAux <- score_BestTivsAux[[2]]

    if (Tk[["score"]] > tree_alpha_list[[t]][["score"]] || is.null(BestTivs[[1]])) {
      Tk <- tree_alpha_list[[t]]

      for (v in 1:fold) {
        BestTivs[[v]] <- BestTivsAux[[v]]
      }
    }
  }
  return(list(BestTivs, Tk, tree_alpha_list))
}

#' @title RCV
#'
#' @param N Number of rows in data.
#' @param Lv Test set.
#' @param y Column output indexes in data.
#' @param alphaIprim Alpha obtained as the square root of the product of two consecutives alpha values in tree_alpha list. It is used to find the best pruing tree.
#' @param fold Parts in which is divided the original data set to do Cross-Validation.
#' @param TAiv List with each possible pruning for the deep tree generated with train set and its alpha values associated.
#'
#' @return Set of best prunings and the associated error calculated with test sets.
RCV <- function(N, Lv, y, alphaIprim, fold, TAiv) {
  BestTivs <- vector("list", fold)
  Rcv <- 0.0
  nY <- length(y)
  
  for (v in 1:fold) {
    Tiv <- list()
    TivAux <- TAiv[[v]][[1]]

    for (i in 1:(length(TAiv[[v]]) - 1)) {
      if (TAiv[[v]][[i]][["alpha"]] <= alphaIprim) {
        Tiv <- TAiv[[v]][[i]]
        TivAux <- Tiv
      }
    }

    if (length(Tiv) == 0) {
      Tiv <- TivAux
    }

    BestTivs[[v]] <- Tiv

    # TEST
    for (reg in 1:nrow(Lv[[v]])) {
      pred <- predictor(Tiv[["tree"]], Lv[[v]][reg, ])

      for (j in 1:nY) {
        Rcv <- Rcv + (Lv[[v]][reg, y[[j]]] - pred[[j]])^2
      }
    }
  }

  Rcv <- Rcv / (N * nY)

  return(list(Rcv, BestTivs))
}
