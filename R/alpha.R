#' @title Alpha Calculation for Pruning Procedure
#'
#' @description This function gets the minimum alpha for each subtree evaluated during the pruning procedure of Efficiency Analysis Trees.
#'
#' @param tree A list containing EAT nodes.
#'
#' @return Numeric value corresponding to the minimum alpha associated with a suitable node to be pruned.
alpha <- function(tree) {
  alpha_min <- Inf

  for (i in 1:length(tree)) {
    if (tree[[i]][["SL"]] == -1) next

    errBranch_numleaves <- RBranch(tree[[i]], tree)
    errBranch <- errBranch_numleaves[[1]]
    numleaves <- errBranch_numleaves[[2]]

    alpha <- (tree[[i]][["R"]] - errBranch) / (numleaves - 1)
    
    if(alpha < 0) alpha <- 0

    if (alpha < alpha_min) {
      alpha_min <- alpha
    }

    if (alpha_min == 0) break
  }

  return(alpha_min)
}

#' @title Branch Pruning
#'
#' @description This function computes the error of a branch as the sum of the errors of its child nodes.
#'
#' @param t List. A given EAT node.
#' @param tree A list containing EAT nodes.
#'
#' @return List containing the sum of the errors of the child nodes of the pruned node and the total number of leaf nodes that come from it.
RBranch <- function(t, tree) {
  if (t[["SL"]] == -1) {
    return(list(t[["R"]], 1))
  }

  RLeft_numL <- RBranch(tree[[posIdNode(tree, t[["SL"]])]], tree)
  RRight_numR <- RBranch(tree[[posIdNode(tree, t[["SR"]])]], tree)

  return(list(RLeft_numL[[1]] + RRight_numR[[1]], RLeft_numL[[2]] + RRight_numR[[2]]))
}
