#' @title Calculation of alpha for pruning procedure
#'
#' @description This function gets the minimun alpha for each subtree evaluated during the pruning procedure of EAT.
#'
#' @param tree A list containing EAT nodes.
#'
#' @return Numeric value corresponfing to the minimun alpha associated with a suitable node to be pruned.
#' 
#' @references
#' \insertRef{breiman1984}{eat}
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

#' @title Error and leaves of the branch to be pruned
#'
#' @description This function calculates the error of a branch as the sum of the errors of its children and the total number of leaf nodes that come from it.
#'
#' @param t List. A given EAT node.
#' @param tree A list containing EAT nodes.
#'
#' @return List with the sum of the errors of the children of the given node and the total number of leaf nodes that comes from it.
RBranch <- function(t, tree) {
  if (t[["SL"]] == -1) {
    return(list(t[["R"]], 1))
  }

  RLeft_numL <- RBranch(tree[[posIdNode(tree, t[["SL"]])]], tree)
  RRight_numR <- RBranch(tree[[posIdNode(tree, t[["SR"]])]], tree)

  return(list(RLeft_numL[[1]] + RRight_numR[[1]], RLeft_numL[[2]] + RRight_numR[[2]]))
}
