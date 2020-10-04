#' @title Alpha
#'
#' @description This function gets the minimun alpha for each possible pruning.
#'
#' @param tree List structure with the tree nodes.
#'
#' @return Minimun alpha associated with a suitable node to be pruned.
alpha <- function(tree){

  alpha_min <- Inf

  for(i in 1:length(tree)){

    if(tree[[i]][["SL"]] == -1) next

    errBranch_numleaves <- RBranch(tree[[i]], tree)
    errBranch <- errBranch_numleaves[[1]]
    numleaves <- errBranch_numleaves[[2]]

    alpha <- (tree[[i]][["R"]] - errBranch) / (numleaves - 1)

    if (alpha < alpha_min) {
      alpha_min <- alpha
    }

    if (alpha_min == 0 ) break

  }

  return(alpha_min)
}

#' @title RBranch
#'
#' @description Recursive function that calculates the error of a node as the sum of the errors of its children and the total number of leaf nodes that come from it.
#'
#' @param t A given node.
#' @param tree List structure with the tree nodes.
#'
#' @return List structure with the sum of the errors of the children of the given node and the total number of leaf nodes that comes from it.
RBranch <- function(t, tree){

  if(t[["SL"]] == -1) return (list(t[["R"]], 1))

  RLeft_numL <- RBranch(tree[[posIdNode(tree, t[["SL"]])]], tree)
  RRight_numR <- RBranch(tree[[posIdNode(tree, t[["SR"]])]], tree)

  return (list(RLeft_numL[[1]] + RRight_numR[[1]], RLeft_numL[[2]] + RRight_numR[[2]]))

}

