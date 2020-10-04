#' @title Predictor
#'
#' @description This function predicts the expected value based on a set of inputs.
#'
#' @param tree List structure with the tree nodes.
#' @param register Set of independent values.
#'
#' @return The expected value of the dependent variable based on the given register.
predictor <- function(tree, register){

  ti <- 1

  while(tree[[ti]][["SL"]] != -1){

    if(register[tree[[ti]][["xi"]]] < tree[[ti]][["s"]]){

      ti <- posIdNode(tree, tree[[ti]][["SL"]])

    } else {

      ti <- posIdNode(tree, tree[[ti]][["SR"]])

    }

  }

  return(tree[[ti]][["y"]])

}

#' @title Position of the node
#'
#' @description This function looks for the position of a given node in the tree.
#'
#' @param tree List structure with the tree nodes.
#' @param idNode Id of a specific node.
#'
#' @return Position of the node or -1 if it is not found.
posIdNode <- function(tree, idNode){

  for (i in 1:length(tree)){

    if(tree[[i]][["id"]] == idNode) return (i)

  }

  return (-1)

}
