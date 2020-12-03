#' @title Predictor
#'
#' @description This function predicts the expected value based on a set of inputs.
#'
#' @param tree List structure with the tree nodes.
#' @param register Set of independent values.
#'
#' @return The expected value of the dependent variable based on the given register.
predictor <- function(tree, register) {
  ti <- 1

  while (tree[[ti]][["SL"]] != -1) {
    if (register[tree[[ti]][["xi"]]] < as.data.frame(tree[[ti]][["s"]])) {
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
posIdNode <- function(tree, idNode) {
  for (i in 1:length(tree)) {
    if (tree[[i]][["id"]] == idNode) {
      return(i)
    }
  }

  return(-1)
}

#' @title Predict
#'
#' @description This function predicts the expected value based on a dataset
#'
#' @param tree List structure with the tree nodes.
#' @param data Data to be predicted.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @return A data frame with the expected value in one column for each dependent variable based on the given dataset.
predict <- function(tree, data, x, y) {
  data <- preProcess(data, x, y, na.rm = T)

  predictions <- c()

  for(register in 1:nrow(data)){
    ti <- 1

    while (tree[[ti]][["SL"]] != -1) {
      if (data[register, ][tree[[ti]][["xi"]]] < as.data.frame(tree[[ti]][["s"]])) {
        ti <- posIdNode(tree, tree[[ti]][["SL"]])
      } else {
        ti <- posIdNode(tree, tree[[ti]][["SR"]])
      }
    }
    predictions <- append(predictions, unlist(tree[[ti]][["y"]]))
  }

  predictions <- matrix(predictions, ncol = length(y), byrow = T) %>%
    as.data.frame()

  names(predictions) <- names(data)[(length(x)+1):(ncol(data)-1)]

  return(predictions)
}
