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
#' @description This function finds the node where a register is located.
#'
#' @param tree A list containing EAT nodes.
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

#' @title Model prediction for EAT
#'
#' @description This function predicts the expected output by an EAT object.
#'
#' @param object An EAT object.
#' @param newdata Dataframe. Set of input variables to predict on.
#'
#' @importFrom dplyr %>%
#'
#' @return Data frame with predicted values with as many columns as outputs.
#' 
#' @export
predict_EAT <- function(object, newdata) {
  
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)
  
  if (!is.data.frame(newdata)){
    stop("newdata must be a data.frame")
  } else if (length(train_names) != length(test_names)){
    stop("Training and prediction data must have the same number of variables")
  } else if (!all(train_names == test_names)){
    stop(cat("Variable name: ", test_names[1], "not found in taining data"))
  }
  
  y <- object[["data"]][["y"]] 
  
  tree <- object[["tree"]]
  
  predictions <- c()

  for(register in 1:nrow(newdata)){
    ti <- 1

    while (tree[[ti]][["SL"]] != -1) {
      if (newdata[register, ][tree[[ti]][["xi"]]] < as.data.frame(tree[[ti]][["s"]])) {
        ti <- posIdNode(tree, tree[[ti]][["SL"]])
      } else {
        ti <- posIdNode(tree, tree[[ti]][["SR"]])
      }
    }
    predictions <- append(predictions, unlist(tree[[ti]][["y"]]))
  }

  predictions <- matrix(predictions, ncol = length(y), byrow = T) %>%
    as.data.frame()

  names(predictions) <-  paste(object[["data"]][["output_names"]],"_pred", sep = "")
  
  predictions <-cbind(newdata, predictions)

  return(predictions)
}
