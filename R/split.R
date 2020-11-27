#' @title Estimation of child nodes
#'
#' @description This function gets the estimation of the response variable and updates Pareto-coordinates and the observation index for both new nodes.
#'
#' @param data Data to be use.
#' @param leaves List structure with leaf nodes or pending expansion nodes.
#' @param t Node which is being splitted.
#' @param xi Variable index that produces the split.
#' @param s Value of xi variable that produces the split.
#' @param y Column output indexes in data.
#'
#' @return Left and right children nodes.
estimEAT <- function(data, leaves, t, xi, s, y) {
  nY <- length(y)
  maxL <- rep(list(-1), nY)

  # Divide child's matrix

  index <- data[, "id"] %in% t[["index"]]

  left <- data[index == T & data[, xi] < s, ]
  right <- data[index == T & data[, xi] >= s, ]

  # Build tL and tR
  # Child's supports
  tL <- t
  tR <- t

  if (nrow(left) == 0 || nrow(right) == 0) {
    tL[["y"]] <- rep(list(Inf), nY)
    tR[["y"]] <- rep(list(Inf), nY)
  } else {
    tL[["index"]] <- left[, "id"]
    tR[["index"]] <- right[, "id"]

    tL[["b"]][xi] <- s
    tR[["a"]][xi] <- s

    # Left son estimation

    yInfLeft <- rep(list(0), nY)

    N_leaves <- length(leaves)

    if (N_leaves != 0) {
      for (i in 1:N_leaves) {
        if (comparePareto(tL, leaves[[i]]) == 1) {
          for (j in 1:nY) {
            if (yInfLeft[[j]] < leaves[[i]][["y"]][[j]]) {
              yInfLeft[[j]] <- leaves[[i]][["y"]][[j]]
            }
          }
        }
      }
    }

    for (j in 1:nY) {
      maxL[[j]] <- max(left[, y[[j]]])

      if (maxL[[j]] >= yInfLeft[[j]]) {
        tL[["y"]][[j]] <- maxL[[j]]
      } else {
        tL[["y"]][[j]] <- yInfLeft[[j]]
      }
    }

    # Right son estimation (same estimate as father)

    tR[["y"]] <- t[["y"]]
  }

  # Children MSE

  tL[["R"]] <- mse(data, tL, y)
  tR[["R"]] <- mse(data, tR, y)

  # Remove

  left <- right <- NULL

  return(list(tL, tR))
}

#' @title Pareto-dominance relationships
#'
#' @description This function denotes if a node domines another one or in other case, if it does not exist any Pareto-dominance relationship.
#'
#' @param t1 A first node.
#' @param t2 A second node.
#'
#' @return -1 if t1 domines t2, 1 if t2 domines t1 and 0 if there are not Pareto-dominance relationships.
comparePareto <- function(t1, t2) {
  if (all.equal(t1$a, t2$a) == TRUE && all.equal(t1$b, t2$b) == TRUE) {
    return(0)
  }

  comp1 <- t1$a < t2$b
  contador <- sum(comp1)

  if (contador == length(t1$a)) {
    return(-1)
  } else {
    comp2 <- t2$a < t1$b
    contador <- sum(comp2)

    if (contador == length(t2$a)) {
      return(1)
    } else {
      return(0)
    }
  }
}

#' @title Split node
#'
#' @description This function gets the variable and split value to be used in estimEAT, selects the best split and updates VarInfo, node indexes and leaves list.
#'
#' @param data Data to be used.
#' @param tree List structure with the tree nodes.
#' @param leaves List with leaf nodes or pending expansion nodes.
#' @param t Node which is being splitted.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#'
#' @importFrom dplyr %>%
#'
#' @return Leaves and tree lists updated with the new children nodes.
split <- function(data, tree, leaves, t, x, y, numStop) {
  N <- nrow(data)
  nX <- length(x)
  N_tree <- length(tree)

  err_min <- Inf

  for (xi in 1:nX) {
    index <- data[, "id"] %in% t[["index"]]
    S <- data[index, xi] %>%
      unique() %>%
      sort()

    if (length(S) == 1) next

    for (i in 2:length(S)) {
      tL_tR_ <- estimEAT(data, leaves, t, xi, S[i], y)
      tL_ <- tL_tR_[[1]]
      tR_ <- tL_tR_[[2]]

      err <- tL_[["R"]] + tR_[["R"]]

      if (err < err_min) {
        t[["xi"]] <- xi
        t[["s"]] <- S[i]
        err_min <- err
        tL <- tL_
        tR <- tR_
      }
    }
  }

  S <- NULL

  t[["SL"]] <- tL[["id"]] <- N_tree + 1
  t[["SR"]] <- tR[["id"]] <- N_tree + 2

  # Establish tree branches (father <--> sons)

  tL[["F"]] <- tR[["F"]] <- t[["id"]]

  tree[[which(t[["id"]] == lapply(tree, function(x) {
    x$id
  }))]] <- t

  if (isFinalNode(tR[["index"]], data[, x], numStop)) {
    tR[["xi"]] <- tR[["s"]] <- -1
    leaves <- append(leaves, list(tR), 0)
  } else {
    leaves <- append(leaves, list(tR))
  }

  if (isFinalNode(tL[["index"]], data[, x], numStop)) {
    tL[["xi"]] <- tL[["s"]] <- -1
    leaves <- append(leaves, list(tL), 0)
  } else {
    leaves <- append(leaves, list(tL))
  }

  tree <- append(tree, list(tL))
  tree <- append(tree, list(tR))

  return(list(tree, leaves))
}

#' @title Mean Square Error
#'
#' @description This function calculates the Mean Square Error between the predicted value and the observations in a given node.
#'
#' @param data Data to be used.
#' @param t A given node.
#' @param y Column output indexes in data.
#'
#' @return Mean Square Error on a node.
mse <- function(data, t, y) {
  if (length(y) == 1) t[["y"]] <- unlist(t[["y"]])

  error <- sum((data[t[["index"]], y] - t[["y"]])^2) / (nrow(data) * length(y))

  return(round(error, 4))
}

#' @title Is Final Node
#'
#' @description This function evaluates a node and checks if it fulfills the conditions to be a final node.
#'
#' @param obs Observation in the evaluated node.
#' @param data Data with predictive variable.
#' @param numStop Minimun number of observations on a node to be splitted.
#'
#' @return True if the node is a final node and false in other case.
isFinalNode <- function(obs, data, numStop) {
  data <- as.data.frame(data)

  if (length(obs) <= numStop || sum(duplicated(data[obs, ])) == length(obs) - 1) {
    return(TRUE)
  }

  return(FALSE)
}
