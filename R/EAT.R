#' @title Efficiency Analysis Tree
#'
#' @description This function prunes the tree-object of deepEAT function by weakest-link pruning method.
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param fold Parts in which is divided the original data set to do Cross-Validation.
#' @param numStop Minimun number of observations on a node to be splitted.
#'
#' @import dplyr
#' @import conflicted
#'
#' @example
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#' Y2 <- log(X1) + 2 - abs(rnorm(50, mean = 0, sd = 0.7))
#'
#' matrix <- data.frame(X1 = X1, X2 = X2, Y1 = Y1, Y2 = Y2)
#'
#' EAT(data = matrix, x = c(1,2), y = c(3, 4))
#'
#' @export
#'
#' @return Best pruned tree by weakest-link pruning.
EAT <- function(data, x, y, fold = 5, numStop = 5){

  conflict_prefer("filter", "dplyr")

  data <- data[, c(x, y)] %>% as.data.frame() %>%
    mutate(id = row_number())

  # Size data

  N <- nrow(data)

  # Reorder index 'x' and 'y' in data

  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)

  # Size 'x' and 'y'

  nX <- length(x)
  nY <- length(y)

  # Deep tree

  data <- append(data, - 1, 0)

  # Insert row to know deepEAT is called by this one
  tree_alpha_list <- deepEAT(data, x, y, numStop)

  data <- data[-1] %>% as.data.frame()

  # Best Tk for now

  Tk <- tree_alpha_list[[1]]

  # Generate Lv (test) and notLv (training)

  Lv_notLv <- generateLv(data, fold)
  Lv <- Lv_notLv[[1]]
  notLv <- Lv_notLv[[2]]

  # Scores

  BestTivs_Tk <- scores(N, Lv_notLv, x, y, fold, numStop, Tk, tree_alpha_list)
  BestTivs <- BestTivs_Tk[[1]]
  Tk <- BestTivs_Tk[[2]]
  tree_alpha_list <- BestTivs_Tk[[3]]

  # SERules

  SE <- SERules(N, Lv, y, fold, Tk[["score"]], BestTivs)

  # selectTk

  Tk <- selectTk(Tk, tree_alpha_list, SE)

  for(i in 1:length(Tk[["tree"]])){

    if(Tk[["tree"]][[i]][["SL"]] == -1){

      Tk[["tree"]][[i]][["xi"]] <- Tk[["tree"]][[i]][["s"]] <- - 1

    }
  }

  return(Tk[["tree"]])

}

#' @title Deep Efficiency Analysis Tree
#'
#' @description This function creates a deep tree and a set of possible prunings.
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#'
#' @import dplyr
#' @import conflicted
#'
#' @return List with each possible pruning for the deep tree and its alpha associated.
deepEAT <- function(data, x, y, numStop){

  # Check if deepEAT is called by EAT

  if(length(data[[1]]) == 1){

    enter <- 1
    data <- data[-1] %>% as.data.frame()

  } else {

    enter <- 0

    conflict_prefer("filter", "dplyr")

    data <- data[, c(x, y)] %>% as.data.frame() %>%
      mutate(id = row_number())

    # Reorder index 'x' and 'y' in data

    x <- 1:((ncol(data) - 1) - length(y))
    y <- (length(x) + 1):(ncol(data) - 1)
  }

  # Size data

  N <- nrow(data)

  # Size 'x' and 'y'

  nX <- length(x)
  nY <- length(y)

  # t node

  t <- list("id" = 1,
            "F" = -1,
            "SL" = -1,
            "SR" = -1,
            "index" = data[["id"]],
            "varInfo" = rep(list(list(Inf, Inf, Inf)), nX),
            "R" = -1,
            "xi" = -1,
            "s" = -1,
            "y" = -1,
            "a" = apply(data[, x, drop = F], 2, min) %>% unname(),
            "b" = rep(Inf, nX)
  )

  t[["y"]] <- lapply(data[ , y], function(x) max(x, na.rm = TRUE)) %>% unname()

  t[["R"]] <- mse(data, t, y)

  # Tree

  tree <- list(t)

  # List of leaf nodes

  leaves <- list(t)
  N_leaves <- length(leaves)

  # Tree alpha list. Pruning

  tree_alpha_list <- list(list("alpha" = alpha(tree),
                               "score" = Inf,
                               "tree" = tree))

  # Build tree

  while(N_leaves != 0){

    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL #Drop t selected

    if(isFinalNode(t[["index"]], data[, x], numStop)) break

    tree_leaves <- split(data, tree, leaves, t, x, y, numStop)

    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)

    # Build the ALPHA tree list

    tree_alpha_list <- append(tree_alpha_list,
                              list(list("alpha" = alpha(tree),
                                        "score" = Inf,
                                        "tree" = tree)),
                              0)
  }

  leaves <- NULL

  if(enter == 1){

    return(tree_alpha_list)

  } else {

    return(tree)
  }
}

