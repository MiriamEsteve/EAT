#' @title Efficiency Analysis Trees
#'
#' @description This function generates a production frontier similar to FDH through an adaptation of regression trees based on CART methodology.
#'
#' @param data Data to be used. Dataframe or matrix objects are acceptable.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param fold Number of folds in which is divided the original dataset to apply Cross-Validation.
#' @param numStop Minimun number of observations in a node for a split to be attempted.
#' @param na.rm If True, NA is omitted.
#'
#' @details The EAT function generates a regression tree model based on CART \insertCite{breiman1984}{eat} under a new approach that guarantees the obtaining of a stepped production frontier that fulfills the property of free disposability. This frontier shares the aforementioned aspects with the FDH frontier \insertCite{deprins1984}{eat} but enhances some of its disadvantages such as the overfitting problem or the underestimation of technical inefficiency. In a first stage, a deep tree is generated. Subsequently, its pruning is carried out using the weakest-ling pruning method \insertCite{breiman1984}{eat}.
#'
#' @references
#' \insertRef{breiman1984}{eat} \cr
#' \insertRef{deprins1984}{eat}
#'
#' @importFrom dplyr %>%
#' @importFrom conflicted conflict_prefer
#' @importFrom Rdpack reprompt
#'
#' @examples
#'
#' # Single output scenario
#'
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#'
#' matrix <- data.frame(X1 = X1, X2 = X2, Y1 = Y1)
#'
#' EAT(data = matrix, x = c(1,2), y = 3, fold = 3, numStop = 4)
#'
#' # Multi-output scenario
#'
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
#' @return List object with the best pruned tree by weakest-link pruning.
EAT <- function(data, x, y, fold = 5, numStop = 5, na.rm = T) {
  conflict_prefer("filter", "dplyr")
  conflict_prefer("mutate", "dplyr")

  data <- preProcess(data, x, y, na.rm = na.rm)
  
  # Size data
  N <- nrow(data)

  # Reorder index 'x' and 'y' in data
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)

  # Size 'x' and 'y'
  nX <- length(x)
  nY <- length(y)

  # Deep tree
  data <- append(data, -1, 0)

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

  for (i in 1:length(Tk[["tree"]])) {
    if (Tk[["tree"]][[i]][["SL"]] == -1) {
      Tk[["tree"]][[i]][["xi"]] <- Tk[["tree"]][[i]][["s"]] <- -1
    }
  }

  print_results(data, x, Tk[["tree"]])

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
#' @importFrom dplyr filter mutate %>% row_number
#' @importFrom conflicted conflict_prefer
#'
#' @return List with each possible pruning for the deep tree and its alpha associated.
deepEAT <- function(data, x, y, numStop) {

  # Check if deepEAT is called by EAT
  if (length(data[[1]]) == 1) {
    enter <- 1
    data <- data[-1] %>% as.data.frame()
  } else {
    enter <- 0

    conflict_prefer("filter", "dplyr")
    conflict_prefer("mutate", "dplyr")

    data <- data[, c(x, y)] %>%
      as.data.frame() %>%
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
  t <- list(
    "id" = 1,
    "F" = -1,
    "SL" = -1,
    "SR" = -1,
    "index" = data[["id"]],
    "R" = -1,
    "xi" = -1,
    "s" = -1,
    "y" = -1,
    "a" = apply(data[, x, drop = F], 2, min) %>% unname(),
    "b" = rep(Inf, nX)
  )

  t[["y"]] <- apply(data[, y, drop = F], 2, max) %>%
    unname() %>%
    as.list()

  t[["R"]] <- mse(data, t, y)

  # Tree
  tree <- list(t)

  # List of leaf nodes
  leaves <- list(t)
  N_leaves <- length(leaves)

  # Tree alpha list. Pruning
  tree_alpha_list <- list(list(
    "alpha" = alpha(tree),
    "score" = Inf,
    "tree" = tree
  ))

  # Build tree
  while (N_leaves != 0) {
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL # Drop t selected

    if (isFinalNode(t[["index"]], data[, x], numStop)) break

    tree_leaves <- split(data, tree, leaves, t, x, y, numStop)

    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)

    # Build the ALPHA tree list
    tree_alpha_list <- append(
      tree_alpha_list,
      list(list(
        "alpha" = alpha(tree),
        "score" = Inf,
        "tree" = tree
      )),
      0
    )
  }

  leaves <- NULL

  if (enter == 1) {
    return(tree_alpha_list)
  } else {
    return(tree)
  }
}

#' @title Print results
#'
#' @description This function prints for each terminal node: the number of observations in it, the efficiency level of the outputs and the mean square error.
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param tree Tree structure.
#'
#' @importFrom knitr kable
#'
#' @return Output print
print_results <- function(data, x, tree) {

  output_names <- names(data)[(length(x)+1):(ncol(data)-1)]

  colnames <- c("Group", "N", output_names, "Error")

  SL <- NULL

  tree_df <- data.frame(Reduce(rbind, tree)) %>%
    filter(SL == -1)

  results <- matrix(
    ncol = length(colnames),
    nrow = nrow(tree_df),
    dimnames = list(NULL, colnames)
  ) %>% as.data.frame()

  results[, "Group"] <- 1:nrow(results)
  results[, "Error"] <- round(sqrt(unlist(tree_df[, "R"])), 2)

  for(i in 1:nrow(results)){
    results[i, "N"] <- length(unlist(tree_df[i, "index"]))
    results[i, 3:(ncol(results)-1)] <- unlist(tree_df[i,"y"])
  }

  return(print(kable(results, "pipe")))

}
