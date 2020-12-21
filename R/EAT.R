#' @title Efficiency Analysis Trees
#'
#' @description This function generates a stepped production frontier through regression trees. 
#' 
#' @name EAT
#'
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param fold Integer. Number of folds in which is divided the dataset to apply cross-validation during the pruning.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'
#' @details The EAT function generates a regression tree model based on CART \insertCite{breiman1984}{eat} under a new approach that guarantees the obtaining of a stepped production frontier that fulfills the property of free disposability. This frontier shares the aforementioned aspects with the FDH frontier \insertCite{deprins1984}{eat} but enhances some of its disadvantages such as the overfitting problem or the underestimation of technical inefficiency. More details in \insertCite{esteve2020}{eat}
#'
#' @references
#' \insertRef{breiman1984}{eat} \cr
#' \cr
#' \insertRef{deprins1984}{eat} \cr
#' \cr
#' \insertRef{esteve2020}{eat}
#'
#' @importFrom dplyr %>% mutate row_number select
#' @importFrom conflicted conflict_prefer
#' @importFrom Rdpack reprompt
#' 
#' @return A list of class EAT is returned containing data, control parmeters and the Efficiency Analysis Trees model.
#'
#' @examples
#' # ====================== #
#' # Single output scenario #
#' # ====================== #
#'
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#'
#' simulated <- data.frame(x1 = X1, x2 = X2, y1 = Y1)
#'
#' EAT(data = simulated, x = c(1,2), y = 3, numStop = 5, fold = 5)
#' 
#' # ====================== #
#' #  Multi output scenario #
#' # ====================== #
#'
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#' Y2 <- log(X1) + 2 - abs(rnorm(50, mean = 0, sd = 0.7))
#'
#' simulated <- data.frame(x1 = X1, x2 = X2, y1 = Y1, y2 = Y2)
#'
#' EAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 3, fold = 7)
#'
#' @export
EAT <- function(data, x, y, numStop = 5, fold = 5, na.rm = T) {
  conflict_prefer("filter", "dplyr")
  
  data <- preProcess(data, x, y, na.rm = na.rm)
  
  rwn <- data[[1]]
  
  data <- data[[2]] %>%
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
  data <- append(data, -1, 0)
  
  # Insert row to know deepEAT is called by this one
  tree_alpha_list <- deepEAT(data, x, y, numStop)

  data <- data[-1] %>% 
    as.data.frame()
  
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
  
  EAT <- EAT_object(data, x, y, rwn, fold, numStop, na.rm, Tk[["tree"]])
  
  print_results(EAT)
  
  invisible(EAT)
}

#' @title Deep Efficiency Analysis Trees
#'
#' @description This function creates a deep tree and a set of possible prunings by weakest-link pruning procedure.
#'
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#'
#' @importFrom dplyr filter mutate %>% row_number
#' @importFrom conflicted conflict_prefer
#'
#' @return List containing each possible pruning for the deep tree and its alpha associated.
deepEAT <- function(data, x, y, numStop) {
  
  # Check if deepEAT is called by EAT
  
   if (length(data[[1]]) == 1){
       enter <- 1
       data <- data[-1] %>% as.data.frame()
    } else {
       enter <- 0
       conflict_prefer("filter", "dplyr")
    
       data <- data[, c(x, y)] %>%
         as.data.frame() %>%
         mutate(id = row_number())
    
    # Reorder index 'x' and 'y' in data
      x <- 1:((ncol(data) - 1) - length(y))
      y <- (length(x) + 1):(ncol(data) - 1)
   }
  
  # if (sys.calls()[[sys.nframe()-1]] == "treesForRCV(notLv, x, y, fold, numStop)") {
  
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
    "a" = apply(data[, x, drop = F], 2, min) %>% 
      unname(),
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
  
  # sys.calls()[[sys.nframe()-1]] == "treesForRCV(notLv, x, y, fold, numStop)" / enter == 1

  if (enter == 1) {
    return(tree_alpha_list)
  } else {
    return(tree)
  }
}

#' @title Print leaf nodes results
#'
#' @description This function prints the number of observations and the proportion, the efficiency level of the outputs and the mean square error for each final node.
#'
#' @param EAT A EAT object
#' 
#' @importFrom dplyr %>% select
#' @importFrom knitr kable
#'
#' @return Printing in table format with the data described above.
print_results <- function(EAT) {
  
  results <- EAT[["nodes_df"]][["leafnodes_df"]] %>%
    select(-index)
  
  return(print(kable(results), "pipe"))
  
}


