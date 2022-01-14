#' @title Efficiency Analysis Trees
#'
#' @description This function estimates a stepped production frontier through regression trees. 
#' 
#' @name EAT
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimum number of observations in a node for a split to be attempted.
#' @param fold Set of number of folds in which the dataset to apply cross-validation during the pruning is divided.
#' @param max.depth Depth of the tree.
#' @param max.leaves Maximum number of leaf nodes.
#' @param na.rm \code{logical}. If \code{TRUE}, \code{NA} rows are omitted.
#'
#' @details The EAT function generates a regression tree model based on CART \insertCite{breiman1984}{eat} under a new approach that guarantees obtaining a stepped production frontier that fulfills the property of free disposability. This frontier shares the aforementioned aspects with the FDH frontier \insertCite{deprins1984}{eat} but enhances some of its disadvantages such as the overfitting problem or the underestimation of technical inefficiency. More details in \insertCite{esteve2020;textual}{eat}.
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
#' @return An \code{EAT} object containing:
#' \itemize{
#'   \item{\code{data} \itemize{
#'                       \item{\code{df}}: data frame containing the variables in the model.
#'                       \item{\code{x}}: input indexes in data.
#'                       \item{\code{y}}: output indexes in data.
#'                       \item{\code{input_names}}: input variable names.
#'                       \item{\code{output_names}}: output variable names.
#'                       \item{\code{row_names}}: rownames in data.}
#'        }
#'   \item{\code{control} \itemize{
#'                         \item{\code{fold}}: fold hyperparameter value.
#'                         \item{\code{numStop}}: numStop hyperparameter value.
#'                         \item{\code{max.leaves}}: max.leaves hyperparameter value.
#'                         \item{\code{max.depth}}: max.depth hyperparameter value.
#'                         \item{\code{na.rm}}: na.rm hyperparameter value.}
#'        }
#'   \item{\code{tree}: list structure containing the EAT nodes.}
#'   \item{\code{nodes_df}: data frame containing the following information for each node. \itemize{
#'        \item{\code{id}}: node index.  
#'        \item{\code{SL}}: left child node index.
#'        \item{\code{N}}: number of observations at the node. 
#'        \item{\code{Proportion}}: proportion of observations at the node.
#'        \item{the output predictions}.
#'        \item{\code{R}}: the error at the node.  
#'        \item{\code{index}}: observation indexes at the node.}
#'        }   
#'   \item{\code{model} \itemize{
#'        \item{\code{nodes}}: total number of nodes at the tree.  
#'        \item{\code{leaf_nodes}}: number of leaf nodes at the tree.
#'        \item{\code{a}}: lower bound of the nodes. 
#'        \item{\code{y}}: output predictions.}
#'        }
#' }
#'
#' @examples
#' # ====================== #
#' # Single output scenario #
#' # ====================== #
#'
#' simulated <- Y1.sim(N = 50, nX = 3)
#' EAT(data = simulated, x = c(1, 2, 3), y = 4, numStop = 10, fold = 5, max.leaves = 6)
#' 
#' # ====================== #
#' #  Multi output scenario #
#' # ====================== #
#'
#' simulated <- X2Y2.sim(N = 50, border = 0.1)
#' EAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 10, fold = 7, max.depth = 7)
#' 
#' @export
EAT <- function(data, x, y, numStop = 5, fold = 5, 
                max.depth = NULL, max.leaves = NULL, 
                na.rm = TRUE) {
  conflict_prefer("filter", "dplyr")
  
  # max.depth and max.leaves included at the same time
  if(!is.null(max.depth) && !is.null(max.leaves)) {
    warning(paste("If max.depth and max.leaves arguments are included, only max.depth is used."))
  }
  
  # Rownames
  rwn <- row.names(data)
  
  # Data in data[x, y] format and rownames
  data <- preProcess(data = data, x = x, y = y, numStop = numStop,
                     fold = fold, max.depth = max.depth, max.leaves = max.leaves, 
                     na.rm = na.rm)
  
  data <- data %>%
    mutate(id = row_number())
  
  # Size data
  N <- nrow(data)

  # Reorder index 'x' and 'y' in data
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)

  # Size 'x' and 'y'
  nX <- length(x)
  nY <- length(y)
  
  # Insert row to know deepEAT is called by this one
  data <- append(data, -1, 0)
  
  # Deep tree and pruning or tree with the size indicated in max.depth or max.leaves
  tree_alpha_list <- deepEAT(data, x, y, numStop, max.depth, max.leaves)

  data <- data[-1] %>% 
    as.data.frame()
  
  # Best Tk for now
  Tk <- tree_alpha_list[[1]]
  
  if (!is.null(max.depth) || !is.null(max.leaves)) {
    EAT <- EAT_object(data, x, y, rwn, numStop, fold, max.depth, max.leaves, na.rm, Tk[["tree"]])
    return(EAT)
  }

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
  
  # set s and xi to -1 for leaf nodes
  Tk[["tree"]] <- lapply(Tk[["tree"]] , function(x) if(x["SL"] == -1) {x["s"] <- x["xi"] <- - 1; x} else {x})
  
  EAT <- EAT_object(data, x, y, rwn, numStop, fold, max.depth, max.leaves, na.rm, Tk[["tree"]])

  return(EAT)
}

#' @title Deep Efficiency Analysis Trees
#'
#' @description This function creates a deep Efficiency Analysis Tree and a set of possible prunings by the weakest-link pruning procedure.
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param numStop Minimum number of observations in a node for a split to be attempted.
#' @param max.depth Maximum depth of the tree.
#' @param max.leaves Maximum number of leaf nodes.
#'
#' @importFrom dplyr filter mutate %>% row_number
#' @importFrom conflicted conflict_prefer
#'
#' @return A \code{list} containing each possible pruning for the deep tree and its associated alpha value. 
deepEAT <- function(data, x, y, numStop = 5, max.depth = NULL, max.leaves = NULL) {
  
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
    "varInfo" = rep(list(c(Inf, Inf, Inf)), nX),
    "R" = -1,
    "xi" = -1,
    "s" = -1,
    "y" = apply(data[, y, drop = F], 2, max) %>%
            unname() %>%
            as.list(),
    "a" = apply(data[, x, drop = F], 2, min) %>% 
            unname(),
    "b" = rep(Inf, nX)
  )

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

  numFinalLeaves <- 1
  N_depth <- 0
  
  # Build tree
  while ( ((is.null(max.leaves)) && (is.null(max.depth)) && N_leaves != 0) 
          || (!(is.null(max.leaves)) && numFinalLeaves < max.leaves & N_leaves != 0)
          || (!(is.null(max.depth)) && N_depth < max.depth & N_leaves != 0)) {
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL # Drop t selected

    # If is final Node --> do not divide
    if (isFinalNode(t[["index"]], data[, x], numStop)) break

    # Divide the node
    tree_leaves <- split(data, tree, leaves, t, x, y, numStop)

    # Num. final leaves (all)
    numFinalLeaves <- numFinalLeaves + 1
    #Num. depth of the tree
    N_depth <- N_depth + 1
    
    # Add the leaf to the node
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
      )),0)
  }

  leaves <- NULL

  if (enter == 1) {
    return(tree_alpha_list)
  } else {
    return(tree)
  }
}

#' @export
print.EAT <- function(x, ...) {
  
  if (!inherits(x, "EAT")){
    stop(paste(deparse(substitute(x)), "must be an EAT object"))
  }
  
  tree <- x[["tree"]]
  input_names <- x[["data"]][["input_names"]]
  
  # Order
  ord <- c(1)
  
  # Reference vector to delete positions
  ref <- c(tree[[1]][["SR"]], tree[[1]][["SL"]])
  
  # Same procedure as in leaves during EAT
  while(length(ref) != 0){
    
    t <- ref[length(ref)]
    
    if (tree[[t]][["SL"]] != -1) {
      ord <- append(ord, t)
      ref <- c(ref[- length(ref)], tree[[t]][["SR"]], tree[[t]][["SL"]])

    } else {
      ord <- append(ord, t)
      ref <- ref[- length(ref)]
      
    }
  }
  
  for (t in ord) {
    cat(
      paste(rep(" | ", floor(t / 2))),
      paste("[", tree[[t]][["id"]], "]", sep = ""),
      
      if (tree[[t]][["id"]] != 1) {
        if(tree[[t]][["id"]] %% 2 == 0) {
          paste(input_names[tree[[tree[[t]][["F"]]]][["xi"]]], "<", 
                round(tree[[tree[[t]][["F"]]]][["s"]], 2)) 
          
        } else {
          paste(input_names[tree[[tree[[t]][["F"]]]][["xi"]]], ">=", 
                round(tree[[tree[[t]][["F"]]]][["s"]], 2)) 
          
        }
      },
      
      if (tree[[t]][["id"]] != 1) paste("-->"),
      
      "y: [", do.call(paste, c(lapply(tree[[t]][["y"]], round, 1),
                                      list(sep = ", "))),"]",
      
      if (tree[[t]][["SL"]] == - 1) paste("<*>"),
      
      "||",
      
      "R:", round(tree[[t]][["R"]], 2),
      
      "n(t):", length(tree[[t]][["index"]]),
      
      rep("\n", 2)
    )
  }
  
  cat(paste('<*> is a leaf node'))
  
}

#' @importFrom dplyr %>% select filter
#' 
#' @export
summary.EAT <- function(object, ...) {
  
  if (!inherits(object, "EAT")){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
  }
  
  results <- object[["nodes_df"]] %>%
    filter(SL == -1) %>%
    select(- index, - SL)
  
  names(results)[c(2:3, 5)] <- c("n(t)", "%", "R(t)")
  
  tree <- object[["tree"]]
  input_names <- object[["data"]][["input_names"]]
  output_names <- object[["data"]][["output_names"]]
  
  cat("\n",
      paste(" Formula: "),
      do.call(paste, c(as.list(output_names), sep = " + ")), 
      "~",
      do.call(paste, c(as.list(input_names), sep = " + ")),
      "\n"
      )
  
  cat(
    "\n",
    "# ========================== #", "\n",
    "#   Summary for leaf nodes   #", "\n",
    "# ========================== #", rep("\n", 2) 
    )
  
  print(results, row.names = FALSE)
  
  cat(
    rep("\n", 1),
    "# ========================== #", "\n",
    "#            Tree            #", "\n",
    "# ========================== #", 
    rep("\n", 2) 
  )
  
  cat(
    " Interior nodes:", paste(object[["model"]][["nodes"]] - object[["model"]][["leaf_nodes"]]), "\n",
    "    Leaf nodes:", paste(object[["model"]][["leaf_nodes"]]), "\n",
    "   Total nodes:", paste(object[["model"]][["nodes"]]),
    rep("\n", 2)
  )
  
  cat("           R(T):", sum(results$R), "\n",
      "       numStop:", object[["control"]][["numStop"]], "\n",
      "          fold:", object[["control"]][["fold"]], "\n",
      "     max.depth:", object[["control"]][["max.depth"]], "\n",
      "    max.leaves:", object[["control"]][["max.leaves"]]
      )
  
  cat(
    rep("\n", 2),
    "# ========================== #", "\n",
    "# Primary & surrogate splits #", "\n",
    "# ========================== #", rep("\n", 2) 
  )
  
  inp <- 1:length(input_names)
  
  for (t in 1:length(tree)) {
    if (tree[[t]][["SL"]] != -1) {
      
      cat(
        paste(" Node ", tree[[t]][["id"]], " --> {",
              tree[[t]][["SL"]], ",", tree[[t]][["SR"]], "}",
              " || ", input_names[tree[[t]][["xi"]]], 
              " --> {R: ", round(unlist(tree[[t]][["varInfo"]][[tree[[t]][["xi"]]]][[1]]) + 
                                 unlist(tree[[t]][["varInfo"]][[tree[[t]][["xi"]]]][[2]]), 
                                 2),
              ", s: ", round(tree[[t]][["s"]], 2), "}", 
              "\n", 
              sep = "")
       )
      
      if (length(inp) > 1) {
      
      cat(paste("   Surrogate splits"), "\n")
      
      # Index of variables except the variable that takes the split    
      vec <- inp[- tree[[t]][["xi"]]]
      
      # Select varInfo
      varInfo <- lapply(tree, "[[", "varInfo")
      
      # Rename varInfo elements with input names
      varInfo <- lapply(varInfo, function(x) {names(x) <- input_names;  return(x)})
      
      # Select only varInfo variables that do not take the split
      varInfo <- varInfo[[t]][vec]
      
      # Sort element by its error (sum of first and second element)
      varInfo <- varInfo[order(sapply(varInfo, function(x) x[[1]] + x[[2]]))]
      
      for (j in 1:length(varInfo)) {
        cat(
          paste("    ", names(varInfo)[j],  " --> ", "{R: ", round(unlist(varInfo[[j]][1]) + 
                                                                     unlist(varInfo[[j]][2]), 
                                                                   2),
                ", s: ", round(unlist(varInfo[[j]][3]), 2), "}", sep = ""),
          "\n"
          )
        }
      }
      cat("\n")
    }
  }
}

#' @title Number of Leaf Nodes in an Efficiency Analysis Trees model
#' 
#' @description This function returns the number of leaf nodes for an Efficiency Analysis Trees model.
#'
#' @param object An \code{EAT} object.
#' 
#' @return Number of leaf nodes of the Efficiency Analysis Trees model introduced.
#' 
#' @examples
#' simulated <- Y1.sim(N = 50, nX = 3)
#' EAT_model <- EAT(data = simulated, x = c(1, 2, 3), y = 4, numStop = 10, fold = 5)
#' EAT_size(EAT_model)
#' 
#' @export
EAT_size <- function(object) {
  
  if (!is(object, "EAT")) {
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
  }
  
  size <- object[["model"]][["leaf_nodes"]]
  
  cat(paste('The number of leaf nodes of the EAT model is:', size))
  
  return(size)
  
}

#' @title Output Levels in an Efficiency Analysis Trees model
#'
#' @description This function returns the frontier output levels for an Efficiency Analysis Trees model.
#'
#' @param object An \code{EAT} object.
#' 
#' @return A \code{data.frame} with the frontier output levels at the leaf nodes of the Efficiency Analysis Trees model introduced.
#' 
#' @examples
#' simulated <- Y1.sim(N = 50, nX = 3)
#' EAT_model <- EAT(data = simulated, x = c(1, 2, 3), y = 4, numStop = 10, fold = 5)
#' EAT_frontier_levels(EAT_model)
#' 
#' @export
EAT_frontier_levels <- function(object) {
  
  if (!is(object, "EAT")) {
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
  }
  
  frontier.levels <- as.data.frame(unique(object[["model"]][["y"]]))
  colnames(frontier.levels) <- object[["data"]][["output_names"]]
  
  cat(paste('The frontier levels of the outputs at the leaf nodes are: ', '\n'))
  
  return(frontier.levels)
  
}

#' @title Descriptive Summary Statistics Table for the Leaf Nodes of an Efficiency Analysis Trees model
#'
#' @description This function returns a descriptive summary statistics table for each output variable calculated from the leaf nodes observations of an Efficiency Analysis Trees model. Specifically, it computes the number of observations, the proportion of observations, the mean, the variance, the standard deviation, the minimum, the first quartile, the median, the third quartile, the maximum and the root mean squared error. 
#' 
#' @param object An \code{EAT} object.
#' 
#' @return A \code{list} or a \code{data.frame} (for 1 output scenario) with the following summary statistics:
#' \itemize{
#'   \item{\code{N}: number of observations.}
#'   \item{\code{Proportion}: proportion of observations.}
#'   \item{\code{mean}: mean.}
#'   \item{\code{var}: variance.}   
#'   \item{\code{sd}: standard deviation.}
#'   \item{\code{min}: minimun.}
#'   \item{\code{Q1}: first quartile.}
#'   \item{\code{median}: median.}
#'   \item{\code{Q3}: third quartile.}
#'   \item{\code{max}: maximum.}
#'   \item{\code{RMSE}: root mean squared error.}
#' } 
#' 
#' @importFrom stats median sd
#' 
#' @examples
#' simulated <- Y1.sim(N = 50, nX = 3)
#' EAT_model <- EAT(data = simulated, x = c(1, 2, 3), y = 4, numStop = 10, fold = 5)
#' EAT_leaf_stats(EAT_model)
#' 
#' @export
EAT_leaf_stats <- function(object) {
  
  if (!is(object, "EAT")) {
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
  }
  
  data <- object[["data"]][["df"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  tree <- object[["tree"]]
  output_names <- object[["data"]][["output_names"]]
  nodes_df <- object[["nodes_df"]]
  nY <- length(y)
  nX <- length(x)
  indx <- nodes_df[, "index"]
  
  descriptive <- vector("list", nrow(nodes_df))
  
  for (i in 1:nrow(nodes_df)){
    
    subset <- as.matrix(data[unlist(indx[[i]]), (nX + 1):ncol(data)])
    
    # Node
    Node <- i
    
    # N
    N <- length(unlist(indx[[i]]))
    
    # Prop
    Proportion <- round(N / nrow(data), 2) * 100
    
    # Means
    mean <- round(apply(subset, 2, mean), 2)
    
    # Var
    var <- round(apply(subset, 2, var), 2)
    
    # Sd
    sd <- round(apply(subset, 2, sd), 2)
    
    # Min
    min <- round(apply(subset, 2, min), 2)
    
    # Q1
    Q1 <- round(apply(subset, 2, quantile)[2, ], 2)
    
    # Median
    median <- round(apply(subset, 2, median), 2)
    
    # Q3
    Q3 <- round(apply(subset, 2, quantile)[4, ], 2)
    
    # Max
    max <- round(apply(subset, 2, max), 2)
    
    # MSE
    MSE <- sum(sapply((subset - nodes_df[i, output_names]) ^ 2, sum)) / N
    MSE <- round(MSE, 2)
    
    # RMSE
    RMSE <- round(sqrt(MSE), 2)
    
    descr <- cbind(Node, N, Proportion, mean, var, sd, min, Q1, median, Q3, max, RMSE)
    colnames(descr)[2:3] <- c("n(t)", "%")
    rownames(descr) <- output_names
    
    descriptive[[i]] <- descr
    
  }
  
  if (length(output_names) == 1) {
    
    descriptive <- data.frame(matrix(unlist(descriptive), ncol = 12, byrow = T))
    colnames(descriptive) <- c('Node', 'n(t)', '%', 'mean', 'var', 'sd', 'min', 'Q1',
                               'median', 'Q3', 'max', 'RMSE')
    
  }
  
  return(descriptive)
}
