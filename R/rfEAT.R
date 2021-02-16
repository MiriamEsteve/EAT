#' @title Individual EAT for Random Forest
#'
#' @description This function builds a individual tree for Random Forest
#'
#' @param data Dataframe containing the training set.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param s_mtry Number of variables randomly sampled as candidates at each split. The available options are: \code{"BRM"}, \code{"DEA1"}, \code{"DEA2"}, \code{"DEA3"}, \code{"DEA4"} or any integer.
#' 
#' @return List of m trees in forest and the error that will be used in the ranking of the importance of the variables.
RandomEAT <- function(data, x, y, numStop, s_mtry){
  
  # Size data
  N <- nrow(data)
  
  # Size 'x' and 'y'
  nX <- length(x)
  nY <- length(y)
  
  #t node
  t <- list("id" = 1,
            "F" = -1,
            "SL" = -1,
            "SR" = -1,
            "index" = data[["id"]],
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
  
  # Build tree
  while(N_leaves != 0){
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL # Drop t selected
    if(isFinalNode(t[["index"]], data[, x], numStop)) break
    
    mtry <- select_mtry(s_mtry, t, nX, nY)
    # Randomly select k (<P) of the original predictors
    # Select random columns by index
    arrayK <- sort(sample(x, mtry, replace = FALSE))
    
    tree_leaves <- split_forest(data, tree, leaves, t, x, y, numStop, arrayK)
    
    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)
  }
  
  leaves <- NULL
  
  return(tree)
}

#' @title Random Forest EAT
#'
#' @description This function build \code{m} individual Efficiency Analysis Trees in a forest structure.
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param m Integer. Number of trees to be build.
#' @param s_mtry Number of variables randomly sampled as candidates at each split. The available options are:
#' \itemize{
#' \item{\code{"BRM"}}: \code{in / 3}
#' \item{\code{"DEA1"}}: \code{(t.obs / 2) - out}  
#' \item{\code{"DEA2"}}: \code{(t.obs / 3) - out}
#' \item{\code{"DEA3"}}: \code{t.obs - 2 * out}
#' \item{\code{"DEA4"}}: \code{min(t.obs / out, (t.obs / 3) - out)}
#' \item{integer}
#' }
#' @param na.rm Logical. If \code{TRUE}, NA rows are omitted.
#'
#' @importFrom dplyr %>% row_number
#' 
#' @examples 
#' 
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.1)
#'
#' RFmodel <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 5,
#'                   m = 50, s_mtry = "BRM", na.rm = TRUE)
#' 
#' @return A RFEAT object containing:
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
#'                         \item{\code{numStop}}: numStop hyperparameter value.
#'                         \item{\code{m}}: m hyperparameter value.
#'                         \item{\code{s_mtry}}: s_mtry hyperparameter value.
#'                         \item{\code{na.rm}}: na.rm hyperparameter value.}
#'        }
#'   \item{\code{forest}: list structure containing the individual EAT.}
#'   \item{\code{error}: Out-of-Bag error at the forest.}   
#'   \item{\code{OOB}: list containing Out-of-Bag set for each tree.}
#' }
#' 
#' @export
RFEAT <- function(data, x, y, numStop = 5, m = 50, 
                  s_mtry = "BRM", na.rm = TRUE){
  conflict_prefer("filter", "dplyr")
  
  if (!s_mtry %in% c("BRM", "DEA1", "DEA2", "DEA3", "DEA4")) {
    stop(paste(s_mtry, "is not available. Plase, cheack help(\"RFEAT\")"))
  }
  
  data <- preProcess(data, x, y, na.rm = na.rm)
  
  rwn <- data[[1]]
  
  data <- data[[2]] %>%
    mutate(id = row_number())
  
  # Reorder index 'x' and 'y' in data
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)
  
  # DMUs
  N <- nrow(data)
  nY <- length(y)
  
  # Forest error
  err <- 0
  
  # Forest list
  forest <- rep(list(list()), m)
  forestArray <- rep(list(list()), m)
  
  for(i in 1:m){
    df_train_arr_test <- bagging(data, x, y)
    df_train <- df_train_arr_test[[1]]
    arr_test <- df_train_arr_test[[2]]
    
    forestArray[[i]] <- arr_test
    
    # Train a tree model on this sample -> EAT
    tree <- RandomEAT(df_train, x, y, numStop, s_mtry)
    forest[[i]] <- tree
  }
  
  # arr_test is a list with m elements.
  # each element of arr_test is a list with N elements.
  # each element of the sublist is a binary value that indicates if the DMU is in the training sample.

  # TEST
  for(i in 1:N){
    reg_i <- data[i, ]
    
    y_EstimArr <- rep(list(0), nY)
    
    # Cardinal Ki
    Ki <- 0

    for(k in 1:m){ #k in Ki
      if(forestArray[[k]][[i]]){
        Ki <- Ki + 1
        y_EstimArr <- mapply("+", y_EstimArr,  predictor(forest[[k]], reg_i[x]))
      }
    }
    
    # y_EstimArr is the mean prediction for each output. If all y_EstimArr = 0
    # this observations is not used for prediction
    if(all(sapply(y_EstimArr, identical, 0)))
      next
    err <- err + sum((reg_i[y] - (y_EstimArr / Ki)) ^ 2)
  }
  err <- err/N
  
  RFEAT <- RFEAT_object(data, x, y, rwn, numStop, m, s_mtry, na.rm, forest, err,
                        forestArray)
  
  return(RFEAT)
}

#' @export
print.RFEAT <- function(x, ...) {
  
  input_names <- x[["data"]][["input_names"]]
  output_names <- x[["data"]][["output_names"]]
  
  cat("\n",
      paste(" Formula: "),
      do.call(paste, c(as.list(output_names), sep = " + ")), 
      "~",
      do.call(paste, c(as.list(input_names), sep = " + ")),
      "\n"
  )
  
  cat(
    rep("\n", 1),
    "# ========================== #", "\n",
    "#           Forest           #", "\n",
    "# ========================== #", 
    rep("\n", 2) 
  )
  
  cat(" Error: ", round(x[["error"]], 2), "\n",
      " numStop: ", x[["control"]][["numStop"]],  "\n",
      " No. of trees (m): ", x[["control"]][["m"]], "\n",
      " No. of inputs tried (s_mtry): ", x[["control"]][["s_mtry"]],
      sep = "")
  
}
