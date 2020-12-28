#' @title EAT object
#'
#' @description This function saves information about the EAT model 
#'
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param register_names String vector. Data rownames.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param fold Integer. Number of folds in which is divided the dataset to apply cross-validation during the pruning.
#' @param na.rm Logical. If True, NA rows are omitted. If False, an error occurs in case of NA rows.
#' @param tree A list containing the nodes of the EAT pruned tree.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return An EAT object
EAT_object <- function(data, x, y, register_names, numStop, fold, na.rm, tree) {
  
  # Output and input names
  output_names <- names(data)[y]
  input_names <- names(data)[x]
  
  colnames <- c("Node", "N", "Proportion", output_names, "Error")
  
  # Tree as data.frame
  nodes_frame <- as.data.frame(Reduce(rbind, tree))
  
  # a
  atreeTk <- nodes_frame  %>% 
    filter(SL == -1) %>% 
    select(a)
  
  atreeTk <- as.data.frame(do.call(cbind, atreeTk$a)) %>% 
    t()
  
  # y
  ytreeTk <- nodes_frame %>% 
    filter(SL == -1) %>% 
    select(y)
  
  ytreeTk <- as.data.frame(do.call(cbind, ytreeTk$y)) %>%
    unlist() %>%
    matrix(ncol = length(y), byrow = T)
  
  # Nodes frame for results
  nodes_frame <- nodes_frame %>%
    mutate(N = sapply(nodes_frame$index, length),
           MSE = round(sqrt(unlist(R)), 2),
           Prop = round((N / N[1]) * 100), 2)
  
  nodes_frame[, output_names] <- unlist(nodes_frame[ ,"y"])
  
  nodes_frame <- nodes_frame %>%
    select(id, SL, N, Prop, output_names, MSE, index)
  
  # Leaf nodes
  leaf_nodes <- nodes_frame %>%
    filter(SL == -1)
  
  EAT_object <- list("data" = list(data = data %>% select(-id),
                                   x = x,
                                   y = y,
                                   input_names = input_names,
                                   output_names = output_names,
                                   row_names = register_names),
                     "control" = list(fold = fold, 
                                      numStop = numStop, 
                                      na.rm = na.rm),
                     "tree" = tree,
                     "nodes_df" = list(nodes_df = nodes_frame %>% select(-SL),
                                       leafnodes_df = leaf_nodes %>% select(-SL)),
                     "model" = list(nodes = length(tree),
                                    leaf_nodes = nrow(atreeTk),
                                    RMSE = NULL,
                                    a = atreeTk,
                                    y = ytreeTk))
  
  class(EAT_object) <- "EAT"
  
  # RMSE
  data.p <- as.data.frame(data[, x])
  pred <- data.frame()
  for (i in 1:nrow(data.p)){
    pred <- rbind(pred, predictor(tree, data.p[i, ]))
  }
  
  predictions <- cbind(data.p, pred)
  names(predictions) <- c(input_names, output_names)
  
  RMSE <- sqrt(sum((data[, y] - predictions[, y]) ^ 2) / nrow(data))
  
  EAT_object[["model"]][["RMSE"]] <- RMSE
  
  return(EAT_object)
  
}

#' @title RFEAT object
#'
#' @description This function saves information about the RFEAT model 
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param register_names String vector. Data rownames.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param m Integer. Number of trees to be build.
#' @param s_mtry Select number of inputs in each split.
#' \itemize{
#' \item{\code{"Breiman"}}: \code{in / 3}
#' \item{\code{"DEA1"}}: \code{(obs / 2) - out}  
#' \item{\code{"DEA2"}}: \code{(obs / 3) - out}
#' \item{\code{"DEA3"}}: \code{obs - 2 * out}
#' \item{\code{"DEA4"}}: \code{min(obs / out, (obs / 3) - out)}
#' }
#' @param na.rm Logical. If \code{TRUE}, NA rows are omitted.
#' @param forest A list containing the individual EAT trees.
#' @param error Error in forest.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return An RFEAT object
RFEAT_object <- function(data, x, y, register_names, numStop, m, s_mtry, na.rm, forest, error) {
  
  # Output and input names
  output_names <- names(data)[y]
  input_names <- names(data)[x]
  
  RFEAT_object <- list("data" = list(data = data %>% select(-id),
                                     x = x,
                                     y = y,
                                     input_names = input_names,
                                     output_names = output_names,
                                     row_names = register_names),
                       "control" = list(numStop = numStop,
                                        m = m,
                                        s_mtry = s_mtry,
                                        na.rm = na.rm),
                       "forest" = forest,
                       "error" = error)
  
  class(RFEAT_object) <- "RFEAT"
  
  return(RFEAT_object)
  
}

