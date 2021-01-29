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
#' @param max.depth Integer. Maximum number of leaf nodes.
#' @param na.rm Logical. If True, NA rows are omitted. If False, an error occurs in case of NA rows.
#' @param tree A list containing the nodes of the EAT pruned tree.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return An EAT object
EAT_object <- function(data, x, y, register_names, numStop, fold, max.depth, na.rm, tree) {
  
  # Output and input names
  output_names <- names(data)[y]
  input_names <- names(data)[x]
  
  # Tree as data.frame
  nodes_frame <- as.data.frame(Reduce(rbind, tree))
  
  # a (leaf nodes)
  atreeTk <- nodes_frame  %>% 
    filter(SL == -1) %>% 
    select(a)
  
  atreeTk <- as.data.frame(do.call(cbind, atreeTk$a)) %>% 
    t()
  
  # y
  
  # all nodes
  
  effcy_levels <- nodes_frame %>%
    select(y)
  
  effcy_levels <- as.data.frame(do.call(cbind, effcy_levels$y)) %>%
    unlist() %>%
    matrix(ncol = length(y), byrow = T) %>%
    as.data.frame()
  
  # leaf nodes
  
  ytreeTk <- nodes_frame %>% 
    filter(SL == -1) %>% 
    select(y)
  
  ytreeTk <- as.data.frame(do.call(cbind, ytreeTk$y)) %>%
    unlist() %>%
    matrix(ncol = length(y), byrow = T)
  
  # Nodes frame for results
  nodes_frame <- nodes_frame %>%
    mutate(N = sapply(nodes_frame$index, length),
           R = round(unlist(R), 2),
           Proportion = round((N / N[1]) * 100), 2)
  
  nodes_frame[, output_names] <- effcy_levels
  
  nodes_frame <- nodes_frame %>%
    select(id, SL, N, Proportion, output_names, R, index)
  
  EAT_object <- list("data" = list(df = data %>% 
                                     select(-id),
                                   x = x,
                                   y = y,
                                   input_names = input_names,
                                   output_names = output_names,
                                   row_names = register_names),
                     "control" = list(fold = fold, 
                                      numStop = numStop,
                                      max.depth = max.depth,
                                      na.rm = na.rm),
                     "tree" = tree,
                     "nodes_df" = nodes_frame,
                     "model" = list(nodes = length(tree),
                                    leaf_nodes = nrow(atreeTk),
                                    a = atreeTk,
                                    y = ytreeTk))
  
  class(EAT_object) <- "EAT"
  
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
#' \item{\code{"DEA1"}}: \code{(t.obs / 2) - out}  
#' \item{\code{"DEA2"}}: \code{(t.obs / 3) - out}
#' \item{\code{"DEA3"}}: \code{t.obs - 2 * out}
#' \item{\code{"DEA4"}}: \code{min(t.obs / out, (t.obs / 3) - out)}
#' }
#' @param na.rm Logical. If \code{TRUE}, NA rows are omitted.
#' @param forest A list containing the individual EAT trees.
#' @param error Error in forest.
#' @param OOB List containing the observations with which each tree has been trained.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return A RFEAT object
RFEAT_object <- function(data, x, y, register_names, numStop, m, s_mtry, na.rm, forest, error, OOB) {
  
  # Output and input names
  output_names <- names(data)[y]
  input_names <- names(data)[x]
  
  RFEAT_object <- list("data" = list(df = data %>% select(-id),
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
                       "error" = error,
                       "OOB" = OOB)
  
  class(RFEAT_object) <- "RFEAT"
  
  return(RFEAT_object)
  
}


