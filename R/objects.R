#' @title Create a EAT object
#'
#' @description This function saves information about the Efficiency Analysis Trees model.
#'
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param rownames \code{string}. Data rownames.
#' @param numStop Minimum number of observations in a node for a split to be attempted.
#' @param fold Set of number of folds in which the dataset to apply cross-validation during the pruning is divided.
#' @param max.depth Maximum number of leaf nodes.
#' @param max.leaves Depth of the tree.
#' @param na.rm \code{logical}. If \code{TRUE}, \code{NA} rows are omitted. If \code{FALSE}, an error occurs in case of \code{NA} rows.
#' @param tree \code{list} containing the nodes of the Efficiency Analysis Trees pruned model.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return An \code{EAT} object.
EAT_object <- function(data, x, y, rownames, numStop, fold, max.depth, max.leaves, na.rm, tree) {
  
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
  
  # Nodes data frame for results
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
                                   row_names = rownames),
                     "control" = list(fold = fold, 
                                      numStop = numStop,
                                      max.leaves = max.leaves,
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

#' @title Create a RFEAT object
#'
#' @description This function saves information about the Random Forest for Efficiency Analysis Trees model.
#' 
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param rownames \code{string}. Data rownames.
#' @param numStop Minimun number of observations in a node for a split to be attempted.
#' @param m Number of trees to be built.
#' @param s_mtry Select number of inputs in each split.
#' \itemize{
#' \item{\code{"Breiman"}}: \code{in / 3}
#' \item{\code{"DEA1"}}: \code{(t.obs / 2) - out}  
#' \item{\code{"DEA2"}}: \code{(t.obs / 3) - out}
#' \item{\code{"DEA3"}}: \code{t.obs - 2 * out}
#' \item{\code{"DEA4"}}: \code{min(t.obs / out, (t.obs / 3) - out)}
#' }
#' @param na.rm \code{logical}. If \code{TRUE}, \code{NA} rows are omitted.
#' @param forest \code{list} containing the individual Efficiency Analysis Trees.
#' @param error Error in Random Forest for Efficiency Analysis Trees.
#' @param OOB \code{list} containing the observations with which each tree has been trained.
#'
#' @importFrom dplyr %>% select filter
#'
#' @return A \code{RFEAT} object.
RFEAT_object <- function(data, x, y, rownames, numStop, m, s_mtry, na.rm, forest, error, OOB) {
  
  # Output and input names
  output_names <- names(data)[y]
  input_names <- names(data)[x]
  
  RFEAT_object <- list("data" = list(df = data %>% select(-id),
                                     x = x,
                                     y = y,
                                     input_names = input_names,
                                     output_names = output_names,
                                     row_names = rownames),
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


