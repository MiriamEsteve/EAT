#' @title Data preprocessing
#'
#' @description This function displays error messages in relation with data such as presence of NA values, types of data not allowed or arguments entered incorrectly. Also, it prepares the data in the required format.
#'
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimum number of observations in a node for a split to be attempted.
#' @param fold Integer. Set of number of folds in which the dataset to apply cross-validation during the pruning is divided.
#' @param max.depth Integer. Depth of the tree.
#' @param max.leaves Integer. Maximum number of leaf nodes.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'
#' @importFrom stats na.omit
#'
#' @return List containing rownames and data processed in the [X, Y] format with only categories allowed.
preProcess <- function(data, x, y, numStop = 5, fold = 5, 
                       max.depth = NULL, max.leaves = NULL, 
                       na.rm = TRUE) {
  
  # fold argument bad introduced
  
  if (!is.null(fold) && fold < 2) {
    stop('fold = ', fold, ' must be greater than or equal 2.')
  }
  
  # numStop argument bad introduced
  
  if (!is.null(numStop) && numStop < 0) {
    stop('numStop = ', numStop, ' must be greater than or equal 0.')
  }
  
  # max.depth  bad introduced
  
  if (!is.null(max.depth) && max.depth <= 0) {
    stop('max.depth = ', max.depth, ' must be greater than 0.')
  }
  
  # max.leaves bad introduced
  
  if (!is.null(max.leaves) && max.leaves <= 0) {
    stop('max.leaves = ', max.leaves, ' must be greater than 0.')
  }
  
  # x and y well / bad introduced
  
  cols <- 1:length(data)
  if (!(all(x %in% cols) && all(y %in% cols))) {
    stop("x or y indexes are not in data.")
  }
  
  # Dataframe
  # List with variables
  # Matix
  
  if (is.list(data) && !is.data.frame(data)){
    
    # Data names?
    
    ifelse(is.null(names(data)), 
           nms <- 1:length(data), # if not 1:x
           nms <- names(data))
    
    data <- data.frame(matrix(unlist(data), ncol = length(nms), byrow = F))
    names(data) <- nms
    
  } else if (is.matrix(data) || is.data.frame(data)) {
    
    data <- data.frame(data)
  }
  
  # Classes
  varClass <- unlist(sapply(data, class))
  
  # Output classes
  outClass <- varClass[y] %in% c("numeric", "double", "integer")
  
  # Error
  if (!all(outClass)){
    stop(paste(names(data)[y][!outClass][1], "is not a numeric or integer vector"))
  }
  
  # Input classes
  # Ordered --> numeric
  for (i in x){
    if (is.ordered(data[, i])) {
      data[, i] <- as.numeric(data[, i])
    }
  }
  
  # Define classes again
  varClass <- unlist(sapply(data, class))
  
  inpClass <- varClass[x] %in% c("numeric", "double", "integer")
  
  # Error
  if (!all(inpClass)){
    stop(paste(names(data)[x][!inpClass][1], "is not a numeric, integer or ordered vector"))
  }
  
  data <- data[, c(x, y)]
  
  # NA values

  if (any(is.na(data))){
    if (na.rm == T){
      data <- na.omit(data)
      warning("Rows with NA values have been omitted .\n")
      
      } else {
      stop("Please, detele or impute NA registers or set na.rm = TRUE to omit them. \n")
    }
  }
  
  rwn <- row.names(data)
  
  return(list(rwn, data))
}


