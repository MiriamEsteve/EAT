#' @title Data preprocessing
#'
#' @description This function displays error messages in relation with data such as presence of NA values or not allowed classes of data. Also, it prepares the data in the required format and gets the names of the rows.
#'
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param na.rm Logical. If True, NA rows are omitted. If False, an error occurs in case of NA rows.
#'
#' @importFrom stats na.omit
#'
#' @return List containing rownames and data processed in the [X, Y] format with only allowed classes.
preProcess <- function(data, x, y, na.rm = T) {
  data <- as.data.frame(data)
  
  for (i in y) {
    if (is.numeric(data[, i]) || is.integer(data[, i]) || is.double(data[, i])) {
      next
    } else {
      stop(paste("The variable", names(data[i]), "must be numeric, integer or double \n"))
    }
  }
  
  for (i in x) {
    if(is.ordered(data[, i])){
      data[, i] <- as.numeric(data[, i])
    } else if (is.numeric(data[, i]) || is.integer(data[, i]) || is.double(data[, i])){
      next
    } else {
      stop(paste("The variable", names(data[i]), "must be numeric, integer or double. In case of an ordinal variable, order = T is necessary. \n"))
    }
  }

  if (any(is.na(data[, c(x, y)]))){
    if (na.rm == T){
      if (sys.calls()[[sys.nframe()-1]] == "predict(tree, data, x, y)"){
        data <- data
      } else {
        data <- na.omit(data)
        warning("Rows with NA values have been omitted \n")
      }
    } else {
      stop("Presence of NA values. Please, detele or impute those registers or set na.rm = TRUE to omit them. \n")
    }
  }

  data <- data[, c(x, y)]
  
  rwn <- row.names(data)

  return(list(rwn, data))
}


