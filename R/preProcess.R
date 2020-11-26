#' @title Data errors
#'
#' @description This function displays error messages in relation with data such as presence of NA or not allowed classes of data.
#'
#' @param data Data to be used. Dataframe or matrix objects are acceptable.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param na.rm If True, NA rows are omitted.
#' @param check If check = 1, the function comes from predictor function and thus NA's should not be omitted.
#'
#' @importFrom dplyr %>% mutate row_number
#' @importFrom stats na.omit
#'
#' @return Data processed
preProcess <- function(data, x, y, na.rm = T, check = 0) {
  data <- data %>% as.data.frame()

  # Evaluación de variables respuesta. Si no es numerica o entera, lanzar mensaje de error

  for (i in y) {
    if (is.numeric(data[, i]) || is.integer(data[, i]) || is.double(data[, i])) {
      next
    } else {
      stop(cat("The variable", names(data[i]), "is not valid. Only numeric variables are allowed as response variable. \n"))
    }
  }

  # Evaluación de variables explicativas

  for (i in x) {
    if(is.ordered(data[, i])){
      data[, i] <- as.numeric(data[, i])
    } else if (is.numeric(data[, i]) || is.integer(data[, i]) || is.double(data[, i])){
      next
    } else {
      stop(cat("The variable", names(data[i]), "is not valid. Only numeric or ordened factors classes are allowed as explanatory variable. In case of an ordinal variable, you should order = T. \n"))
    }
  }

  # Evaluación de NA's
  if (any(is.na(data[, c(x, y)]))){
    if (na.rm == T){
      if (check == 1){
        data <- data
      } else {
        data <- na.omit(data)
        warning("Rows with NA values have been omitted")
      }
    } else {
      stop("Presence of NA values")
    }
  }

  data <- data[, c(x, y)] %>%
    mutate(id = row_number())

  return(data)
}


