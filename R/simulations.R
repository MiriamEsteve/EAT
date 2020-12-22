#' @title Single output data generation
#'
#' @description This function is used to simulate the data in a single output scenario.
#'
#' @param N Integer. Sample size.
#' @param nX Integer. Number of inputs. \code{1}, \code{3}, \code{6}, \code{9}, \code{12} and \code{15} are acceptable.
#'
#' @importFrom dplyr %>%
#' @importFrom stats runif rnorm
#'
#' @return Data frame with the values of a scenario with nX inputs and nY outputs.
sgle.out_scenario <- function(N, nX) {
  if(!(nX %in% c(1, 3, 6, 9, 12, 15))){
    stop(paste(nX, "is not allowed"))
  }
  
  colnames <- c(paste("x", 1:nX, sep = ""), "y")
  
  data <- matrix(
    ncol = length(colnames),
    nrow = N,
    dimnames = list(NULL, colnames)
  ) %>% as.data.frame()
  
  for (x in 1:nX){
    data[, x] <- runif(n = N, min = 1, max = 10)
  }
  
  u <- abs(rnorm(n = N, mean = 0, sd = 0.4))
  
  if (nX == 1){
    y <- 3 * log(data[, "x1"])
    data[, "y"] <- y * exp(-u)
    data[, "yD"] <- y
    
  } else if (nX == 3){
    y <- 3 * (data[, "x1"] ** 0.05) * (data[, "x2"] ** 0.15) * (data[, "x3"] ** 0.3)
    data[, "y"] <- y * exp(-u)
    data[, "yD"] <- y
    
  } else if (nX == 6){
    y <- 3 * (data[, "x1"] ** 0.05) * (data[, "x2"] ** 0.001) * (data[, "x3"] ** 0.004) *
      (data[, "x4"] ** 0.045) * (data[, "x5"] ** 0.1) * (data[, "x6"] ** 0.3)
    data[, "y"] <- y * exp(-u)
    data[, "yD"] <- y
    
  } else if (nX == 9){
    y <- 3 * (data[, "x1"] ** 0.005) * (data[, "x2"] ** 0.001) * (data[, "x3"] ** 0.004) *
      (data[, "x4"] ** 0.005) * (data[, "x5"] ** 0.001) * (data[, "x6"] ** 0.004) *
      (data[, "x7"] ** 0.08) * (data[, "x8"] ** 0.1) * (data[, "x9"] ** 0.3)
    data["y"] <- y * exp(-u)
    data["yD"] <- y
    
  } else if (nX == 12){
    y <- 3 * (data[, "x1"] ** 0.005) * (data[, "x2"] ** 0.001) * (data[, "x3"] ** 0.004) *
      (data[, "x4"] ** 0.005) * (data[, "x5"] ** 0.001) * (data[, "x6"] ** 0.004) *
      (data[, "x7"] ** 0.08) * (data[, "x8"] ** 0.05) * (data[, "x9"] ** 0.05) *
      (data[, "x10"] ** 0.075) * (data[, "x11"] ** 0.025) * (data[, "x12"] ** 0.2)
    data["y"] <- y * exp(-u)
    data["yD"] <- y
  
  } else {
    y <- 3 * (data[, "x1"] ** 0.005) * (data[, "x2"] ** 0.001) * (data[, "x3"] ** 0.004) *
      (data[, "x4"] ** 0.005) * (data[, "x5"] ** 0.001) * (data[, "x6"] ** 0.004) *
      (data[, "x7"] ** 0.08) * (data[, "x8"] ** 0.05) * (data[, "x9"] ** 0.05) *
      (data[, "x10"] ** 0.05) * (data[, "x11"] ** 0.025) * (data[, "x12"] ** 0.025) *
      (data[, "x13"] ** 0.025) * (data[, "x14"] ** 0.025) * (data[, "x15"] ** 0.15)
    data["y"] <- y * exp(-u)
    data["yD"] = y
  }
  
  return(data)
}


#' @title 2 inputs & 2 outputs scenario
#'
#' @description This function is used to simulate the data in a scenario with 2 inputs and 2 outputs.
#'
#' @param N Sample size.
#' @param border Percentage of DMU's in the frontier.
#' @param noise Random noise.
#'
#' @importFrom dplyr %>%
#' @importFrom stats runif rnorm
#'
#' @return Data frame with the values of a scenario with nX inputs and nY outputs.
two.out_two.in_scenario <- function(N, border, noise = NULL) {
  nX <- 2
  nY <- 2
  
  colnames <- c(paste("x", 1:nX, sep = ""), paste("y", 1:nY, sep = ""))

  data <- matrix(
    ncol = length(colnames),
    nrow = N,
    dimnames = list(NULL, colnames)
  ) %>% as.data.frame()

  data[, 1:nX] <- runif(N, 5, 50)

  z <- runif(N, -1.5, 1.5)

  ln_x1 <- log(data[, "x1"])
  ln_x2 <- log(data[, "x2"])

  op1 <- -1 + 0.5 * z + 0.25 * (z**2) - 1.5 * ln_x1

  op2 <- -0.6 * ln_x2 + 0.2 * (ln_x1**2) + 0.05 * (ln_x2**2) - 0.1 * ln_x1 * ln_x2

  op3 <- 0.05 * ln_x1 * z - 0.05 * ln_x2 * z

  ln_y1_ast <- -(op1 + op2 + op3)

  data[, "y1"] <- exp(ln_y1_ast)

  data[, "y2"] <- exp(ln_y1_ast + z)

  if (border > 0) {
    index <- sample(1:N, N * border)

    N_sample <- length(index)

    half_normal <- rnorm(N_sample, 0, 0.3**(1 / 2)) %>%
      abs()
    
    half_normal <- exp(half_normal)

    if (!is.null(noise)) {
      normal1 <- rnorm(N_sample, 0, 0.01**(1 / 2))
      normal1 <- exp(normal1)
      
      normal2 <- rnorm(N_sample, 0, 0.01**(1 / 2))
      normal2 <- exp(normal2)

      data[index, "y1"] <- data[index, "y1"] / (half_normal * normal1)
      data[index, "y2"] <- data[index, "y2"] / (half_normal * normal2)
    } else {
      data[index, "y1"] <- data[index, "y1"] / half_normal
      data[index, "y2"] <- data[index, "y2"] / half_normal
    }
  }

  return(data)
}
