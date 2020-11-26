#' @title Multioutput data generation
#'
#' @description This function is used to simulate the data in a multioutput scenario.
#'
#' @param N Sample size.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param border Percentage of DMU's in the frontier.
#' @param noise Random noise.
#'
#' @importFrom dplyr %>%
#' @importFrom stats runif rnorm
#'
#' @export
#'
#' @return Data frame with the values of a scenario with nX inputs and nY outputs.
simulated_data <- function(N, nX, nY, border, noise = NULL) {
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
      abs() %>%
      exp()

    if (!is.null(noise)) {
      normal1 <- rnorm(N_sample, 0, 0.01**(1 / 2)) %>% exp()
      normal2 <- rnorm(N_sample, 0, 0.01**(1 / 2)) %>% exp()

      data[index, "y1"] <- data[index, "y1"] / (half_normal * normal1)
      data[index, "y2"] <- data[index, "y2"] / (half_normal * normal2)
    } else {
      data[index, "y1"] <- data[index, "y1"] / half_normal
      data[index, "y2"] <- data[index, "y2"] / half_normal
    }
  }

  return(data)
}
