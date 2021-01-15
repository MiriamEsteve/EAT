#' @title Banker, Charnes and Cooper programming model with output orientation for an EAT model
#'
#' @description Banker, Charnes and Cooper programming model with output orientation for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_BCC_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + 1, nrow = 1)
    objVal[1] <- 1
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(0, atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(- y_k[d, yi], ytreeTk[, yi]), ">=", rhs = 0)
    }
    
    # Constrain 2.3 - phi = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Banker, Charnes and Cooper programming model with input orientation for an EAT model
#'
#' @description Banker, Charnes and Cooper programming model with input orientation for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_BCC_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + 1, nrow = 1)
    objVal[1] <- 1
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(- x_k[d, xi], atreeTk[, xi]), "<=",  rhs = 0)
    }
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(0, ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Directional Distance Function mathematical programming model for an EAT model
#'
#' @description Directional Distance Function for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_DDF <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + 1, nrow = 1)
    objVal[1] <- 1
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(x_k[d, xi], atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(- y_k[d, yi], ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Rusell Model with input orientation for an EAT model
#'
#' @description Rusell Model with input orientation for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_RSL_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nX, nrow = 1)
    objVal[1:nX] <- 1 / nX
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + nX)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      vec <- c()
      vec[xi] <- - x_k[d, xi]
      vec[(1:nX)[- xi]] <- 0
      vec[(nX + 1):(nX + N_leaves)] <- atreeTk[, xi]
      
      add.constraint(lps, xt = vec, "<=",  rhs = 0)
    }
    
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(rep(0, nX), ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nX), rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + nX, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Rusell Model with output orientation for an EAT model
#'
#' @description Rusell Model with output orientation for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_RSL_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nY, nrow = 1)
    objVal[1:nY] <- 1 / nY
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + nY)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(rep(0, nY), atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    
    for(yi in 1:nY)
    {
      vec <- c()
      vec[yi] <- - y_k[d, yi]
      vec[(1:nY)[- yi]] <- 0
      vec[(nY + 1):(nY + N_leaves)] <- ytreeTk[, yi]
      
      add.constraint(lps, xt = vec, ">=", rhs = 0)
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nY), rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + nY, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Weighted Additive Model for an EAT model
#'
#' @description Weighted Additive Model for an EAT model.
#'
#' @param j Integer. Number of DMUs.
#' @param scores Matrix. Empty matrix for scores.
#' @param x_k Dataframe. Set of input variables.
#' @param y_k Dataframe. Set of output variables.
#' @param atreeTk Matrix. Set of "a" Pareto-coordinates.
#' @param ytreeTk Matrix. Set of predictions.
#' @param nX Integer. Number of inputs.
#' @param nY Integer. Number of outputs.
#' @param N_leaves Integer. Number of leaf nodes.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_WAM <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nY + nX, nrow = 1)
    objVal[1:(nX + nY)] <- c(1 / x_k[d, ], 1 / y_k[d, ])
    
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + nY + nX)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      vec <- c()
      vec[xi] <- 1
      vec[(1:nX)[- xi]] <- 0
      vec[(nX + 1):(nX + nY)] <- 0
      vec[(nX + nY + 1):(nY + nX + N_leaves)] <- atreeTk[, xi]
      
      add.constraint(lps, xt = vec, "<=",  rhs = x_k[d, xi])
    }
    
    for(yi in 1:nY)
    {
      vec <- c()
      vec[1:nX] <- 0
      vec[nX + yi] <- - 1
      vec[((nX + 1):(nX + nY))[- yi]] <- 0
      vec[(nX + nY + 1):(nY + nX + N_leaves)] <- ytreeTk[, yi]
      
      add.constraint(lps, xt = vec, ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nY + nX), rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + (nX + nY), type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title EAT Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU by an EAT model.
#' 
#' @param data Dataframe for which the efficiency score is calculated.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param object An EAT object.
#' @param scores_model Mathematic programming model to calculate scores. 
#' \itemize{
#' \item{\code{BCC_out}} BBC model. Output-oriented.
#' \item{\code{BCC_in}}  BBC model. Input-oriented.
#' \item{\code{DDF}}     Directional Distance Function.
#' \item{\code{RSL_out}} Rusell model. Output-oriented.
#' \item{\code{RSL_in}}  Rusell model. Input-oriented.
#' \item{\code{WAM}}     Weighted Additive Model.
#' }
#' @param r Integer. Decimal units for scores.
#' @param FDH. Logical. If \code{TRUE}, FDH scores are calculated with the programming model selected in \code{scores_model}
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#' @export
#' 
#' @examples
#' 
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#' Y2 <- log(X1) + 2 - abs(rnorm(50, mean = 0, sd = 0.7))
#'
#' simulated <- data.frame(x1 = X1, x2 = X2, y1 = Y1, y2 = Y2)
#' 
#' n <- nrow(simulated)
#' t_index <- sample(1:n, n * 0.8)
#' training <- simulated[t_index, ]
#' test <- simulated[-t_index, ]
#' 
#' EAT_model <- EAT(data = training, x = c(1,2), y = c(3, 4), numStop = 3, fold = 7)
#'
#' efficiencyEAT(data = training, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'               scores_model = "BCC_out", r = 2, na.rm = TRUE)
#' 
#' efficiencyEAT(data = test, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'               scores_model = "BCC_out", r = 2, na.rm = TRUE)
#'
#' @return Dataframe with input variables and efficiency scores by an EAT model.
efficiencyEAT <- function(data, x, y, object, 
                          scores_model, r = 4, FDH = TRUE,
                          na.rm = TRUE) {
  
  if (!scores_model %in% c("BCC_out", "BCC_in", "DDF", 
                           "RSL_out", "RSL_in", "WAM")){
    stop(paste(scores_model, "is not available. Please, check help(efficiencyEAT)"))
  }
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  rwn_data <- preProcess(data, x, y, na.rm = T)
  
  rwn <- rwn_data[[1]]
  data <- rwn_data[[2]]
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  data_names <- names(data)
  
  if (!is.data.frame(data)){
    stop("data must be a data.frame")
  } else if (length(train_names) != length(data_names)){
    stop("Training and prediction data must have the same number of variables")
  } else if (!all(train_names == data_names)){
    stop(paste("Variable name: ", data_names[1], "not found in taining data"))
  }

  j <- nrow(data)
  scores <- matrix(nrow = j, ncol = 1)
  x_k <- as.matrix(data[, x])
  y_k <- as.matrix(data[, y])
  nX <- length(x)
  nY <- length(y)
  
  atreeTk <- object[["model"]][["a"]]
  ytreeTk <- object[["model"]][["y"]]
  N_leaves <- object[["model"]][["leaf_nodes"]]
  
  if (scores_model == "BCC_out"){
    scores <- EAT_BCC_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_BCC_out"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_BCC_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_BCC_out"
    }

  } else if (scores_model == "BCC_in"){
    scores <- EAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_BCC_in"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_BCC_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_BCC_in"
    }

  } else if (scores_model == "DDF"){
    scores <- EAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_DDF"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_DDF(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_DDF"
    }

  } else if (scores_model == "RSL_out"){
    scores <- EAT_RSL_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_RSL_out"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_RSL_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_RSL_out"
    }

  } else if (scores_model == "RSL_in"){
    scores <- EAT_RSL_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_RSL_in"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_RSL_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_RSL_in"
    }

  } else if (scores_model == "WAM"){
    scores <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_WAM"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_WAM(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_WAM"
    }
  }

  scores <- as.data.frame(scores)
  names(scores) <- EAT_model
  rownames(scores) <- rwn
  
  descriptive <- scores %>%
    summarise("Model" = "EAT",
              "Mean" = round(mean(scores[, 1]), 2),
              "Std. Dev." = round(sd(scores[, 1]), 2),
              "Min" = round(min(scores[, 1]), 2),
              "Q1" = round(quantile(scores[, 1])[[2]], 2),
              "Median" = round(median(scores[, 1]), 2),
              "Q3" = round(quantile(scores[, 1])[[3]], 2),
              "Max" = round(max(scores[, 1]), 2)
              )
  
  if (FDH == TRUE){
    scores_FDH <- as.data.frame(scores_FDH)
    names(scores_FDH) <- FDH_model
    rownames(scores_FDH) <- rwn
    
    descriptive_FDH <- scores %>%
      summarise("Model" = "FDH",
                "Mean" = round(mean(scores_FDH[, 1]), 2),
                "Std. Dev." = round(sd(scores_FDH[, 1]), 2),
                "Min" = round(min(scores_FDH[, 1]), 2),
                "Q1" = round(quantile(scores_FDH[, 1])[[2]], 2),
                "Median" = round(median(scores_FDH[, 1]), 2),
                "Q3" = round(quantile(scores_FDH[, 1])[[3]], 2),
                "Max" = round(max(scores_FDH[, 1]), 2)
      )
  }
  
  if (FDH == TRUE){
    
    scores_df <- cbind(data, round(scores, r), round(scores_FDH, r))
    print(scores_df[, c(ncol(scores_df) - 1, ncol(scores_df))])
    
    cat("\n")
    print(descriptive, row.names = FALSE)
    cat("\n")
    print(descriptive_FDH, row.names = FALSE)
    
    invisible(scores_df)
    
  } else {
    
    scores_df <- cbind(data, round(scores, r))
    print(round(scores_df[, ncol(scores_df)], r))
    
    cat("\n") 
    print(descriptive, row.names = FALSE)
        
    
    invisible(scores_df)
    
  }
}