#' @title Banker, Charnes and Cooper programming model with output orientation for a convex Efficiency Analysis Tree model
#'
#' @description Banker, Charnes and Cooper programming model with output orientation for a convex Efficiency Analysis Tree model.
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
CEAT_BCC_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Banker, Charnes and Cooper programming model with input orientation for a convex Efficiency Analysis Tree model
#'
#' @description Banker, Charnes and Cooper programming model with input orientation for a convex Efficiency Analysis Tree model.
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
CEAT_BCC_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Directional Distance Function mathematical programming model for a convex Efficiency Analysis Tree model
#'
#' @description Directional Distance Function for a convex Efficiency Analysis Tree model.
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
CEAT_DDF <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Russell Model with input orientation for a convex Efficiency Analysis Tree model
#'
#' @description Russell Model with input orientation for a convex Efficiency Analysis Tree model.
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
CEAT_RSL_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Russell Model with output orientation for a convex Efficiency Analysis Tree model
#'
#' @description Russell Model with output orientation for a convex Efficiency Analysis Tree model.
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
CEAT_RSL_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Weighted Additive Model for a convex Efficiency Analysis Tree model
#'
#' @description Weighted Additive Model for a convex Efficiency Analysis Tree model.
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
#' @param weights Character. \code{"MIP"} for Measure of Inefficiency Proportion or \code{"RAM"} for Range Adjusted Measure of Inefficiency.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
CEAT_WAM <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, weights) {
  
  # Range for RAM measures
  
  if (weights == "RAM") {
    InputRanges <- apply(x_k, 2, max) - apply(x_k, 2, min)
    OutputRanges <- apply(y_k, 2, max) - apply(y_k, 2, min)
    
    ranges <- c(InputRanges, OutputRanges) / (nX + nY)
  }
  
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nY + nX, nrow = 1)
    
    if (weights == "MIP") {
      objVal[1:(nX + nY)] <- c(1 / x_k[d, ], 1 / y_k[d, ]) 
      
    } else if (weights == "RAM"){
      objVal[1:(nX + nY)] <- ranges
      
    }
    
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
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Convex Efficiency Analysis Tree Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU through a convex Efficiency Analysis Trees model.
#' 
#' @param data Dataframe containing the DMU for which the efficiency score is calculated.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param object An EAT object.
#' @param scores_model Mathematical programming model to calculate scores. 
#' \itemize{
#' \item{\code{BCC.OUT}} BCC model. Output-oriented. Efficiency level at 1.
#' \item{\code{BCC.INP}}  BCC model. Input-oriented. Efficiency level at 1.
#' \item{\code{DDF}}     Directional Distance Function. Efficiency level at 0.
#' \item{\code{RSL.OUT}} Russell model. Output-oriented. Efficiency level at 1.
#' \item{\code{RSL.INP}}  Russell model. Input-oriented. Efficiency level at 1.
#' \item{\code{WAM.MIP}} Weighted Additive Model. Measure of Inefficiency Proportions. Efficiency level at 0.
#' \item{\code{WAM.RAM}} Weighted Additive Model. Range Adjusted Measure of Inefficiency. Efficiency level at 0.
#' }
#' @param digits Integer. Decimal units for scores.
#' @param DEA Logical. If \code{TRUE}, DEA scores are calculated with the programming model selected in \code{scores_model}
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#' @export
#' 
#' @examples
#' 
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#'
#' efficiencyCEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'               scores_model = "BCC.OUT", digits = 2, DEA = TRUE, na.rm = TRUE)
#' }
#'
#' @return Dataframe with input variables and efficiency scores through a convex EAT model.
efficiencyCEAT <- function(data, x, y, object, 
                           scores_model, digits = 3, DEA = TRUE,
                           na.rm = TRUE) {
  
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
    
  } 
  
  if (digits < 0) {
    stop(paste('digits =', digits, 'must be greater than 0.'))
  }
  
  if (!scores_model %in% c("BCC.OUT", "BCC.INP", "DDF", 
                           "RSL.OUT", "RSL.INP", "WAM.MIP",
                           "WAM.RAM")){
    stop(paste(scores_model, "is not available. Please, check help(\"efficiencyCEAT\")"))
  }
  
  rwn_data <- preProcess(data, x, y, na.rm = na.rm)
  
  rwn <- rwn_data[[1]]
  data <- rwn_data[[2]]
  
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  if (!identical(sort(train_names), sort(names(data)))) {
    stop("Different variable names in training and data set")
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
  
  if (scores_model == "BCC.OUT"){
    scores <- CEAT_BCC_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    CEAT_model <- "CEAT_BCC_OUT"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_BCC_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      DEA_model <- "DEA_BCC_OUT"
    }

  } else if (scores_model == "BCC.INP"){
    scores <- CEAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    CEAT_model <- "CEAT_BCC_INP"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_BCC_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      DEA_model <- "DEA_BCC_INP"
    }

  } else if (scores_model == "DDF"){
    scores <- CEAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    CEAT_model <- "CEAT_DDF"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_DDF(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      DEA_model <- "DEA_DDF"
    }

  } else if (scores_model == "RSL.OUT"){
    scores <- CEAT_RSL_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    CEAT_model <- "CEAT_RSL_OUT"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_RSL_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      DEA_model <- "DEA_RSL_OUT"
    }

  } else if (scores_model == "RSL.INP"){
    scores <- CEAT_RSL_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    CEAT_model <- "CEAT_RSL_INP"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_RSL_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      DEA_model <- "DEA_RSL_INP"
    }

  } else if (scores_model == "WAM.MIP"){
    scores <- CEAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, "MIP")
    CEAT_model <- "CEAT_WAM_MIP"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_WAM(j, scores, x_k, y_k, x_k, y_k, nX, nY, j, "MIP")
      DEA_model <- "DEA_WAM_MIP"
    }
    
  } else if (scores_model == "WAM.RAM"){
    scores <- CEAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, "RAM")
    CEAT_model <- "CEAT_WAM_RAM"
    
    if (DEA == TRUE){
      scores_DEA <- CEAT_WAM(j, scores, x_k, y_k, x_k, y_k, nX, nY, j, "RAM")
      DEA_model <- "DEA_WAM_RAM"
    }
  }

  scores <- as.data.frame(scores)
  names(scores) <- CEAT_model
  rownames(scores) <- rwn
  
  descriptive <- scores %>%
    summarise("Model" = "CEAT",
              "Mean" = round(mean(scores[, 1]), digits),
              "Std. Dev." = round(sd(scores[, 1]), digits),
              "Min" = round(min(scores[, 1]), digits),
              "Q1" = round(quantile(scores[, 1])[[2]], digits),
              "Median" = round(median(scores[, 1]), digits),
              "Q3" = round(quantile(scores[, 1])[[3]], digits),
              "Max" = round(max(scores[, 1]), digits)
              )
  
  if (DEA == TRUE){
    scores_DEA <- as.data.frame(scores_DEA)
    names(scores_DEA) <- DEA_model
    rownames(scores_DEA) <- rwn
    
    descriptive[2, ] <- scores %>%
      summarise("Model" = "DEA",
                "Mean" = round(mean(scores_DEA[, 1]), digits),
                "Std. Dev." = round(sd(scores_DEA[, 1]), digits),
                "Min" = round(min(scores_DEA[, 1]), digits),
                "Q1" = round(quantile(scores_DEA[, 1])[[2]], digits),
                "Median" = round(median(scores_DEA[, 1]), digits),
                "Q3" = round(quantile(scores_DEA[, 1])[[3]], digits),
                "Max" = round(max(scores_DEA[, 1]), digits)
      )
    
    scores_df <- cbind(data, round(scores, digits), round(scores_DEA, digits))
    print(scores_df[, c(ncol(scores_df) - 1, ncol(scores_df))])
    
    cat("\n")
    print(descriptive, row.names = FALSE)
    
    invisible(scores_df)
    
  } else {
    
    scores_df <- cbind(data, round(scores, digits))
    print(round(scores_df[, ncol(scores_df)], digits))
    
    cat("\n") 
    print(descriptive, row.names = FALSE)
        
    invisible(scores_df)
    
  }
}