#' @title Banker, Charnes and Cooper programming model with output orientation.
#'
#' @description Banker, Charnes and Cooper programming model with output orientation.
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

#' @title Banker, Charnes and Cooper programming model with input orientation.
#'
#' @description Banker, Charnes and Cooper programming model with input orientation.
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

#' @title Directional Distance Function mathematical programming model
#'
#' @description Directional Distance Function.
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

#' @title Rusell model with input orientation
#'
#' @description Rusell model with input orientation.
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

#' @title Rusell model with output orientation.
#'
#' @description Rusell model with output orientation.
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

#' @title Weighted Additive mathematical programming model
#'
#' @description Weighted Additive mathematical programming model.
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

#' @title Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU.
#' 
#' @param data Dataframe for which the efficiency score is calculated.
#' @param object An EAT object.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param scores_model Mathematic programming model to calculate scores. 
#' \itemize{
#' \item{\code{EAT_BCC_out}} BBC model. Output orientation.
#' \item{\code{EAT_BCC_in}}  BBC model. Input orientation.
#' \item{\code{EAT_DDF}}     Directional distance model.
#' \item{\code{EAT_RSL_out}} Rusell model. Output orientation
#' \item{\code{EAT_RSL_in}}  Rusell model. Input orientation.
#' \item{\code{EAT_WAM}}     Weighted Additive model.
#' }
#' @param r Integer. Decimal units for scores.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#'
#' @export
#'
#' @return Efficiency scores
efficiency_EAT <- function(data, x, y, object, 
                           scores_model, r = 4, na.rm = TRUE) {
  
  if (!scores_model %in% c("EAT_BCC_out", "EAT_BCC_in", "EAT_DDF", 
                           "EAT_RSL_out", "EAT_RSL_in", "EAT_WAM")){
    stop("You should choose an available model. Please check help(efficiency_scores)")
  }
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  data <- preProcess(data, x, y, na.rm = T)
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  data_names <- names(data)
  
  if (!is.data.frame(data)){
    stop("newdata must be a data.frame")
  } else if (length(train_names) != length(data_names)){
    stop("Training and prediction data must have the same number of variables")
  } else if (!all(train_names == data_names)){
    stop(cat("Variable name: ", data_names[1], "not found in taining data"))
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
  
  if (scores_model == "EAT_BCC_out"){
    scores <- EAT_BCC_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_BCC_in"){
    scores <- EAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_DDF"){
    scores <- EAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_RSL_out"){
    scores <- EAT_RSL_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_RSL_in"){
    scores <- EAT_RSL_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_WAM"){
    scores <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
  }

  scores <- as.data.frame(scores)
  names(scores) <- scores_model
  rownames(scores) <- object[["data"]][["row_names"]]

  descriptive <- scores %>%
    summarise("Mean" = round(mean(scores[, 1]), 2),
              "Std. Dev." = round(sd(scores[, 1]), 2),
              "Min" = round(min(scores[, 1]), 2),
              "Q1" = round(quantile(scores[, 1])[[2]], 2),
              "Median" = round(median(scores[, 1]), 2),
              "Q3" = round(quantile(scores[, 1])[[3]], 2),
              "Max" = round(max(scores[, 1]), 2)
              )
  
  print(kable(descriptive), "pipe")

  return(round(scores, r))
}

#' @title Efficiency Scores Jitter Plot
#'
#' @description This function returns a jitter plot. DMUs are grouped in nodes (same input paradigm) and its score are showed. A black point represents the score mean in a group and the line the standard deviation. Finally, the user can specify an upper bound (upb) and a lower bound (lwb) in order to show the labels.
#'
#' @param object An EAT object.
#' @param scores Dataframe with scores.
#' @param upb Numeric. Upper bound for labeling.
#' @param lwb Numeric. Lower bound for labeling.
#'
#' @importFrom ggplot2 ggplot aes geom_jitter stat_summary position_jitter
#' @importFrom dplyr %>% select
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
#' @return Geom jitter plot with DMUs and scores.
efficiency_jitter <- function(object, scores, upb = 1, lwb = 0) {
  
  groups <- object[["nodes_df"]][["leafnodes_df"]] %>%
    select(id, index)
  
  scores_df <- data.frame(Score = scores,
                          Group = NA)
  
  names(scores_df) <- c("Score", "Group")
  
  rownames(scores_df) <- object[["data"]][["row_names"]]
  
  for (i in 1:nrow(scores_df)){
    scores_df[groups[i, "index"][[1]], "Group"] <- groups[i, "id"]
  }
  
  scores_df$Group <- as.factor(scores_df$Group)
  
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m - sd(x)
    ymax <- m + sd(x)
    return(c(y = m, ymin = ymin, ymax = ymax))
  }
  
  jitter_plot <- ggplot(scores_df, aes(x = Group, y = Score, color = Group)) + 
    geom_jitter(position = position_jitter(0.2), size = 2) +
    stat_summary(fun.data = data_summary, color = "black") +
    geom_text_repel(aes(label = ifelse(Score >= lwb & Score <= upb, 
                                       rownames(scores_df), "")))
  
  return(jitter_plot)
}

#' @title Efficiency Scores Density Plot
#'
#' @description Density plot for scores. EAT scores in red and FDH scores in blue.
#'
#' @param object An EAT object.
#' @param scores Dataframe with scores.
#' @param FDH Logical. If FDH = TRUE, density plot for FDH is showed too.
#'
#' @importFrom ggplot2 ggplot aes_string geom_histogram geom_density
#'
#' @export
#'
#' @return Density plot for scores
efficiency_density <- function(object, scores, FDH = TRUE) {
  
  efficiency_density <- ggplot() +
    geom_density(data = scores, aes_string(names(scores)[1]), alpha = .3, 
                 fill = "#FF583A", colour = "#FF583A") +
    xlab("Score") +
    ylab("Density")
  
  if (FDH == TRUE){
    
    data <- object[["data"]][["data"]]
    x <- object[["data"]][["x"]]
    y <- object[["data"]][["y"]]
    
    j <- N_leaves <- nrow(data)
    FDH_scores <- matrix(nrow = j, ncol = 1)
    x_k <- atreeTk <- as.matrix(data[, x])
    y_k <- ytreeTk <- as.matrix(data[, y])
    nX <- length(x)
    nY <- length(y)
    
    if (names(scores) == "EAT_BCC_out"){
      scores_FDH <- EAT_BCC_out(j, FDH_scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
      
    } else if (names(scores) == "EAT_BCC_in"){
      scores_FDH <- EAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
      
    } else if (names(scores) == "EAT_DDF"){
      scores_FDH <- EAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
      
    } else if (names(scores) == "EAT_RSL_out"){
      scores_FDH <- EAT_RSL_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
      
    } else if (names(scores) == "EAT_RSL_in"){
      scores_FDH <- EAT_RSL_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
      
    } else if (names(scores) == "EAT_WAM"){
      scores_FDH <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    }
    
    scores_FDH <- as.data.frame(scores_FDH)
    
    names(scores_FDH) <- "scores_FDH"
  
    efficiency_density <- efficiency_density + 
      geom_density(data = scores_FDH, aes_string(x = "scores_FDH"), 
                   alpha = .3, 
                   fill = "#00AFBB", colour = "#00AFBB") 
  }
  
  return(efficiency_density)
}






