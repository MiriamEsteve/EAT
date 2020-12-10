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
EAT_RUS_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
EAT_RUS_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
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
#' @param object An EAT object.
#' @param scores_model Mathematic programming model to calculate scores. 
#' \itemize{
#' \item{\code{EAT_BCC_out}} BBC model. Output orientation.
#' \item{\code{EAT_BCC_in}}  BBC model. Input orientation.
#' \item{\code{EAT_DDF}}     Directional distance model.
#' \item{\code{EAT_RUS_out}} Rusell model. Output orientation
#' \item{\code{EAT_RUS_in}}  Rusell model. Input orientation.
#' \item{\code{EAT_WAM}}     Weighted Additive model.
#' }
#' @param r Integer. Decimal units for scores.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#'
#' @export
#'
#' @return Efficiency scores
efficiency_scores <- function(object, scores_model, r = 4) {
  
  if (!scores_model %in% c("EAT_BCC_out", "EAT_BCC_in", "EAT_DDF", 
                           "EAT_RUS_out", "EAT_RUS_in", "EAT_WAM")){
    stop("You should choose an available model. Please check help(efficiency_scores)")
  }
  
  data <- object[["data"]][["data"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  
  j <- nrow(data)
  scores <- matrix(nrow = j, ncol = 1)
  x_k <- as.matrix(data[, x])
  y_k <- as.matrix(data[, y])
  atreeTk <- object[["model"]][["a"]]
  ytreeTk <- object[["model"]][["y"]]
  nX <- length(x)
  nY <- length(y)
  N_leaves <- object[["model"]][["leaf_nodes"]]
  
  if (scores_model == "EAT_BCC_out"){
    scores <- EAT_BCC_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_BCC_in"){
    scores <- EAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_DDF"){
    scores <- EAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_RUS_out"){
    scores <- EAT_RUS_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_RUS_in"){
    scores <- EAT_RUS_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)

  } else if (scores_model == "EAT_WAM"){
    scores <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
  }

  V1 <- NULL

  scores <- as.data.frame(scores)
  names(scores) <- "Score"
  rownames(scores) <- object[["data"]][["row_names"]]

  descriptive <- scores %>%
    summarise("Mean" = round(mean(scores$Score), 2),
              "Std. Dev." = round(sd(scores$Score), 2),
              "Min" = round(min(scores$Score), 2),
              "Q1" = round(quantile(scores$Score)[[2]], 2),
              "Median" = round(median(scores$Score), 2),
              "Q3" = round(quantile(scores$Score)[[3]], 2),
              "Max" = round(max(scores$Score), 2)
              )
  
  print(kable(descriptive), "pipe")

  return(round(scores, r))

}











































#' @title Efficiency Bar Plot
#'
#' @description This function returns a bar plot with scores
#'
#' @param scores Score object
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param color Logical. If True, observations with same efficient level output are painted the same level.
#'
#' @importFrom ggplot2 ggplot aes geom_col theme element_text scale_fill_brewer scale_alpha_discrete
#' @importFrom dplyr  %>% mutate left_join group_by
#' @importFrom stats median quantile sd
#'
#' @export
#'
#' @return Efficiency scores
efficiency_barplot <- function(scores, data, tree, x, y, color = T) {
  
  if(color == T){
    
    predictions <- eat::predict(tree, data, x, y)
    
    levels <- unique(predictions)
    
    levels <- levels %>%
      mutate(Group = as.factor(1:nrow(levels)))
    
    scores <- cbind(scores, predictions) %>%
      left_join(levels) %>% 
      group_by(Group) %>%
      mutate(Efficient = as.factor(ifelse(V1 == max(V1), 1, 0)))
    
    efficiency_histogram <- ggplot(scores,
                                   aes(x = reorder(row.names(scores), - V1), y = V1,
                                       fill = Group, alpha = Efficient)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45), legend.position = "bottom") +
      xlab("DMU") +
      ylab("Score") +
      scale_fill_brewer(palette = "Set1") + 
      scale_alpha_discrete(range = c(0.4, 0.9))
  } else {
    
    efficiency_histogram <- ggplot(scores,
                                   aes(x = reorder(row.names(scores), - V1), y = V1)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45)) +
      xlab("DMU") +
      ylab("Score")
  }
  
  return(efficiency_histogram)
  
}

#' @title Efficiency Density and Barplot
#'
#' @description This function returns a density plot
#'
#' @param scores Score object
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram  geom_density
#' @importFrom dplyr  %>% mutate left_join group_by
#' @importFrom stats median quantile sd
#'
#' @export
#'
#' @return Efficiency scores
efficiency_density_barplot <- function(scores, data, tree, x, y) {
  
  efficiency_density <- ggplot(scores, aes(V1)) +
      geom_histogram(aes(y = ..density..), colour = "black", fill = "grey")+
      geom_density(alpha = .3, fill = "#FF583A", colour = "#FF583A") +
      xlab("Score") +
      ylab("Frequency")

  return(efficiency_density_barplot)
}

#' @title Efficiency Density FDH and EAT
#'
#' @description This function returns a density plot
#'
#' @param scores Score object
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram  geom_density
#' @importFrom dplyr  %>% mutate left_join group_by
#' @importFrom stats median quantile sd
#'
#' @export
#'
#' @return Efficiency scores
efficiency_density <- function(scores, data, tree, x, y) {
  
  efficiency_density <- ggplot(scores, aes(V1)) +
    geom_density(alpha = .3, fill = "#FF583A", colour = "#FF583A") +
    xlab("Score") +
    ylab("Frequency")
  
  return(efficiency_density)
}

#' @title Free Disposal Hull predictor
#'
#' @description ...
#'
#' @param data Data frame or matrix.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' 
#' @export
#'
#' @value ...
yFDH <- function(data, x, y) {
  
 data <- preProcess(data, x, y)
 
 # Size data
 N <- nrow(data)
 
 # Reorder index 'x' and 'y' in data
 x <- 1:((ncol(data) - 1) - length(y))
 y <- (length(x) + 1):(ncol(data) - 1)
 
 data <- data %>% select(-id)
  
 data[, "yFDH"] <- 0
 
 for (i in 1:nrow(data)){
   data[i, "yFDH"] <- FDH(data, data[i, x], y)
 }
 
 return(data)
 
}

#' @title Free Disposal Hull
#'
#' @description ...
#'
#' @param data Data frame or matrix.
#' @param xarray Input columns
#' @param yarray Output columns
#' 
#' @export
#'
#' @value ...
FDH <- function(data, xarray, y){
  yMax <- -Inf
  
  for (n in 1:nrow(data)){
    newMax <- TRUE
    for (j in 1:length(xarray)){
      if (data[n, j] > xarray[j]){
        newMax <- FALSE
        break
      }
    }
    if (newMax && yMax < data[n, y]) yMax <- data[n, y]
  }
  
  return(yMax)
}




