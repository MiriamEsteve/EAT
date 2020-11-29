#' @title Efficiecy Objects
#'
#' @description This function prepares the necessary objects to get the scores.
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom dplyr %>% filter select
#'
#' @return Number of rows, matrix for scoring, matrix of inputs, matrix of outputs, a Pareto-coordinates, predictions, number of inputs, number of outputs and number of leaf nodes.
eff_data <- function(data, tree, x, y) {
  
  SL <- a <- NULL

  j <- nrow(data)

  scores <- matrix(nrow = j, ncol = 1)

  x_k <- as.matrix(data[, x])
  y_k <- as.matrix(data[, y])

  tree <- data.frame(Reduce(rbind, tree))
  atreeTk <- tree %>% filter(SL == -1) %>% select(a)
  ytreeTk <- tree %>% filter(SL == -1) %>% select(y)

  atreeTk <- as.data.frame(do.call(cbind, atreeTk$a)) %>% t()
  ytreeTk <- as.data.frame(do.call(cbind, ytreeTk$y)) %>%
    unlist() %>%
    matrix(ncol = length(y), byrow = T)

  nX <- length(x)
  nY <- length(y)
  N_leaves <- nrow(atreeTk)

  return(list(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves))

}

#' @title BCC output-oriented
#'
#' @description BCC output-oriented
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_BCC_output <- function(data, tree, x, y) {
  
 efficiency_data <- eff_data(data, tree, x, y)
 
 j <- efficiency_data[[1]]
 scores <- efficiency_data[[2]]
 x_k <- efficiency_data[[3]]
 y_k <- efficiency_data[[4]]
 atreeTk <- efficiency_data[[5]]
 ytreeTk <- efficiency_data[[6]]
 nX <- efficiency_data[[7]]
 nY <- efficiency_data[[8]]
 N_leaves <- efficiency_data[[9]]

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

#' @title BCC input-oriented
#'
#' @description BCC input-oriented
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_BCC_input <- function(data, tree, x, y) {

  efficiency_data <- eff_data(data, tree, x, y)

  j <- efficiency_data[[1]]
  scores <- efficiency_data[[2]]
  x_k <- efficiency_data[[3]]
  y_k <- efficiency_data[[4]]
  atreeTk <- efficiency_data[[5]]
  ytreeTk <- efficiency_data[[6]]
  nX <- efficiency_data[[7]]
  nY <- efficiency_data[[8]]
  N_leaves <- efficiency_data[[9]]

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

#' @title EAT Directional Distance
#'
#' @description EAT Directional Distance
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_DD <- function(data, tree, x, y) {

  efficiency_data <- eff_data(data, tree, x, y)

  j <- efficiency_data[[1]]
  scores <- efficiency_data[[2]]
  x_k <- efficiency_data[[3]]
  y_k <- efficiency_data[[4]]
  atreeTk <- efficiency_data[[5]]
  ytreeTk <- efficiency_data[[6]]
  nX <- efficiency_data[[7]]
  nY <- efficiency_data[[8]]
  N_leaves <- efficiency_data[[9]]

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

#' @title Input oriented Rusell model
#'
#' @description Input oriented Rusell model
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_Rusell_input <- function(data, tree, x, y) {

  efficiency_data <- eff_data(data, tree, x, y)

  j <- efficiency_data[[1]]
  scores <- efficiency_data[[2]]
  x_k <- efficiency_data[[3]]
  y_k <- efficiency_data[[4]]
  atreeTk <- efficiency_data[[5]]
  ytreeTk <- efficiency_data[[6]]
  nX <- efficiency_data[[7]]
  nY <- efficiency_data[[8]]
  N_leaves <- efficiency_data[[9]]

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

#' @title Output oriented Rusell model
#'
#' @description Output oriented Rusell model
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_Rusell_output <- function(data, tree, x, y) {

  efficiency_data <- eff_data(data, tree, x, y)

  j <- efficiency_data[[1]]
  scores <- efficiency_data[[2]]
  x_k <- efficiency_data[[3]]
  y_k <- efficiency_data[[4]]
  atreeTk <- efficiency_data[[5]]
  ytreeTk <- efficiency_data[[6]]
  nX <- efficiency_data[[7]]
  nY <- efficiency_data[[8]]
  N_leaves <- efficiency_data[[9]]

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

#' @title Weighted Additive model
#'
#' @description Weighted Additive model
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with scores.
EAT_WA <- function(data, tree, x, y) {

  efficiency_data <- eff_data(data, tree, x, y)

  j <- efficiency_data[[1]]
  scores <- efficiency_data[[2]]
  x_k <- efficiency_data[[3]]
  y_k <- efficiency_data[[4]]
  atreeTk <- efficiency_data[[5]]
  ytreeTk <- efficiency_data[[6]]
  nX <- efficiency_data[[7]]
  nY <- efficiency_data[[8]]
  N_leaves <- efficiency_data[[9]]

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
      vec[(nX + nY + 1):(nY + nX + N_leaves)] <- ytreeTk[, xi]

      add.constraint(lps, xt = vec, ">=", rhs = y_k[d, xi])
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
#' @description This function returns a numeric vector with the efficiency score for each DMU, an histogram and a brief descriptive analysis.
#'
#' @param data Data to be used.
#' @param tree Tree structure.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param scores_model Model to get scores. "EAT_BCC_output" and "EAT_BCC_input" for BCC model with output and input orientation respectively; "EAT_DD" for directional distance model; "EAT_Rusell_output" and "EAT_Rusell_input" for Rusell models with output and input orientation respectively and "EAT_WA" for Weighted Additive models.
#' @param color Logical. If True, observations with same efficient level output are painted the same level.
#'
#' @importFrom ggplot2 ggplot aes geom_col theme element_text scale_fill_brewer scale_alpha_discrete
#' @importFrom dplyr summarise %>% mutate left_join group_by
#' @importFrom stats median quantile sd
#'
#' @export
#'
#' @return Efficiency scores
efficiency_scores <- function(data, tree, x, y, scores_model, color = T) {
  
  if (!scores_model %in% c("EAT_BCC_output", "EAT_BCC_input", "EAT_DD", 
                           "EAT_Rusell_input", "EAT_Rusell_output", 
                           "EAT_WA")){
    stop("You should choose an available model. Please check help(efficiency_scores)")
  }
  
  if (scores_model == "EAT_BCC_output"){
    scores <- EAT_BCC_output(data, tree, x, y)

  } else if (scores_model == "EAT_BCC_input"){
    scores <- EAT_BCC_input(data, tree, x, y)

  } else if (scores_model == "EAT_DD"){
    scores <- EAT_DD(data, tree, x, y)

  } else if (scores_model == "EAT_Rusell_input"){
    scores <- EAT_Rusell_input(data, tree, x, y)

  } else if (scores_model == "EAT_Rusell_output"){
    scores <- EAT_Rusell_output(data, tree, x, y)

  } else if (scores_model == "EAT_WA"){
    scores <- EAT_WA(data, tree, x, y)
  }

  V1 <- NULL

  scores <- as.data.frame(scores)
  rownames(scores) <- rownames(data)

  descriptive <- scores %>%
    summarise("Mean" = round(mean(scores$V1), 2),
              "Std. Dev." = round(sd(scores$V1), 2),
              "Min" = round(min(scores$V1), 2),
              "Q1" = round(quantile(scores$V1)[[2]], 2),
              "Median" = round(median(scores$V1), 2),
              "Q3" = round(quantile(scores$V1)[[3]], 2),
              "Max" = round(max(scores$V1), 2)
              )
  
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
      theme(axis.text.x = element_text(angle = 45)) +
      xlab("DMU") +
      ylab("Score") +
      scale_fill_brewer(palette = "Set1") + 
      scale_alpha_discrete(range = c(0.4, 0.9)) +
      theme(legend.position = "bottom")
  } else {
    
    efficiency_histogram <- ggplot(scores,
                                   aes(x = reorder(row.names(scores), - V1), y = V1)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45)) +
      xlab("DMU") +
      ylab("Score")
  }

  return(list(scores, descriptive, efficiency_histogram))

}


