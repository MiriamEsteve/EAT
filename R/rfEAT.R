#' @title Random Forest EAT
#'
#' @description This function build "m" EAT trees in forest
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in 
#' @param numStop Ending rule
#' @param s_mtry  Select number of inputs. It could be: "Breiman", "DEA1", "DEA2", "DEA3" or "DEA4"
#'
#' @importFrom dplyr %>% mutate row_number
#'
#' @export
#' 
#' @return List of m trees in forest and the error that will be used in the ranking of the importance of the variables.
utils::globalVariables(c("x", "y"))
bagging <- function(data){
  col_name <- names(data[1,c(x,y)])
  df_train <- data.frame(matrix(ncol=length(col_name), nrow=0))
  colnames(df_train) <- col_name
  df_train <- df_train %>% mutate(id = row_number())
  
  N <- nrow(data)
  array <- rep(list(1), N)
  
  for(i in 1:N){
    chosen_idx <- sort(sample(data$id, 1))
    df_train[i,] <- data[chosen_idx,]
    array[chosen_idx] <- 0
  }
  df_train <- df_train %>% mutate(id = row_number())
  
  
  return(list(df_train, array))
}


#' @title Select m_try
#'
#' @description This function is a middle function to build "m" EAT trees in forest
#'
#' @param s_mtry  Select number of inputs. It could be: "Breiman", "DEA1", "DEA2", "DEA3" or "DEA4"
#' @param t Node
#' @param nX Number of columns input indexes in data.
#' @param nY Number of columns output indexes in 
#' 
#' 
#' @return Number of inputs selected according with the rule specified.
select_mtry <- function(s_mtry, t, nX, nY){
  nt = length(t["index"])
  mtry = 0
  
  if(s_mtry == "Breiman")
    mtry = nX / 3
  
  else if(s_mtry == "DEA1")
    mtry = (nt / 2) - nY
  
  else if(s_mtry == "DEA2")
    mtry = (nt / 3) - nY
  
  else if(s_mtry == "DEA3")
    mtry = nt - (2 * nY)
  
  else if(s_mtry == "DEA4")
    mtry = min(nt / nY, (nt / 3) - nY)
  
  else
    mtry = s_mtry
  
  if(mtry < 1)
    return(1)
  if(mtry > nX)
    return(nX)
  
  return(as.integer(round(mtry)))
}

#' @title Split node in Random Forest EAT
#'
#' @description This function gets the variable and split value to be used in estimEAT, selects the best split, node indexes and leaves list.
#'
#' @param data Data to be used.
#' @param tree List structure with the tree nodes.
#' @param leaves List with leaf nodes or pending expansion nodes.
#' @param t Node which is being splitted.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param arrayK Column input indexes in data selected by s_mtry
#'
#' @importFrom dplyr %>%
#'
#' @return Leaves and tree lists updated with the new children nodes.
split_forest <- function(data, tree, leaves, t, y, numStop, arrayK){
  N <- nrow(data)
  nX <- length(x)
  N_tree <- length(tree)
  
  err_min <- Inf
  
  for(xi in arrayK){
    index <- data$id %in% t[["index"]]
    S <- data[index, xi] %>% unique() %>% sort()
    
    for(i in 2:length(S)){
      tL_tR_ <- estimEAT(data, leaves, t, xi, S[i], y)
      tL_ <- tL_tR_[[1]]
      tR_ <- tL_tR_[[2]]
      
      err <- tL_[["R"]] + tR_[["R"]]
      
      if((t[["varInfo"]][[xi]][[1]] + t[["varInfo"]][[xi]][[2]]) > err)
        t[["varInfo"]][[xi]] <- list(tL_[["R"]], tR_[["R"]], S[i])
      
      if (err < err_min){
        t[["xi"]] <- xi
        t[["s"]] <- S[i]
        err_min <- err
        tL <- tL_
        tR <- tR_
      }
      
    }
  }
  
  S <- NULL
  
  t$SL <- tL$id <- N_tree + 1
  t$SR <- tR$id <- N_tree + 2
  # Establish tree branches (father <--> sons)
  tL$F <- tR$F <- t$id
  
  tree[[which(t[["id"]] == lapply(tree, function(x){x$id}))]] <- t
  
  # If they are end nodes, set VarInfo all to zero
  if(isFinalNode(tR[["index"]], data, numStop)){
    tR[["varInfo"]] <- rep(list(list(0, 0, 0)), nX)
    tR[["xi"]] <- tR[["s"]] <- -1
    leaves <- append(leaves, list(tR), 0)
  } else{
    leaves <- append(leaves, list(tR))
  }
  
  if(isFinalNode(tL[["index"]], data, numStop)){
    tL[["varInfo"]] <- rep(list(list(0, 0, 0)), nX)
    tL[["xi"]] <- tL[["s"]] <- -1
    leaves <- append(leaves, list(tL), 0)
  } else
    leaves <- append(leaves, list(tL))
  
  tree <- append(tree, list(tL))
  tree <- append(tree, list(tR))
  
  return(list(tree, leaves))
}

#' @title Random Forest EAT 2
#'
#' @description This function is a middle function to build "m" EAT trees in forest
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in 
#' @param numStop Ending rule
#' @param s_mtry  Select number of inputs. It could be: "Breiman", "DEA1", "DEA2", "DEA3" or "DEA4"
#' 
#' @return List of m trees in forest and the error that will be used in the ranking of the importance of the variables.
RandomEAT <- function(data, x, y, numStop, s_mtry){
  
  #Size data
  N <- nrow(data)
  #Size 'x' and 'y'
  nX <- length(x)
  nY <- length(y)
  
  #t node
  t <- list("id" = 1,
            "F" = -1,
            "SL" = -1,
            "SR" = -1,
            "index" = data[["id"]],
            "varInfo" = rep(list(c(Inf, Inf, Inf)), nX),
            "R" = -1,
            "xi" = -1,
            "s" = -1,
            "y" = -1,
            "a" = apply(data[, x], 2, min) %>% unname(),
            "b" = rep(Inf, nX)
  )
  
  t[["y"]] <- lapply(data[ , y], function(x) max(x, na.rm = TRUE)) %>% unname()
  
  t[["R"]] <- mse(data, t, y)
  
  #Tree
  tree <- list(t)
  
  #List of leaf nodes
  leaves <- list(t)
  N_leaves <- length(leaves)
  
  
  #Build tree
  while(N_leaves != 0){
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL #Drop t selected
    if(isFinalNode(t[["index"]], data, numStop)) break
    
    mtry <- select_mtry(s_mtry, t, nX, nY)
    # Randomly select k (<P) of the original predictors
    # Select random columns by index
    arrayK <- sort(sample(x, mtry))
    
    tree_leaves <- split_forest(data, tree, leaves, t, y, numStop, arrayK)
    
    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)
  }
  
  leaves <- NULL
  
  return(tree)
}

#' @title Random Forest EAT
#'
#' @description This function build "m" EAT trees in forest
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in 
#' @param numStop Ending rule
#' @param s_mtry  Select number of inputs. It could be: "Breiman", "DEA1", "DEA2", "DEA3" or "DEA4"
#'
#' @importFrom dplyr %>% row_number
#'
#' @export
#' 
#' @return List of m trees in forest and the error that will be used in the ranking of the importance of the variables.
RFEAT <- function(m, data, x, y, numStop = 5, s_mtry = "Breiman"){
  #Prepare data
  conflict_prefer("filter", "dplyr")
  
  data <- data[, c(x, y)] %>% as.data.frame()
  data <- data %>% mutate(id = row_number())
  #Reorder index 'x' and 'y' in data
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)
  
  #DMUs
  N <- nrow(data)
  nY <- length(y)
  
  #Forest error
  err <- 0
  
  #Forest list
  forest <- rep(list(list()),m)
  forestArray <- rep(list(list()),m)
  
  for(i in 1:m){
    df_train_arr_test <- bagging(data)
    df_train <- df_train_arr_test[[1]]
    arr_test <- df_train_arr_test[[2]]
    
    forestArray[[i]] <- arr_test
    
    #Train a tree model on this sample -> EAT
    tree <- RandomEAT(data, x, y, numStop, s_mtry)
    forest[[i]] <- tree
  }
  
  #TEST
  for(i in 1:N){
    reg_i <- data[i,]
    
    y_EstimArr <- rep(list(0), nY)
    
    # Cardinal Ki
    Ki <- 1
    
    for(k in 1:m){ #k in Ki
      if(forestArray[k][[1]][[i]]){
        Ki <- Ki + 1
        y_EstimArr <- mapply("+", y_EstimArr,  predictor(forest[[k]], reg_i[x]))
      }
    }
    
    if(all(sapply(y_EstimArr, identical, 0)))
      next
    err <- err + sum((reg_i[y] - (y_EstimArr/Ki))^2)
  }
  
  return(list(forest, err/N))
}

#' @title Random Forest EAT predictions
#'
#' @description Efficiency score of Random Forest EAT
#'
#' @param forest Trees structures in forest list.
#' @param xn Row indexes in data.
#' 
#' @export
#'
#' @return Number of rows, matrix of inputs, matrix of outputs, predictions, number of inputs, number of outputs and number of leaf nodes.
RF_predictor <- function(forest, xn){
  m <- length(forest)
  y_result <- rep(list(list()), m)
  
  for(tree in 1:m){
    y_result[[tree]] <- predictor(forest[[tree]], xn)
  }
  
  y_result <- as.data.frame(matrix(unlist(y_result), nrow=length(unlist(y_result[1]))))
  y_result <- apply(y_result, 1, "mean")
  
  return(y_result)
}

#' @title Efficiency Objects of Random Forest EAT
#'
#' @description Efficiency score of Random Forest EAT
#'
#' @param data Data to be used.
#' @param forest_err List was returned by RFEAT function
#' @param y Column output indexes in data.
#'
#' @importFrom dplyr %>% mutate
#' 
#' @export
#'
#' @return Number of rows, matrix for scoring, matrix of inputs, matrix of outputs, a Pareto-coordinates, predictions, number of inputs, number of outputs and number of leaf nodes.
scoreRF <- function(data, forest_err, y){
  N <- nrow(data)
  nY <- length(y)
  
  #Forest values return by RFEAT()
  forest <- forest_err[[1]]
  
  data <- data %>% mutate(scoreRF = rep(0, N))
  
  yRF <- rep(list(list()), nY)
  y_result <- as.data.frame(matrix(ncol=nY, nrow=N))
  
  for(xn in 1:N){
    yRF <- RF_predictor(forest, data[xn, ])
    
    if(typeof(yRF[[1]]) != "double")
      yRF <- RF[[1]]
    
    for(d in 1:nY){
      y_result[xn, d] <- round(yRF[[d]]/data[xn, y[[d]]], 6)
    }
    data$scoreRF[xn] <- min(y_result[xn,])
  }
  
  return(data)
}