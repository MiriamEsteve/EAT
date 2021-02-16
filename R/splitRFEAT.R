#' @title Bagging data
#'
#' @description Bootstrap agggregating for data.
#'
#' @param data Dataframe containing the variables in the model.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' 
#' @importFrom dplyr %>% row_number
#' 
#' @return List containing training dataframe and list with binary respose as 0 if the observations has been selected for training and 0 in other case.
bagging <- function(data, x, y){
  df_train <- data.frame(matrix(ncol = length(c(x, y)), nrow = 0))
  colnames(df_train) <- names(data[, c(x, y)])
  df_train <- df_train %>%
    mutate(id = row_number())
  
  N <- nrow(data)
  array <- rep(list(1), N)
  
  for(i in 1:N){
    chosen_idx <- sort(sample(data[, "id"], 1))
    df_train[i, ] <- data[chosen_idx, ]
    array[chosen_idx] <- 0
  }
  
  df_train <- df_train %>% 
    mutate(id = row_number())
  
  return(list(df_train, array))
}


#' @title Select possible inputs in split.
#'
#' @description This function selects the number of inputs for a split in Random Forest.
#'
#' @param s_mtry  Select number of inputs. It could be: \code{"Breiman"}, \code{"DEA1"}, \code{"DEA2"}, \code{"DEA3"} or \code{"DEA4"} or any integer.
#' @param t Node which is being splitted.
#' @param nX Number of inputs in data.
#' @param nY Number of outputa in data.
#' 
#' @return Number of inputs selected according with the rule specified.
select_mtry <- function(s_mtry, t, nX, nY){
  nt = length(t["index"])
  mtry = 0
  
  if(s_mtry == "BRM") {
    mtry = nX / 3
    
  }else if(s_mtry == "DEA1") {
    mtry = (nt / 2) - nY
    
  }else if(s_mtry == "DEA2") {
    mtry = (nt / 3) - nY
    
  }else if(s_mtry == "DEA3") {
    mtry = nt - (2 * nY)
    
  }else if(s_mtry == "DEA4") {
    mtry = min(nt / nY, (nt / 3) - nY)
    
  }else {
    stop(paste(s_mtry, "is not available. Plase, cheack help(\"RFEAT\")"))
  }
  
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
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param arrayK Column input indexes in data selected by s_mtry.
#'
#' @importFrom dplyr %>%
#'
#' @return Leaves and tree lists updated with the new children nodes.
split_forest <- function(data, tree, leaves, t, x, y, numStop, arrayK){
  N <- nrow(data)
  nX <- length(x)
  N_tree <- length(tree)
  
  err_min <- Inf
  
  for(xi in arrayK){
    index <- data[, "id"] %in% t[["index"]]
    S <- data[index, xi] %>% 
      unique() %>% 
      sort()
    
    if (length(S) == 1) next
    
    for(i in 2:length(S)){
      tL_tR_ <- estimEAT(data, leaves, t, xi, S[i], y)
      tL_ <- tL_tR_[[1]]
      tR_ <- tL_tR_[[2]]
      
      err <- tL_[["R"]] + tR_[["R"]]
      
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
  
  t[["SL"]] <- tL[["id"]] <- N_tree + 1
  t[["SR"]] <- tR[["id"]] <- N_tree + 2
  # Establish tree branches (father <--> sons)
  tL[["F"]] <- tR[["F"]] <- t$id
  
  tree[[which(t[["id"]] == lapply(tree, function(x) {
    x$id
  }))]] <- t
  
  
  # If they are end nodes
  if(isFinalNode(tR[["index"]], data[, x], numStop)){
    tR[["xi"]] <- tR[["s"]] <- -1
    leaves <- append(leaves, list(tR), 0)
  } else{
    leaves <- append(leaves, list(tR))
  }
  
  if(isFinalNode(tL[["index"]], data[, x], numStop)){
    tL[["xi"]] <- tL[["s"]] <- -1
    leaves <- append(leaves, list(tL), 0)
  } else
    leaves <- append(leaves, list(tL))
  
  tree <- append(tree, list(tL))
  tree <- append(tree, list(tR))
  
  return(list(tree, leaves))
}