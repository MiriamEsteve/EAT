#' @title Random Forest Secuencial
#'
#' @description This function generates a set of tree models with different samples of the original data.
#'
#' @param m Number of trees.
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param k_forest Number of variables chosen to do Random Forest.
#'
#' @return List with m tree models.
#'
#' @import dplyr
#'
#' @export
random_forest_secuencial <- function(m, data, x, y, numStop = 5, k_forest = 2){
  
  try(if(k_forest > length(x)) 
      stop("K_forest must be less than the number of independent variables")
      )
  
  forest <- list()

  for(i in 1:m){
    
    df_bag <- data %>%
      sample_n(nrow(data), replace = T)

    forest <- append(forest, list(EAT_forest(df_bag, x, y, numStop, k_forest)))

  }

  return(forest)

}

#' @title Forest EAT
#'
#' @description This function generates a deep tree from each new subset from the original data.
#'
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param k_forest Number of variables chosen to do Random Forest.
#'
#' @return Deep tree from a certain subset of the original data.
#'
#' @import dplyr
#'
#' @export
EAT_forest <- function(data, x, y, numStop, k_forest){
  
  data <- data[, c(x, y)] %>% as.data.frame() %>%
    mutate(id = row_number())
  
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)

  N <- nrow(data)
  nX <- length(x)
  nY <- length(y)

  t <- list("id" = 1,
            "F" = -1,
            "SL" = -1,
            "SR" = -1,
            "index" = data[["id"]],
            "varInfo" = rep(list(list(Inf, Inf, Inf)), nX),
            "R" = -1,
            "xi" = -1,
            "s" = -1,
            "y" = -1,
            "a" = apply(data[, x, drop = F], 2, min) %>% unname(),
            "b" = rep(Inf, nX)
  )
  
  t[["y"]] <- lapply(data[ , y], function(x) max(x, na.rm = TRUE)) %>% unname()
  
  t[["R"]] <- mse(data, t, y)
  
  # Variables to do random fores
  
  tree <- list(t)
  leaves <- list(t)
  N_leaves <- length(leaves)
  
  while(N_leaves != 0){
    
    arrayK <- sample(x, k_forest, replace = F) %>% sort()
    
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL #Drop t selected
    
    if(isFinalNode(t[["index"]], data[, x], numStop)) break
    
    tree_leaves <- split_forest(data, tree, leaves, t, x, y, numStop, arrayK)
    
    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)
    

  }
  
  leaves <- NULL
  
  return(tree)

}

#' @title Split forest
#'
#' @description This function gets the variable and split value to be used in estimEAT, selects the best split and updates VarInfo, node indexes and leaves list.
#'
#' @param data Data to be used.
#' @param tree List structure with the tree nodes.
#' @param leaves List with leaf nodes or pending expansion nodes.
#' @param t Node which is being splitted.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param numStop Minimun number of observations on a node to be splitted.
#' @param arrayK Column input indexes to split.
#'
#' @return Leaves and tree lists updated with the new children nodes.

split_forest <- function(data, tree, leaves, t, x, y, numStop, arrayK){
  
  N <- nrow(data)
  N_tree <- length(tree)
  nX <- length(x)
  
  err_min <- Inf
  
  for(xi in arrayK){
    
    index <- data[, "id"] %in% t[["index"]]
    S <- data[index, xi] %>% unique() %>% sort()
    
    for(i in 2:length(S)){
      
      tL_tR_ <- estimEAT(data, leaves, t, xi, S[i], y)
      tL_ <- tL_tR_[[1]]
      tR_ <- tL_tR_[[2]]
      
      err <- tL_[["R"]] + tR_[["R"]]
      
      if((t[["varInfo"]][[xi]][[1]] + t[["varInfo"]][[xi]][[2]]) > err){
        
        t[["varInfo"]][[xi]] <- list(tL_[["R"]], tR_[["R"]], S[i])
        
      }
      
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
  
  tL[["F"]] <- tR[["F"]] <- t[["id"]]
  
  tree[[which(t[["id"]] == lapply(tree, function(x){x$id}))]] <- t
  
  # If they are end nodes, set VarInfo all to zero
  
  if(isFinalNode(tR[["index"]], data[, x], numStop)){
    
    tR[["varInfo"]] <- rep(list(list(0, 0, 0)), nX)
    tR[["xi"]] <- tR[["s"]] <- -1
    leaves <- append(leaves, list(tR), 0)
    
  } else {
    
    leaves <- append(leaves, list(tR))
  }
  
  if(isFinalNode(tL[["index"]], data[, x], numStop)){
    
    tL[["varInfo"]] <- rep(list(list(0, 0, 0)), nX)
    tL[["xi"]] <- tL[["s"]] <- -1
    leaves <- append(leaves, list(tL), 0)
    
  } else {
    
    leaves <- append(leaves, list(tL))
  }
  
  tree <- append(tree, list(tL))
  tree <- append(tree, list(tR))
  
  return(list(tree, leaves))
  
}

#' @title Random Forest Predictor
#'
#' @description This function predicts ...
#'
#' @param forest Random Forest EAT object.
#'
#' @return List with m tree models.
#'
#' @import dplyr
#'
#' @export
random_predictor <- function(forest, Xn){
  
  leny <- length(forest[[1]][[1]][["y"]])
  lenf <- length(forest)
  
  y_result <- matrix(0, ncol = leny, nrow = lenf) %>% as.data.frame()
  
  for(tree in 1:lenf){
    
    y_result[tree, ] <- predictor(forest[[tree]], Xn) %>% unlist()
    
  }
  
  for(j in 1:leny){
    
    y_result[, j] <- y_result[, j] %>% sort(decreasing = T)
    
  }
  
  return(y_result)
 
}

#' @title Random Test
#'
#' @description This function predicts the output of a independent variables data set.
#'
#' @param forest Random Forest EAT object.
#' @param df_test Data to be predicted.
#'
#' @return ...
#'
#' @import dplyr
#'
#' @export
random_test <- function(forest, df_test){
  
  leny <- length(forest[[1]][[1]][["y"]])
  lent <- nrow(df_test)
  
  y_predict <- matrix(0, ncol = leny, nrow = lent) %>% as.data.frame()
  
  for(xN in 1:lent){
    
    y_predict[xN, ] <- random_predictor(forest, df_test[xN, ])[1, ]
    
  }
  
  return(y_predict) # en data frame

}

#' @title Random Score EAT
#'
#' @description This function ...
#'
#' @param data Data to be used.
#' @param tree Tree of forest object
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param nameCol Name of coulumn
#'
#' @return ...
#'
#' @import dplyr
#'
#' @export

random_scoreEAT <- function(data, tree, x, y, nameCol){  
  
  for(i in 1:nrow(data)) {
    
    xn <- data[i, x]
    yn <- data[i, y]
    
    matrix[i, nameCol] <- predict_cplex_fi_EAT(xn, yn, tree)

    }
}

#' @title Forest Score EAT
#'
#' @description This function ...
#'
#' @param forest Random Forest EAT object.
#' @param data Data to be used.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param nameCol Name of coulumn
#'
#' @return ...
#'
#' @import dplyr
#'
#' @export

forest_scoreEAT <- function(forest, data, x, y, nameCol = "Score"){    
  
  data[nameCol] <- NA
  
  for(tree in 1:length(forest)){
    
    random_scoreEAT(data, forest[[tree]], x, y, nameCol)
    
  }
}

#' @title Histogram of Y
#'
#' @description This function ...
#'
#' @param data Data to be used with scores.
#' @param nameCol Name of coulumn
#'
#' @return ...
#'
#' @import ggplot2
#' @import gridExtra
#'
#' @export

random_histogram <- function(data, nameCol = "Score"){    
  
  ncol <- 5 # árboles
  nrow <- 3 # int(np.ceil(len(df.columns) / (1.0*ncol)))
  
  count <- 0
  
  while(T) {
    
    a <- plot( a,b , pch=20)
    b <- plot(a-b , pch=18)
    c <- hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="")
    d <- boxplot(a , col="grey" , xlab="a")
    e <- plot( a,b , pch=20)
    f <- plot(a-b , pch=18)
    g <- hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="")
    h <- boxplot(a , col="grey" , xlab="a")
    i <- plot( a,b , pch=20)
    j <- plot(a-b , pch=18)
    k <- hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="")
    l <- boxplot(a , col="grey" , xlab="a")
    
  }
  
}