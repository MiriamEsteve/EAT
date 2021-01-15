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
    mtry = s_mtry
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

#' @title Individual EAT for Random Forest
#'
#' @description This function builds a individual tree for Random Forest
#'
#' @param data Dataframe containing the training set.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param s_mtry  Select number of inputs. It could be: \code{"Breiman"}, \code{"DEA1"}, \code{"DEA2"}, \code{"DEA3"} or \code{"DEA4"} or any integer.
#' 
#' @return List of m trees in forest and the error that will be used in the ranking of the importance of the variables.
RandomEAT <- function(data, x, y, numStop, s_mtry){
  
  # Size data
  N <- nrow(data)
  
  # Size 'x' and 'y'
  nX <- length(x)
  nY <- length(y)
  
  #t node
  t <- list("id" = 1,
            "F" = -1,
            "SL" = -1,
            "SR" = -1,
            "index" = data[["id"]],
            "R" = -1,
            "xi" = -1,
            "s" = -1,
            "y" = -1,
            "a" = apply(data[, x, drop = F], 2, min) %>% 
              unname(),
            "b" = rep(Inf, nX)
  )
  
  t[["y"]] <- apply(data[, y, drop = F], 2, max) %>%
    unname() %>%
    as.list()
  
  t[["R"]] <- mse(data, t, y)
  
  # Tree
  tree <- list(t)
  
  # List of leaf nodes
  leaves <- list(t)
  N_leaves <- length(leaves)
  
  # Build tree
  while(N_leaves != 0){
    t <- leaves[[N_leaves]]
    leaves[[N_leaves]] <- NULL # Drop t selected
    if(isFinalNode(t[["index"]], data[, x], numStop)) break
    
    mtry <- select_mtry(s_mtry, t, nX, nY)
    # Randomly select k (<P) of the original predictors
    # Select random columns by index
    arrayK <- sort(sample(x, mtry))
    
    tree_leaves <- split_forest(data, tree, leaves, t, x, y, numStop, arrayK)
    
    tree <- tree_leaves[[1]]
    leaves <- tree_leaves[[2]]
    N_leaves <- length(leaves)
  }
  
  leaves <- NULL
  
  return(tree)
}

#' @title Random Forest EAT
#'
#' @description This function build \code{m} individual Efficiency Analysis Trees in a forest structure.
#' 
#' @param data Dataframe or matrix containing the variables in the model.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Integer. Minimun number of observations in a node for a split to be attempted.
#' @param m Integer. Number of trees to be build.
#' @param s_mtry Select number of inputs in each split. It can be an integer or any of the following options:
#' \itemize{
#' \item{\code{"Breiman"}}: \code{in / 3}
#' \item{\code{"DEA1"}}: \code{(obs / 2) - out}  
#' \item{\code{"DEA2"}}: \code{(obs / 3) - out}
#' \item{\code{"DEA3"}}: \code{obs - 2 * out}
#' \item{\code{"DEA4"}}: \code{min(obs / out, (obs / 3) - out)}
#' }
#' @param na.rm Logical. If \code{TRUE}, NA rows are omitted.
#'
#' @importFrom dplyr %>% row_number
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
#' Rf_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 5,
#'                   m = 100, s_mtry = "Breiman", na.rm = TRUE)
#' 
#' @return A RFEAT object.
RFEAT <- function(data, x, y, numStop = 5, m = 50, 
                  s_mtry = "Breiman", na.rm = TRUE){
  conflict_prefer("filter", "dplyr")
  
  data <- preProcess(data, x, y, na.rm = na.rm)
  
  rwn <- data[[1]]
  
  data <- data[[2]] %>%
    mutate(id = row_number())
  
  # Reorder index 'x' and 'y' in data
  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)
  
  # DMUs
  N <- nrow(data)
  nY <- length(y)
  
  # Forest error
  err <- 0
  
  # Forest list
  forest <- rep(list(list()), m)
  forestArray <- rep(list(list()), m)
  
  for(i in 1:m){
    df_train_arr_test <- bagging(data, x, y)
    df_train <- df_train_arr_test[[1]]
    arr_test <- df_train_arr_test[[2]]
    
    forestArray[[i]] <- arr_test
    
    # Train a tree model on this sample -> EAT
    tree <- RandomEAT(df_train, x, y, numStop, s_mtry)
    forest[[i]] <- tree
  }
  
  # TEST
  for(i in 1:N){
    reg_i <- data[i, ]
    
    y_EstimArr <- rep(list(0), nY)
    
    # Cardinal Ki
    Ki <- 1
    
    for(k in 1:m){ #k in Ki
      if(forestArray[[k]][[i]]){
        Ki <- Ki + 1
        y_EstimArr <- mapply("+", y_EstimArr,  predictor(forest[[k]], reg_i[x]))
      }
    }
    
    # y_EstimArr is the mean prediction for each output
    
    if(all(sapply(y_EstimArr, identical, 0)))
      next
    err <- err + sum((reg_i[y] - (y_EstimArr / Ki)) ^ 2)
  }
  
  RFEAT <- RFEAT_object(data, x, y, rwn, numStop, m, s_mtry, na.rm, forest, err / N)
  
  invisible(RFEAT)
}

#' @title Random Forest EAT predictions
#'
#' @description Efficiency score of Random Forest EAT
#'
#' @param forest Trees structures in forest list.
#' @param xn Row indexes in data.
#'
#' @return Number of rows, matrix of inputs, matrix of outputs, predictions, number of inputs, number of outputs and number of leaf nodes.
RF_predictor <- function(forest, xn){
  m <- length(forest)
  y_result <- rep(list(list()), m)
  
  for(tree in 1:m){
    y_result[[tree]] <- predictor(forest[[tree]], xn)
  }
  
  y_result <- as.data.frame(matrix(unlist(y_result), nrow = length(unlist(y_result[1]))))
  y_result <- apply(y_result, 1, "mean")
  
  return(y_result)
}

#' @title RFEAT Efficiency Scores
#'
#' @description This function calculates the efficiency scores for each DMU by an RFEAT model.
#'
#' @param data Dataframe for which the efficiency score is calculated.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param object A RFEAT object
#'
#' @importFrom dplyr %>% mutate
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
#' RFEAT_model <- RFEAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 5,
#'                      m = 50, s_mtry = "Breiman", na.rm = TRUE)
#'
#' efficiencyRFEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = RFEAT_model)
#'
#' @return Dataframe with input variables and efficiency scores by a RFEAT model.
efficiencyRFEAT <- function(data, x, y, object){
  
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
  
  N <- nrow(data)
  nY <- length(y)
  
  # Forest values return by RFEAT()
  forest <- object[["forest"]]
  
  data <- data %>% 
    mutate(scoreRF = rep(0, N))
  
  yRF <- rep(list(list()), nY)
  y_result <- as.data.frame(matrix(ncol = nY, nrow = N))
  
  for(xn in 1:N){
    yRF <- RF_predictor(forest, data[xn, ])
    
    if(typeof(yRF[[1]]) != "double")
      yRF <- RF[[1]]
    
    for(d in 1:nY){
      y_result[xn, d] <- round(yRF[[d]] / data[xn, y[[d]]], 6)
    }
    data$scoreRF[xn] <- min(y_result[xn, ])
  }
  
  scoreRF <- as.data.frame(data$scoreRF)
  names(scoreRF) <- "scoreRF"
  rownames(scoreRF) <- rwn
  
  print(scoreRF)
  
  invisible(data)
}

#' @title Model prediction for RFEAT
#'
#' @description This function predicts the expected output by an RFEAT object.
#'
#' @param object A RFEAT object.
#' @param newdata Dataframe. Set of input variables to predict on.
#'
#' @importFrom dplyr %>%
#'
#' @return Data frame with the original data and the predicted values.
#' 
#' @export
predictRFEAT <- function(object, newdata) {
  
  train_names <- object[["data"]][["input_names"]]
  test_names <- names(newdata)
  
  if (class(object) != "RFEAT"){
    stop(paste(object, "must be an RFEAT object"))
  }
  
  if (!is.data.frame(newdata)){
    stop("newdata must be a data.frame")
  } else if (length(train_names) != length(test_names)){
    stop("Training and prediction data must have the same number of variables")
  } else if (!all(train_names == test_names)){
    stop(paste("Variable name: ", test_names[1], "not found in taining data"))
  }
  
  y <- object[["data"]][["y"]] 
  forest <- object[["forest"]]
  m <- object[["control"]][["m"]]
  
  predictions <- data.frame()
  
  for(register in 1:nrow(newdata)){
    y_result <- rep(list(list()), m)
    
    for(tree in 1:m){
      y_result[[tree]] <- predictor(forest[[tree]], newdata[register, ])
    }
    
    y_result <- as.data.frame(matrix(unlist(y_result), nrow = length(unlist(y_result[1]))))
    y_result <- apply(y_result, 1, "mean")
    
    predictions <- rbind(predictions, y_result)
    
  }
  
  names(predictions) <-  paste(object[["data"]][["output_names"]], "_pred", sep = "")
  
  print(predictions)
  
  predictions <- cbind(newdata, predictions)
  
  invisible(predictions)
}

#' @title Ranking of variables by RFEAT
#'
#' @description This function calculates variable importance for an RFEAT object.
#'
#' @param object An RFEAT object.
#' @param r Integer. Decimal units.
#' @param barplot Logical. If \code{TRUE}, a barplot with importance scores is displayed.
#'
#' @return Dataframe with scores or list with scores and barplot.
#' 
#' @export   
rankingRFEAT <- function(object, r = 2, barplot = TRUE) {
  
  if (class(object) != "RFEAT"){
    stop(paste(object, "must be an RFEAT object"))
    
  } 
  
  if(length(object[["data"]][["x"]]) < 2){
    stop("More than two predictors are necessary")
  }
  
  scores <- imp_var_RFEAT(object = object, r = r)
  
  if (barplot == T){
    barplot <- barplot_importance(scores, threshold = NULL)
    return(list(scores, barplot))
    
  } else {
    return(scores)
  }
  
  # EAT_ranking.default <- function(x) "Hola"
  # RFEAT_ranking.default <- function(x) "Adios"
  # g <- function(x) {
  # UseMethod("RFEAT_ranking")
  # }
  
}

#' @title Importance variable of xj in Random Forest EAT
#'
#' @description Importance variable of xj in Random Forest EAT
#'
#' @param object An RFEAT object
#' @param r Integer. Decimal units.
#' 
#' @importFrom dplyr %>% arrange
#' @return List of importance of inputs xj
imp_var_RFEAT <- function(object, r = 2){
  
  err <- object[["error"]]
  data <- object[["data"]][["df"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  numStop <- object[["control"]][["numStop"]]
  m <- object[["control"]][["m"]]
  s_mtry <- object[["control"]][["s_mtry"]]
  
  imp <- data.frame()
  
  for(xi in x){
    # Barajar xi
    df_xi <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
    rf_err_xi <- RFEAT(df_xi, x, y, numStop, m, s_mtry)
    err_xi <- rf_err_xi[["error"]]
    imp <- rbind(imp, (100 * ((err_xi - err)/err)))
  }
  
  rownames(imp) <- object[["data"]][["input_names"]]
  colnames(imp) <- "Importance"
  
  imp <- imp %>%
    arrange(desc(Importance))
  
  return(imp)
}

#' @title Tuning an RFEAT model
#'
#' @description This funcion calculates the mean square error for a Random Forest of Efficiency Analysis Tree built with a set of given hyperparameters. 
#'
#' @param training Training dataframe or matrix containing the variables in the model for model construction.
#' @param test Test dataframe or matrix containing the variables in the model for model assessment.
#' @param x Vector. Column input indexes in data.
#' @param y Vector. Column output indexes in data.
#' @param numStop Vector. Set of minimun number of observations in a node for a split to be attempted.
#' @param m Vector. Set of number of trees to be build.
#' @param s_mtry. Vector. Set of options for selecting number of inputs to be selected in each split.
#' @param na.rm Logical. If \code{TRUE}, \code{NA} rows are omitted.
#' 
#' @export
#'
#' @return Dataframe in which each row corresponds to a given set of hyperparameters with its corresponding mean square error.
bestRFEAT <- function(training, test, x, y, numStop, m, s_mtry, na.rm = TRUE) {
  
  train_names <- names(training[, c(x, y)])
  test_names <- names(test[, c(x, y)])
  
  if (length(train_names) != length(test_names)){
    stop("Training and test data must have the same number of variables")
  } else if (!all(train_names == test_names)){
    stop(paste("Variable name: ", test_names[1], "not found in taining data"))
  }
  
  test <- preProcess(test, x, y, na.rm = na.rm)[[2]]
  
  hp <- expand.grid(numStop = numStop,
                    m = m,
                    s_mtry = s_mtry)
  
  hp$MSE <- NA
  
  s_mtry_opt <- c("Breiman", "DEA1", "DEA2", "DEA3", "DEA4")
  
  for (i in 1:nrow(hp)) {
    
    if (hp[i, "s_mtry"] %in% s_mtry_opt){
      input_select <- as.character(hp[i, "s_mtry"])
    
    } else {
      input_select <- as.numeric(hp[i, "s_mtry"])
      
    }
    
    RFEATmodel <- RFEAT(data = training, x = x, y = y, numStop = hp[i, "numStop"],
                        m = hp[i, "m"], s_mtry = input_select, na.rm = TRUE)
    
    x.t <- RFEATmodel[["data"]][["x"]]
    y.t <- RFEATmodel[["data"]][["y"]]
    
    # RMSE
    
    data.p <- as.data.frame(test[, x.t])
    pred <- data.frame()
    for (j in 1:nrow(data.p)){
      pred <- rbind(pred, RF_predictor(RFEATmodel[["forest"]], data.p[j, ]))
    }
    
    predictions <- cbind(data.p, pred)
    
    MSE <- sqrt(sum((test[, y.t] - predictions[, y.t]) ^ 2) / nrow(test))
    
    hp[i, "MSE"] <- round(MSE, 2)
    
  }
  
  print(hp)
  
}

print.RFEAT <- function(x, ...) {
  
  input_names <- x[["data"]][["input_names"]]
  output_names <- x[["data"]][["output_names"]]
  
  cat("\n",
      paste(" Formula: "),
      do.call(paste, c(as.list(output_names), sep = " + ")), 
      "~",
      do.call(paste, c(as.list(input_names), sep = " + ")),
      "\n"
  )
  
  cat(
    rep("\n", 1),
    "# ========================== #", "\n",
    "#           Forest           #", "\n",
    "# ========================== #", 
    rep("\n", 2) 
  )
  
  cat(" Total MSE: ", round(x[["MSE"]], 2), "\n",
      " numStop: ", x[["control"]][["numStop"]],  "\n",
      " No. of trees (m): ", x[["control"]][["m"]], "\n",
      " No. of inputs tried (s_mtry): ", x[["control"]][["s_mtry"]],
      sep = "")
  
}
