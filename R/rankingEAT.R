#' @title Breiman's Variable Importance
#'
#' @description This function recalculates all the possible splits, with the exception of the one being used, and for each node and variable gets the best split based on their degree of importance.
#'
#' @param data Data from EAT object.
#' @param tree Tree from EAT object.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param digits Decimal units.
#'
#' @importFrom dplyr %>% filter
#'
#' @return A dataframe with the best split for each node and its variable importance.
imp_var_EAT <- function(data, tree, x, y, digits) {
  index <- tree[[1]][["index"]]

  result <- list()

  empty <- list()

  nX <- length(x)

  for (t in 1:length(tree)) {
    if (tree[[t]][["SL"]] == -1) next

    for (xi in 1:nX) {
      P <- list()

      array <- data[index, xi] %>%
        unique() %>%
        sort()

      for (i in 2:length(array)) {
        if (tree[[t]][["s"]] == array[i]) next
        
        errLp_errRp_tLp_tRp <- estimEAT(
          data, tree, tree[[t]], xi,
          array[i], y
        )

        tL_p <- errLp_errRp_tLp_tRp[[1]]
        tR_p <- errLp_errRp_tLp_tRp[[2]]
        errL_p <- errLp_errRp_tLp_tRp[[1]][["R"]]
        errR_p <- errLp_errRp_tLp_tRp[[2]][["R"]]
        
        if (all(tL_p[["y"]] == Inf) || all(tR_p[["y"]] == Inf)) next

        tL_i <- tree[[tree[[t]][["SL"]]]][["index"]]

        tL_p_i <- tL_p[["index"]]

        tR_i <- tree[[tree[[t]][["SR"]]]][["index"]]

        tR_p_i <- tR_p[["index"]]

        imp <- round((sum(tL_p_i %in% tL_i) + sum(tR_p_i %in% tR_i)) / length(tL_i), digits)

        P <- append(P, list(list(
          "id" = tree[[t]][["id"]],
          "xi" = xi,
          "s" = tree[[t]][["s"]],
          "array" = array[i],
          "errL" = errL_p,
          "errR" = errR_p,
          "imp" = imp
        )))
      }

      if (length(P) == 0) {
        empty <- append(empty, tree[[t]][["id"]])

        next
      }

      max_value <- lapply(P, function(x) {
        x$imp
      }) %>%
        unlist() %>%
        max()

      if (all(lapply(P, function(x) {
        x$imp
      }) == max_value) == TRUE) {
        result <- append(result, list(P[[2]]))
      } else {
        result <- append(result, list(P[[1]]))
      }
    }
  }

  resultado <- matrix(unlist(result), nrow = length(result), byrow = T) %>%
    data.frame() %>%
    rename(
      "id" = X1,
      "xi" = X2,
      "s" = X3,
      "sm" = X4,
      "R(tL_p)" = X5,
      "R(tR_p)" = X6,
      "P(s,sm)" = X7
    )

  resultado <- resultado %>%
    filter(!(id %in% unlist(empty)))

  return(resultado)
}

#' @title Breiman Importance
#'
#' @description This function evaluates the importance of each predictor by the notion of surrogate splits.
#'
#' @param object An EAT object.
#' @param digits Decimal units.
#'
#' @importFrom dplyr %>%  mutate row_number filter rename arrange
#'
#' @return Dataframe with one column and the importance of each variable in rows.
M_Breiman <- function(object, digits) {
  
  data <- object[["data"]][["df"]] %>%
    mutate(id = row_number())
  tree <- object[["tree"]]
  x <- object[["data"]][["x"]] 
  y <- object[["data"]][["y"]]
  nX <- length(x)
  
  resultado <- imp_var_EAT(data, tree, x, y, digits)
  
  M <- as.list(rep(0, nX))
  
  for (var in 1:nX) {
    for (t in 1:length(tree)) {
      if (tree[[t]][["SL"]] == -1) next
      
      errors <- resultado %>% filter(
        xi == var,
        id == tree[[t]][["id"]]
      )
      
      M[[var]] <- M[[var]] + (tree[[t]][["R"]] - errors[1, "R(tL_p)"] - errors[1, "R(tR_p)"])
    }
  }
  
  M <- rbind(unlist(M)) %>% data.frame()
  
  names(M) <- names(data[, x])
  
  maxM <- max(M)
  
  if (maxM != 0) {
    m <- round(((100 * M) / maxM), digits)
    
    M <- rbind(M, m)
  } else {
    print("Divide by zero")
  }
  
  m <- t(m) %>%
    as.data.frame() %>%
    rename("Importance" = "V1") %>%
    arrange(desc(Importance))
  
  return(m)
}

#' @title Ranking of Variables by Efficiency Analysis Trees model.
#'
#' @description This function computes the variable importance through an Efficiency Analysis Trees model.
#'
#' @param object An \code{EAT} object.
#' @param barplot \code{logical}. If \code{TRUE}, a barplot with the importance scores is displayed.
#' @param threshold Importance score value in which a line is graphed.
#' @param digits Decimal units.
#'
#' @return \code{data.frame} with the importance scores and a barplot representing the the variable importance if \code{barplot = TRUE}.
#' 
#' @examples
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' rankingEAT(object = EAT_model,
#'            barplot = TRUE,
#'            threshold = 70,
#'            digits = 2)
#' }
#' 
#' @export   
rankingEAT <- function(object, barplot = TRUE, threshold = 70, digits = 2) {
  
  if (!is(object, "EAT")) {
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
  
  } else if (threshold < 0) {
    stop(paste('threshold =', threshold, 'must be greater than 0.'))
    
  } else if (digits < 0) {
    stop(paste('digits =', digits, 'must be greater than 0.'))
  
  } else if(length(object[["data"]][["x"]]) < 2) {
    stop("More than two predictors are necessary.")
    
  }
  
  scores <- M_Breiman(object = object, digits = digits)
  
  if (barplot == T){
    barplot <- barplot_importance(scores, threshold = threshold)
    return(list(scores = scores, barplot = barplot))
    
  } else {
    return(scores)
  }
}