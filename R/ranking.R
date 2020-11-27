#' @title Barplot variable importance
#'
#' @description This function generates a barplot with the importance of each variable.
#'
#' @param m Dataframe with the importance of each variable.
#' @param threshold Importance value in which a broken line should be graphed for feature selection.
#'
#' @importFrom ggplot2 ggplot geom_col xlab aes geom_hline
#' @importFrom conflicted conflict_prefer
#' @importFrom stats reorder
#'
#' @return Barplot that represents each variable in the x-axis and its importance in the y-axis. It returns the numeric values too.
barplot_importance <- function(m, threshold) {
  Importance <- NULL

  ggplot(m, aes(x = reorder(row.names(m), -Importance), y = Importance)) +
    geom_col() +
    geom_hline(yintercept = threshold, linetype = "dotted",
               color = "blue", size = 1.5) +
    xlab("Variable")
}

#' @title Importance of Breiman
#'
#' @description This function evaluates the importance of each predictor variable by Breiman method.
#'
#' @param data Data to be used.
#' @param tree Tree-object of EAT function.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param r Decimal units.
#' @param threshold Importance value in which a broken line should be graphed for feature selection.
#'
#' @importFrom dplyr %>% filter rename arrange
#' @importFrom conflicted conflict_prefer
#'
#' @export
#'
#' @return Dataframe with one column and the importance of each variable in rows.
M_Breiman <- function(data, tree, x, y, r = 2, threshold = 70) {
  conflict_prefer("filter", "dplyr")

  data <- preProcess(data, x, y, na.rm = T)

  x <- 1:((ncol(data) - 1) - length(y))
  y <- (length(x) + 1):(ncol(data) - 1)

  nX <- length(x)

  resultado <- imp_var_Breiman(data, tree, x, y, r)

  M <- as.list(rep(0, nX))

  xi <- desc <- Importance <- id <- NULL

  for (var in 1:nX) {
    for (t in 1:length(tree)) {
      if (tree[[t]][["s"]] == -1) next

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
    m <- round(((100 * M) / maxM), r)

    M <- rbind(M, m)
  } else {
    print("Divide by zero")
  }

  m <- t(m) %>%
    as.data.frame() %>%
    rename("Importance" = "V1") %>%
    arrange(desc(Importance))

  return(list(m, barplot_importance(m, threshold = threshold)))
}

#' @title Importance variable of Breiman
#'
#' @description This function recalculates all the possible splits, with the exception of the actual used, and for each node and variable gets the best split based on their degree of importance.
#'
#' @param data Data to be used.
#' @param tree Tree-object of EAT function.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param r Decimal units.
#'
#' @importFrom dplyr %>% filter
#'
#' @return A dataframe with the best split for each node and variable and its importance.
imp_var_Breiman <- function(data, tree, x, y, r) {
  id <- NULL

  index <- tree[[1]][["index"]]

  result <- list()

  empty <- list()

  nX <- length(x)

  for (t in 1:length(tree)) {
    if (tree[[t]][["s"]] == -1) next

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

        if (tL_p[["y"]] == Inf || tR_p[["y"]] == Inf) next

        tL_i <- tree[[tree[[t]][["SL"]]]][["index"]]

        tL_p_i <- tL_p[["index"]]

        tR_i <- tree[[tree[[t]][["SR"]]]][["index"]]

        tR_p_i <- tR_p[["index"]]

        imp <- round((sum(tL_p_i %in% tL_i) + sum(tR_p_i %in% tR_i)) / length(tL_i), r)

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

  X1 <- X2 <- X3 <- X4 <- X5 <- X6 <- X7 <- NULL

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
