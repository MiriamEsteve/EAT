#' @title Plot EAT
#'
#' @description This function creates a plot with the tree-structure.
#'
#' @param tree Tree-object of EAT function.
#' @param data Matrix of data.
#' @param x Position of the input columns in data.
#' @param y Position of the output columns in data.
#' @param horizontal True if the plot is displayed with a horizontal layout.
#'
#' @import partykit
#' @import ggparty
#' @importFrom ggplot2 aes theme
#'
#' @export
#'
#' @return Plot object with the tree-structure.
EAT.plot <- function(tree, data, x, y, horizontal = FALSE) {

  data <- data[, c(x, y)] %>% as.data.frame()

  splitvar <- Id <- R <- nodesize <- Y <- NULL

  nodelist <- vector("list", length(tree))

  N <- nrow(data)

  for(i in 1:length(nodelist)){

    if(tree[[i]][["s"]] == -1) {

      nodelist[[i]] <- list("id" = as.integer(i),
                            "info" = list("Y" = lapply(tree[[i]][["y"]], round, 1),
                                          "R" = round(sqrt(tree[[i]][["R"]]), 2),
                                          "Id" = i,
                                          "n" = round(length(tree[[i]][["index"]]) * 100 / N, 1)))

    } else {

      nodelist[[i]] <- list("id" = as.integer(i),

                            "split" = partysplit(varid = as.integer(tree[[i]][["xi"]]),
                                                 breaks = round(tree[[i]][["s"]], 2),
                                                 right = FALSE),

                            "kids" = c(tree[[i]][["SL"]], tree[[i]][["SR"]]),

                            "info" = list("Y" = lapply(tree[[i]][["y"]], round, 1),
                                          "R" = round(sqrt(tree[[i]][["R"]]), 2),
                                          "Id" = i,
                                          "n" = round(length(tree[[i]][["index"]]) * 100 / N, 1)))

    }

  }

  node <- as.partynode(nodelist)

  py <- party(node, data)

  layout <- ggparty:::get_plot_data(py)[, 1:4]

  for(i in 2:(nrow(layout)-1)) {

    for(j in (i+1):nrow(layout)) {

      if(layout$parent[i] == layout$parent[j] && layout$y[i] > layout$y[j]) {

        layout$y[j] <- layout$y[i]

        break

      } else if(layout$parent[i] == layout$parent[j] && layout$y[j] > layout$y[i]) {

        layout$y[i] <- layout$y[j]

        break

      }
    }
  }

  if(length(y) == 1){

    infovars <- list(R = "$node$info$R",
                     Y = "$node$info$Y",
                     Id = "$node$info$Id",
                     n = "$node$info$n",
                     PR = "$node$info$PR")

    ggparty(py, horizontal = horizontal,
            add_vars = infovars,
            layout = layout[, 1:3]) +

      geom_edge(col = "black",
                size = 1,
                alpha = 1,
                ids = -1) +

      geom_edge_label(colour = "black", size = 3, shift = 0.6) +

      geom_node_label(aes(col = splitvar),

                      line_list = list(aes(label = paste(Id, ":", splitvar)),
                                       aes(label = paste(Y)),
                                       aes(label = paste(n, "%"))),

                      line_gpar = list(list(size = 8),
                                       list(size = 8),
                                       list(size = 8)),

                      ids = "inner") +

      geom_node_label(line_list = list(aes(label = paste(Id, ": R =", R)),
                                       aes(label = paste(Y)),
                                       aes(label = paste(n, "%"))),

                      line_gpar = list(list(size = 8),
                                       list(size = 8),
                                       list(size = 8)),

                      ids = "terminal") +

      theme(legend.position = "none")

  } else {

    infovars <- list(R = "$node$info$R",
                     Y = "$node$info$Y[[1]]",
                     Y2 = "$node$info$Y[[2]]",
                     Id = "$node$info$Id",
                     n = "$node$info$n",
                     PR = "$node$info$PR")

    ggparty(py, horizontal = horizontal,
            add_vars = infovars,
            layout = layout[, 1:3]) +

      geom_edge(col = "black",
                size = 1,
                alpha = 1,
                ids = -1) +

      geom_edge_label(colour = "black", size = 3, shift = 0.6) +

      geom_node_label(aes(col = splitvar),

                      line_list = list(aes(label = paste(Id, ":", splitvar)),
                                       aes(label = paste("(", Y, ",", Y2, ")")),
                                       aes(label = paste(n, "% obs"))),

                      line_gpar = list(list(size = 8),
                                       list(size = 8),
                                       list(size = 8)),

                      ids = "inner") +

      geom_node_label(line_list = list(aes(label = paste(Id, ": R =", R)),
                                       aes(label = paste("(", Y, ",", Y2, ")")),
                                       aes(label = paste(n, "% obs"))),

                      line_gpar = list(list(size = 8),
                                       list(size = 8),
                                       list(size = 8)),

                      ids = "terminal") +

      theme(legend.position = "none")

  }


}
