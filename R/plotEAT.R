#' @title Efficiency Analysis Trees Plot
#'
#' @description Plot a tree-structure for an Efficiency Analysis Trees model. 
#'
#' @param object An EAT object.
#'
#' @importFrom partykit as.partynode party partysplit
#' @importFrom ggparty ggparty geom_edge geom_edge_label geom_node_label
#' @importFrom ggplot2 theme aes
#' 
#' @return Plot object with the following elements for each node:
#' \itemize{
#' \item{id}: node index.
#' \item{R}: error at the node.
#' \item{n(t)}: number of observations at the node.
#' \item{an input name}: splitting variable.
#' \item{y}: output prediction.
#' }
#' 
#' @examples 
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#' 
#' plotEAT(EAT_model)
#' }
#' 
#' @export
plotEAT <- function(object) {
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
    
  } 
  
  tree <- object[["tree"]]
  data <- object[["data"]][["df"]]
  y <- object[["data"]][["y"]]
  
  N <- nrow(data)
  nodelist <- vector("list", length(tree))
  
  for (i in 1:length(nodelist)) {
    if (tree[[i]][["SL"]] == -1) {
      nodelist[[i]] <- list(
        "id" = as.integer(i),
        "info" = list(
          "Y" = lapply(tree[[i]][["y"]], round, 1),
          "R" = round(tree[[i]][["R"]], 2),
          "Id" = i,
          "n1" = length(tree[[i]][["index"]]),
          "n2" = round(length(tree[[i]][["index"]]) * 100 / N, 1)
        )
      )
    } else {
      nodelist[[i]] <- list(
        "id" = as.integer(i),
        "split" = partysplit(
          varid = as.integer(tree[[i]][["xi"]]),
          breaks = ifelse(class(tree[[i]][["s"]]) %in% c("ordered", "factor"),
                          tree[[i]][["s"]],
                          round(tree[[i]][["s"]], 2)),
          right = FALSE
        ),

        "kids" = c(tree[[i]][["SL"]], tree[[i]][["SR"]]),
        "info" = list(
          "Y" = lapply(tree[[i]][["y"]], round, 1),
          "R" = round(tree[[i]][["R"]], 2),
          "Id" = i,
          "n1" = length(tree[[i]][["index"]]),
          "n2" = round(length(tree[[i]][["index"]]) * 100 / N, 1)
        )
      )
    }
  }
  
  node <- as.partynode(nodelist)
  py <- party(node, data)
  layout <- layout(py)
  
  infovars <- vector("list", 5 + length(y))

  ynames <- c()

  for (i in 1:length(y)) {
    ynames <- append(ynames, gsub(" ", "", paste("Y", i)))
  }

  names(infovars) <- c("R", ynames, "Id", "n1", "n2", "PR")

  infovars[["R"]] <- "$node$info$R"
  infovars[["Id"]] <- "$node$info$Id"
  infovars[["n1"]] <- "$node$info$n1"
  infovars[["n2"]] <- "$node$info$n2"
  infovars[["PR"]] <- "$node$info$PR"

  for (i in 2:(length(y) + 1)) {
    infovars[[i]] <- gsub(" ", "", paste("$node$info$Y[[", i - 1, "]]"))
  }

  ggparty(py,
    add_vars = infovars,
    layout = layout[, 1:3]
  ) +

    geom_edge(
      col = "black",
      size = 1,
      alpha = 1,
      ids = -1
    ) +

    geom_edge_label(colour = "black", size = 3, shift = 0.6) +

    geom_node_label(aes(col = splitvar),
      line_list = list(
        aes(label = paste("Id:", Id)),
        aes(label = paste("R:", R)),
        aes(label = paste("n(t):", n1)),
        aes(label = paste(splitvar)),
        aes(label = paste("y: [", do.call(paste, c(lapply(ynames, as.name),
                                                    list(sep = ","))), "]", sep = ""))
      ),

      line_gpar = list(
        list(size = 7),
        list(size = 7),
        list(size = 7),
        list(size = 7),
        list(size = 7)
      ),

      ids = "inner"
    ) +

   geom_node_label(
     line_list = list(
       aes(label = paste("Id:", Id)),
       aes(label = paste("R:", R)),
       aes(label = paste("n(t):", n1)),
       aes(label = paste("y:[", do.call(paste, c(lapply(ynames, as.name),
                                                   list(sep = ","))), "]", sep = ""))
     ),

     line_gpar = list(
       list(size = 7),
       list(size = 7),
       list(size = 7),
       list(size = 7)
     ),

      ids = "terminal"
    ) +

    theme(legend.position = "none")
}

#' @title Layout for nodes in plotEAT
#'
#' @description This function modifies the coordinates of the nodes in the plotEAT function to overcome overlapping.
#'
#' @param py a party object.
#'
#' @importFrom ggparty ggparty
#'
#' @return Dataframe with suitable modifications of the node layout.
layout <- function(py) {

  layout <- ggparty(py)$data[, 1:4]

  for (i in 2:(nrow(layout) - 1)) {
    for (j in (i + 1):nrow(layout)) {
      if (layout$parent[i] == layout$parent[j] && layout$y[i] > layout$y[j]) {
        layout$y[j] <- layout$y[i]

        break
      } else if (layout$parent[i] == layout$parent[j] && layout$y[j] > layout$y[i]) {
        layout$y[i] <- layout$y[j]

        break
      }
    }
  }

  y_scale <- 1 / (length(unique(layout$y)) + 1)

  for(i in 2:nrow(layout)){
    if((layout[layout[i, "parent"], "y"] - layout[i, "y"]) != y_scale){
      layout[i, "y"] <- layout[layout[i, "parent"], "y"] - y_scale
    }
  }

  min_x <- 0.07
  nodes_modified <- c()
  condition <- TRUE

  while(condition == TRUE){
    for(i in 1:(nrow(layout) - 1)){
      for(j in (i + 1):nrow(layout)){
        if(layout[i, "y"] == layout[j, "y"] && abs(layout[i, "x"] - layout[j, "x"]) < min_x){
          if(layout[i, "x"] - 0.01 < 0) next
          layout[i, "x"] <- layout[i, "x"] - 0.007
          nodes_modified <- append(nodes_modified, c(i,j))
        }
      }
    }
    if(length(nodes_modified) != length(unique(nodes_modified))) {
      condition <- TRUE
      nodes_modified <- c()
    } else {
      condition <- FALSE
    }
  }

  return(layout)
}
