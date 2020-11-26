#' @title EAT frontier
#'
#' @description This function creates a plot with EAT frontier for the scenario corresponding to one input and one output.
#'
#' @param data Data used for modeling.
#' @param tree List structure with the tree nodes.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param rownames logical. If T, row names are displayed in the plot instead of points.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_text xlab ylab
#' @importFrom conflicted conflict_prefer
#'
#' @export
#'
#' @return Scatter plot with DMUs and frontier predicted by `EAT`
frontier <- function(data, tree, x, y, rownames = FALSE){
  conflict_prefer("predict", "eat")

  if(length(x) > 1 || length(y) > 1){
    stop(cat("More than one input or one output are not allowed"))

  } else {

    x_values <- NULL

    fp <- data.frame(x_values = seq(min(data[, x]), max(data[, x]),  length.out = 2000),
                     frontier = 0)
    pred <- predict(tree, fp, 1, 2)
    fp[, "frontier"] <- pred

    plot <- ggplot(data) +
     geom_line(data = fp, aes(x = x_values, y = frontier), size = 1, colour = "turquoise4") +
      xlab(names(data)[x]) +
      ylab(names(data)[y])

    if (rownames == T){
      plot <- plot +
        geom_text(aes_string(x = names(data)[x], y = names(data)[y], label = "rownames(data)"))
    } else {
      plot <- plot +
        geom_point(aes_string(x = names(data)[x], y = names(data)[y]))
    }
  }
  return(plot)
}
