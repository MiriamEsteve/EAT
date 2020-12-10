#' @title Single input and output scenario EAT frontier
#'
#' @description This function creates a plot with the frontier estimated by EAT for the scenario corresponding to one input and one output.
#'
#' @param t An EAT object.
#' @param train.data Logical. If T, training DMUs are displayed.
#' @param train.color String. Color for training DMUs.
#' @param pch Integer. Point shape.
#' @param rownames Logical. If T, row names are displayed instead of points.
#' @param rwn.size Integer. Rowname size.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_text xlab ylab
#' @importFrom conflicted conflict_prefer
#'
#' @return Scatter plot with DMUs and the frontier predicted by EAT.
#' 
#' @export
frontier <- function(t, train.data = FALSE, train.color = "black", pch = 19, rownames = FALSE, 
                     rwn.size = 3){
  conflict_prefer("predict", "eat")
  
  t_data <- t[["data"]][["data"]]
  rownames(t_data) <- t[["data"]][["row_names"]]
  x <- t[["data"]][["x"]]
  y <- t[["data"]][["y"]]
  tree <- t[["tree"]]
  x_names <- t[["data"]][["input_names"]]
  y_names <- t[["data"]][["output_names"]]

  if(length(x) > 1 || length(y) > 1){
    stop(cat("More than one input or one output are not allowed"))

  } else {

    x_values <- NULL
    
    fp <- data.frame(x_values = seq(min(t_data[, x]), max(t_data[, x]),  length.out = 2000))
    names(fp) <- x_names
    pred <- predict(t, fp)
    fp[, "frontier"] <- pred
    names(fp)[1] <- "x1"

    plot <- ggplot(t_data) +
     geom_line(data = fp, aes(x = x1, y = frontier), size = 1, colour = "turquoise4") +
      xlab(x_names) +
      ylab(y_names)
    
    if (train.data == FALSE){
      plot <- plot +
        xlim(c(min(fp$x1), max(fp$x1))) +
        ylim(c(min(fp$frontier) - (min(fp$frontier) / 10), max(fp$frontier)))
    } else {
      if (rownames == T){
        plot <- plot +
          geom_text(aes_string(x = x_names, 
                               y = y_names, 
                               label = "rownames(t_data)"),
                    color = train.color,
                    size = rwn.size)
      } else {
        plot <- plot + 
          geom_point(aes_string(x =x_names, y = y_names),
                     color = train.color,
                     pch = pch)
      }
    }
  }
  return(plot)
}