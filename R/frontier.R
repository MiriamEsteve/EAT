#' @title EAT frontier graph for a single input and output scenario
#'
#' @description This function creates a plot with the frontier estimated by EAT for a scenario corresponding to one input and one output.
#' 
#' @name frontier
#'
#' @param object An EAT object.
#' @param train.data Logical. If \code{TRUE}, training DMUs are displayed.
#' @param train.color String. Color for training DMUs.
#' @param pch Integer. Point shape.
#' @param size Integer. Point size.
#' @param rwn Logical. If \code{TRUE}, rownames are displayed instead of points.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_text xlab ylab xlim ylim
#' @importFrom ggrepel geom_label_repel
#'
#' @return Plot with estimated production frontier
#' 
#' @export
frontier <- function(object, train.data = FALSE, train.color = "black", 
                     pch = 19, size = 1, rwn = FALSE){
  
  if (class(object) != "EAT"){
    stop(paste(object, "must be an EAT object"))
  }

  t_data <- object[["data"]][["data"]]
  rownames(t_data) <- object[["data"]][["row_names"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  tree <- object[["tree"]]
  x_names <- object[["data"]][["input_names"]]
  y_names <- object[["data"]][["output_names"]]

  if(length(x) > 1 || length(y) > 1){
    stop("More than one input or one output are not allowed")

  } else {

    fp <- data.frame(x_values = seq(min(t_data[, x]), max(t_data[, x]),  
                                    length.out = 2000))
    names(fp) <- x_names
    
    pred <- data.frame()
    for (i in 1:nrow(fp)){
      pred <- rbind(pred, predictor(tree, fp[i, ]))
    }
    
    pred <- cbind(fp, pred)
    names(pred) <- c("x1", "frontier")

    plot <- ggplot(t_data) +
      geom_line(data = pred, aes(x = x1, y = frontier), size = 1, colour = "turquoise4") +
      xlab(x_names) +
      ylab(y_names)
    
    if (train.data == FALSE){
      plot <- plot +
        xlim(c(min(pred$x1), max(pred$x1))) +
        ylim(c(min(pred$frontier) - (min(pred$frontier) / 10), max(pred$frontier)))
    } else {
      plot <- plot + 
        geom_point(aes_string(x = x_names, y = y_names),
                   color = train.color,
                   pch = pch,
                   size = size)
      
      if (rwn == TRUE){
        plot <- plot + 
          geom_text_repel(aes_string(x = x_names, y = y_names, 
                                     label = "rownames(t_data)"))
      }
    }
  }
  return(plot)
}

  
