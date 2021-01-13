#' @title EAT frontier graph for a single input and output scenario
#'
#' @description This function displays a plot with the frontier estimated by EAT for a scenario corresponding to one input and one output.
#' 
#' @name frontier
#'
#' @param object An EAT object.
#' @param FDH Logical. If \code{TRUE}, FDH frontier is displayed.
#' @param train.data Logical. If \code{TRUE}, training DMUs are displayed.
#' @param train.color String. Color for training DMUs.
#' @param pch Integer. Point shape.
#' @param size Integer. Point size.
#' @param rwn Logical. If \code{TRUE}, rownames are displayed instead of points.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_step geom_text xlab ylab xlim ylim theme element_blank element_rect
#' @importFrom ggrepel geom_label_repel
#' @importFrom reshape2 melt
#'
#' @return Plot with estimated production frontier
#' 
#' @examples
#' 
#' data("PISAindex")
#' n <- nrow(PISAindex)
#' t_index <- sample(1:n, n * 0.8)
#' training <- PISAindex[t_index, ]
#' test <- PISAindex[-t_index, ]
#' 
#' single_model <- EAT(data = training, x = 6, y = 3)
#' 
#' frontier <- frontier(object = single_model, train.data = TRUE, train.color = "#F8766D",
#'                      rwn = TRUE, size = 1.5)
#' 
#' plot(frontier)
#' 
#' @export
frontier <- function(object, FDH = FALSE, train.data = FALSE, train.color = "black", 
                     pch = 19, size = 1, rwn = FALSE){
  
  if (class(object) != "EAT"){
    stop(paste(object, "must be an EAT object"))
  }

  t_data <- object[["data"]][["df"]]
  rownames(t_data) <- object[["data"]][["row_names"]]
  x <- object[["data"]][["x"]]
  y <- object[["data"]][["y"]]
  tree <- object[["tree"]]
  x_names <- object[["data"]][["input_names"]]
  y_names <- object[["data"]][["output_names"]]

  if(length(x) > 1 || length(y) > 1){
    stop("More than one input or one output are not allowed")

  } else {
    
    # Empty data.frame
    pred <- data.frame()
    
    # Predictions for EAT
    for (i in 1:nrow(t_data)){
      pred <- rbind(pred, predictor(tree, t_data[i, 1]))
    }
    
    # Join data input and prediction
    pred <- cbind(t_data, pred)
    
    # Rename to estimEAT
    names(pred) <- c("x1", "y", "estimEAT")
    
    if (FDH == TRUE){
      # FDH prediction
      predictFDH <- predictFDH(t_data, 1, 2)
      
      # Rename to estimFDH
      names(predictFDH)[3] <- "estimFDH"
      
      # Data to ggplot2
      ggdata <- data.frame(x1 = pred$x1, y = pred$y,
                           EAT = pred$estimEAT,
                           FDH = predictFDH$estimFDH)
      
      ggdata <- melt(ggdata, id.vars = c("x1", "y"))
      
      names(ggdata)[3:4] <- c("Model", "Frontier")
      
      # Change Model order for order of colors
      ggdata$Model <- factor(ggdata$Model, c("FDH", "EAT"))
      
    } else {
      # Data to ggplot2
      ggdata <- pred
      ggdata$Model <- "EAT"
      ggdata <- ggdata[, c(1, 2, 4, 3)]
      names(ggdata)[4] <- "Frontier"
    }
    
    plot <- ggplot(data = ggdata) +
      geom_step(aes(x = x1, y = Frontier, color = Model)) +
      xlab(x_names) +
      ylab(y_names) +
      theme(
        legend.box.background = element_rect(color = "grey", size = 0.5),
        legend.position = c(0.06, 0.9),
        legend.justification = "center",
        legend.box.just = "right",
        legend.title = element_blank()
      )
    
    if (train.data == FALSE){
      plot <- plot +
        xlim(c(min(ggdata$x1), max(ggdata$x1))) +
        ylim(c(min(ggdata$Frontier) - (min(ggdata$Frontier) / 10), max(ggdata$Frontier)))
    } else {
      plot <- plot + 
        geom_point(data = t_data, aes_string(x = x_names, y = y_names),
                   color = train.color,
                   pch = pch,
                   size = size)
      
      if (rwn == TRUE){
        plot <- plot + 
          geom_text_repel(data = t_data, aes_string(x = x_names, y = y_names, 
                                     label = "rownames(t_data)"))
      }
    }
  }
  return(plot)
}

  
