#' @title Efficiency Analysis Trees Frontier Graph
#'
#' @description This function displays a plot with the frontier estimated by Efficiency Analysis Trees in a scenario of one input and one output.
#' 
#' @name frontier
#'
#' @param object An EAT object.
#' @param FDH Logical. If \code{TRUE}, FDH frontier is displayed.
#' @param observed.data Logical. If \code{TRUE}, observed DMUs are displayed.
#' @param observed.color String. Color for observed DMUs.
#' @param pch Integer. Point shape.
#' @param size Integer. Point size.
#' @param rwn Logical. If \code{TRUE}, rownames are displayed.
#' @param max.overlaps Exclude text labels that overlap too many things.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_step geom_text xlab ylab xlim ylim theme element_blank element_rect
#' @importFrom ggrepel geom_label_repel 
#' @importFrom reshape2 melt
#'
#' @return Plot with estimated production frontier
#' 
#' @examples
#' 
#' simulated <- Y1.sim(N = 50, nX = 1)
#' 
#' model <- EAT(data = simulated,
#'              x = 1,
#'              y = 2)
#' 
#' frontier <- frontier(object = model,
#'                      FDH = TRUE, 
#'                      observed.data = TRUE,
#'                      rwn = TRUE)
#' plot(frontier)
#' 
#' @export
frontier <- function(object, FDH = FALSE, observed.data = FALSE, observed.color = "black", 
                     pch = 19, size = 1, rwn = FALSE, max.overlaps = 10){
  
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
    
  } 

  t_data <- object[["data"]][["df"]]
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
      t_data <- append(t_data, -1, 0)
      
      # FDH prediction
      predictFDH <- predictFDH(t_data, 1, 2)
      
      t_data <- as.data.frame(t_data[-1])
      rownames(t_data) <- object[["data"]][["row_names"]]
      
      # Rename to estimFDH
      names(predictFDH)[2] <- "estimFDH"
      
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
    
    if (observed.data == FALSE){
      plot <- plot +
        xlim(c(min(ggdata$x1), max(ggdata$x1))) +
        ylim(c(min(ggdata$Frontier) - (min(ggdata$Frontier) / 10), max(ggdata$Frontier)))
    } else {
      plot <- plot + 
        geom_point(data = t_data, aes_string(x = x_names, y = y_names),
                   color = observed.color,
                   pch = pch,
                   size = size)
      
      if (rwn == TRUE){
        plot <- plot + 
          geom_text_repel(data = t_data, aes_string(x = x_names, y = y_names, 
                                                    label = "rownames(t_data)"),
                          max.overlaps = max.overlaps)
      }
    }
  }
  return(plot)
}

  
