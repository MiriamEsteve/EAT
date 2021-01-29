#' @title Efficiency Scores Jitter Plot
#'
#' @description This function returns a jitter plot from \code{ggplot2}. This graphic shows how DMUs are grouped into leaf nodes in a model built using the \code{EAT} function. Each leaf node groups DMUs with the same level of resources. The dot and the black line represent, respectively, the mean value and the standard deviation of the scores of its node. Additionally, efficient DMU labels always are displayed based on the model entered in the \code{scores_model} argument. Finally, the user can specify an upper bound \code{upn} and a lower bound \code{lwb} in order to show, in addition, the labels which efficiency score is between them.
#'
#' @param object An EAT object.
#' @param scores_EAT Dataframe with scores (from \code{efficiencyEAT} or \code{efficiencyCEAT}).
#' @param scores_model Mathematic programming model of scores_EAT. 
#' \itemize{
#' \item{\code{BCC_out}} BCC model. Output orientation.
#' \item{\code{BCC_in}}  BCC model. Input orientation.
#' \item{\code{DDF}}     Directional Distance Model.
#' \item{\code{RSL_out}} Rusell model. Output orientation
#' \item{\code{RSL_in}}  Rusell model. Input orientation.
#' \item{\code{WAM}}     Weighted Additive model.
#' } 
#' @param upb Numeric. Upper bound for labeling.
#' @param lwb Numeric. Lower bound for labeling.
#'
#' @importFrom ggplot2 ggplot aes geom_jitter stat_summary position_jitter labs
#' @importFrom dplyr %>% select filter
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#' 
#' @examples 
#' 
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#'
#' EAT_scores <- efficiencyEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = EAT_model,
#'                             scores_model = "BCC_out", r = 2, na.rm = TRUE)
#' 
#' efficiencyJitter(object = EAT_model, scores_EAT = EAT_scores, scores_model = "BCC_out")
#'
#' @return Jitter plot with DMUs and scores.
efficiencyJitter <- function(object, scores_EAT, scores_model, upb = NULL, lwb = NULL) {
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object"))
    
  } 
  
  if (!scores_model %in% c("BCC_out", "BCC_in", "DDF", 
                           "RSL_out", "RSL_in", "WAM")){
    stop(paste(scores_model, "is not available. Please, check help(efficiencyEAT)"))
  }
  
  groups <- object[["nodes_df"]] %>%
    filter(SL == -1) %>%
    select(id, index)
  
  scores_df <- data.frame(Score = scores_EAT,
                          Group = NA)
  
  names(scores_df) <- c("Score", "Group")
  
  rownames(scores_df) <- object[["data"]][["row_names"]]
  
  for (i in 1:nrow(scores_df)){
    scores_df[groups[i, "index"][[1]], "Group"] <- groups[i, "id"]
  }
  
  scores_df$Group <- as.factor(scores_df$Group)
  
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m - sd(x)
    ymax <- m + sd(x)
    return(c(y = m, ymin = ymin, ymax = ymax))
  }
  
  jitter_plot <- ggplot(scores_df, aes(x = Group, y = Score, color = Group)) + 
    geom_jitter(position = position_jitter(0.2), size = 2) +
    stat_summary(fun.data = data_summary, color = "black") +
    labs(color = "Leaf node index")
  
  if (scores_model %in% c("BCC_out", "BCC_in", "RSL_out", "RSL_in")){
    jitter_plot <- jitter_plot +
    geom_text_repel(aes(label = ifelse(Score == 1, 
                                         rownames(scores_df), "")))
  } else {
    jitter_plot <- jitter_plot +
      geom_text_repel(aes(label = ifelse(Score == 0, 
                                         rownames(scores_df), "")))
  }
  
  if (!is.null(lwb) && !is.null(upb)){
    jitter_plot <- jitter_plot + 
      geom_text_repel(aes(label = ifelse(Score >= lwb & Score <= upb, 
                                         rownames(scores_df), "")))
  } else if (!is.null(upb)) {
    jitter_plot <- jitter_plot +
      geom_text_repel(aes(label = ifelse(Score <= upb, 
                                         rownames(scores_df), "")))
  } else if (!is.null(lwb)) {
    jitter_plot <- jitter_plot +
      geom_text_repel(aes(label = ifelse(Score >= lwb, 
                                         rownames(scores_df), "")))
  }
  
  return(jitter_plot)
}

#' @title Efficiency Scores Density Plot
#'
#' @description Density plot for efficiency scores.
#'
#' @param scores Dataframe with efficiency scores.
#' @param model String vector. Scoring models in the order shown in \code{scores} by columns. The available models are: \code{"EAT"}, \code{"FDH"}, \code{"CEAT"}, \code{"DEA"} and \code{"RFEAT"}.
#'
#' @importFrom ggplot2 ggplot geom_density
#' @importFrom reshape2 melt
#'
#' @export
#' 
#' @examples 
#' 
#' simulated <- eat:::X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#'
#' scores <- efficiencyEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'                         scores_model = "BCC_out", r = 2, FDH = TRUE, na.rm = TRUE)
#' 
#' efficiencyDensity(scores = scores[, 5:6],
#'                   model = c("EAT", "FDH"))
#'
#' @return Density plot for efficiency scores.
efficiencyDensity <- function(scores, model = c("EAT", "FDH")) {
  
  if (!all(model %in% c("EAT", "FDH", "RFEAT", "CEAT", "DEA"))){
    stop(paste("Some model in", model, "is not allowed. Please, check help(\"efficiencyDensity\")"))
  }
  
  names(scores) <- model
  
  scores <- melt(scores, id.vars = NULL)
  names(scores)[1] <- "Model"
  
  efficiencyDensity <- ggplot(scores, aes(x = value, fill = Model, colour = Model)) +
    geom_density(alpha = .2) +
    xlab("Score") +
    ylab("Density")
  
  return(efficiencyDensity)
  
}




