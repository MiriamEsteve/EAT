#' @title Efficiency Scores Jitter Plot
#'
#' @description This function returns a jitter plot from \code{ggplot2}. This graphic shows how DMUs are grouped into leaf nodes in a model built using the \code{EAT} function. Each leaf node groups DMUs with the same level of resources. The dot and the black line represent, respectively, the mean value and the standard deviation of the scores of its node. Additionally, efficient DMU labels always are displayed based on the model entered in the \code{scores_model} argument. Finally, the user can specify an upper bound \code{upn} and a lower bound \code{lwb} in order to show, in addition, the labels whose efficiency score lies between them.
#'
#' @param object An EAT object.
#' @param df_scores Dataframe with efficiency scores (from \code{efficiencyEAT} or \code{efficiencyCEAT}).
#' @param scores_model Mathematical programming model to calculate scores. 
#' \itemize{
#' \item{\code{BCC.OUT}} BCC model. Output-oriented.
#' \item{\code{BCC.INP}}  BCC model. Input-oriented.
#' \item{\code{DDF}}     Directional Distance Function.
#' \item{\code{RSL.OUT}} Russell model. Output-oriented.
#' \item{\code{RSL.INP}}  Russell model. Input-oriented.
#' \item{\code{WAM.MIP}} Weighted Additive Model. Measure of Inefficiency Proportions.
#' \item{\code{WAM.RAM}} Weighted Additive Model. Range Adjusted Measure of Inefficiency.
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
#'                             scores_model = "BCC.OUT", digits = 2, na.rm = TRUE)
#' 
#' efficiencyJitter(object = EAT_model, df_scores = EAT_scores, scores_model = "BCC.OUT")
#'
#' @return Jitter plot with DMUs and scores.
efficiencyJitter <- function(object, df_scores, scores_model, upb = NULL, lwb = NULL) {
  if (class(object) != "EAT"){
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
    
  } 
  
  # Available score model
  
  if (!scores_model %in% c("BCC.OUT", "BCC.INP", "DDF", 
                           "RSL.OUT", "RSL.INP", "WAM.MIP",
                           "WAM.RAM")){
    stop(paste(scores_model, "is not available. Please, check help(\"efficiencyEAT\")"))
  }
  
  # Available lower and upper bound
  
  if (!is.null(upb) && upb < 0) {
    stop(paste('upb =', upb, 'must be greater than or equal 0.'))
    
  }
  
  if (!is.null(lwb) && lwb < 0) {
    stop(paste('lwb =', lwb, 'must be greater than or equal 0.'))
    
  }
  
  groups <- object[["nodes_df"]] %>%
    filter(SL == -1) %>%
    select(id, index)
  
  scores_df <- data.frame(Score = df_scores,
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
  
  ifelse(scores_model %in% c("BCC.OUT", "BCC.INP", "RSL.OUT", "RSL.INP"),
         score <- 1,
         score <- 0)
  
  jitter_plot <- jitter_plot + geom_text_repel(aes(label = ifelse(Score == score, 
                                                                  rownames(scores_df), "")),
                                               show.legend = FALSE)
  
  if (!is.null(lwb) && !is.null(upb)){
    jitter_plot <- jitter_plot + 
      geom_text_repel(aes(label = ifelse(Score >= lwb & Score <= upb, 
                                         rownames(scores_df), "")),
                      show.legend = FALSE)
    
  } else if (!is.null(upb)) {
    jitter_plot <- jitter_plot +
      geom_text_repel(aes(label = ifelse(Score <= upb, 
                                         rownames(scores_df), "")),
                      show.legend = FALSE)
    
  } else if (!is.null(lwb)) {
    jitter_plot <- jitter_plot +
      geom_text_repel(aes(label = ifelse(Score >= lwb, 
                                         rownames(scores_df), "")),
                      show.legend = FALSE)
  }
  
  return(jitter_plot)
}

#' @title Efficiency Scores Density Plot
#'
#' @description Density plot for efficiency scores.
#'
#' @param df_scores Dataframe with efficiency scores.
#' @param model String vector. Scoring models in the order shown in \code{df_scores} by columns. The available models are: \code{"EAT"}, \code{"FDH"}, \code{"CEAT"}, \code{"DEA"} and \code{"RFEAT"}.
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
#'                         scores_model = "BCC.OUT", digits = 2, FDH = TRUE, na.rm = TRUE)
#' 
#' efficiencyDensity(df_scores = scores[, 5:6],
#'                   model = c("EAT", "FDH"))
#'
#' @return Density plot for efficiency scores.
efficiencyDensity <- function(df_scores, model = c("EAT", "FDH")) {
  
  if (!all(model %in% c("EAT", "FDH", "RFEAT", "CEAT", "DEA"))){
    stop(paste("Some model is not allowed. Please, check help(\"efficiencyDensity\")"))
  }
  
  names(df_scores) <- model
  
  df_scores <- melt(df_scores, id.vars = NULL)
  names(df_scores)[1] <- "Model"
  
  efficiencyDensity <- ggplot(df_scores, aes(x = value, fill = Model, colour = Model)) +
    geom_density(alpha = .2) +
    xlab("Score") +
    ylab("Density")
  
  return(efficiencyDensity)
  
}




