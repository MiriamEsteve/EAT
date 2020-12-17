#' @title Efficiency Scores Jitter Plot
#'
#' @description This function returns a jitter plot. DMUs are grouped in nodes (same input paradigm) and its score are showed. A black point represents the score mean in a group and the line the standard deviation. Finally, the user can specify an upper bound (upb) and a lower bound (lwb) in order to show the labels.
#'
#' @param object An EAT object.
#' @param scores_EAT Dataframe with scores.
#' @param upb Numeric. Upper bound for labeling.
#' @param lwb Numeric. Lower bound for labeling.
#'
#' @importFrom ggplot2 ggplot aes geom_jitter stat_summary position_jitter
#' @importFrom dplyr %>% select
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
#' @return Geom jitter plot with DMUs and scores.
efficiency_jitter <- function(object, scores_EAT, upb = 1, lwb = 0) {
  if (class(object) != "EAT"){
    stop(paste(object, "must be an EAT object"))
  }
  
  groups <- object[["nodes_df"]][["leafnodes_df"]] %>%
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
    geom_text_repel(aes(label = ifelse(Score >= lwb & Score <= upb, 
                                       rownames(scores_df), "")))
  
  return(jitter_plot)
}

#' @title Efficiency Scores Density Plot
#'
#' @description Density plot for scores. EAT scores in red and FDH scores in blue.
#'
#' @param scores_EAT Dataframe with EAT scores.
#' @param scores_FDH Optional. Dataframe with FDH scores.
#'
#' @importFrom ggplot2 ggplot geom_density
#'
#' @export
#'
#' @return Density plot for scores and data.frame with numeric results.
efficiency_density <- function(scores_EAT, scores_FDH = NULL) {
  
  if (is.null(scores_FDH)) {
    
    scores_EAT <- as.data.frame(scores_EAT)
    scores_EAT$Model <- factor("EAT") 
    names(scores_EAT) <- c("EAT", "Model")
    
    efficiency_density <- ggplot(scores_EAT, aes(x = EAT, fill = Model, colour = Model)) +
      geom_density(alpha = .2) +
      xlab("Score") +
      ylab("Density")
    
  } else {
    
    scores_EAT <- as.data.frame(scores_EAT)
    scores_EAT$Model <- factor("EAT")
    names(scores_EAT)[1] <- "score"
    
    scores_FDH <- as.data.frame(scores_FDH)
    scores_FDH$Model <- factor("FDH")
    names(scores_FDH)[1] <- "score"
    
    scores_df <- rbind(scores_EAT, scores_FDH)
    
    efficiency_density <- ggplot(scores_df, aes(x = score, fill = Model, colour = Model)) +
      geom_density(alpha = .2) +
      xlab("Score") +
      ylab("Density") 
    
  }
  
  return(efficiency_density)
}




