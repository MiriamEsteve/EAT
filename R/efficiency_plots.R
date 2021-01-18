#' @title Efficiency Scores Jitter Plot
#'
#' @description This function returns a jitter plot. DMUs are grouped in nodes (same input paradigm) and its scores are showed. A black point represents the mean score in a group and the line the standard deviation. Finally, efficient DMUs are labeled and the user can specify an upper bound (upb) and a lower bound (lwb) in order to show extra labels.
#'
#' @param object An EAT object.
#' @param scores_EAT Dataframe with scores.
#' @param scores_model Mathematic programming model of scores_EAT. 
#' \itemize{
#' \item{\code{BCC_out}} BBC model. Output orientation.
#' \item{\code{BCC_in}}  BBC model. Input orientation.
#' \item{\code{DDF}}     Directional distance model.
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
    stop("object must be an EAT object")
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
#' @description Density plot for scores. EAT scores in red and FDH scores in blue.
#'
#' @param scores_EAT Dataframe with EAT scores.
#' @param scores_FDH Optional. Dataframe with FDH scores.
#' @param scores_RFEAT Optional. Dataframe with RFEAT scores.
#'
#' @importFrom ggplot2 ggplot geom_density
#'
#' @export
#' 
#' @examples 
#' 
#' X1 <- runif(50, 1, 10)
#' X2 <- runif(50, 2, 10)
#' Y1 <- log(X1) + 3 - abs(rnorm(50, mean = 0, sd = 0.4))
#' Y2 <- log(X1) + 2 - abs(rnorm(50, mean = 0, sd = 0.7))
#'
#' simulated <- data.frame(x1 = X1, x2 = X2, y1 = Y1, y2 = Y2)
#' 
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4), numStop = 5, fold = 5)
#'
#' scores <- efficiencyEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'                         scores_model = "BCC_out", r = 2, FDH = TRUE, na.rm = TRUE)
#' 
#' efficiencyDensity(scores_EAT = scores$EAT_BCC_out, scores_FDH = scores$FDH_BCC_out,
#'                   scores_RFEAT = NULL)
#'
#' @return Density plot for scores and data.frame with numeric results.
efficiencyDensity <- function(scores_EAT, scores_FDH = NULL, scores_RFEAT = NULL) {
  
  if (is.null(scores_FDH) && is.null(scores_RFEAT)){
    
    scores_EAT <- as.data.frame(scores_EAT)
    scores_EAT$Model <- factor("EAT") 
    names(scores_EAT) <- c("EAT", "Model")
    
    efficiencyDensity <- ggplot(scores_EAT, aes(x = EAT, fill = Model, colour = Model)) +
      geom_density(alpha = .2) +
      xlab("Score") +
      ylab("Density")
    
  } else if (is.null(scores_RFEAT)) {
    
    scores_EAT <- as.data.frame(scores_EAT)
    scores_EAT$Model <- factor("EAT")
    names(scores_EAT)[1] <- "score"
    
    scores_FDH <- as.data.frame(scores_FDH)
    scores_FDH$Model <- factor("FDH")
    names(scores_FDH)[1] <- "score"
    
    scores_df <- rbind(scores_EAT, scores_FDH)
    
    efficiencyDensity <- ggplot(scores_df, aes(x = score, fill = Model, colour = Model)) +
      geom_density(alpha = .2) +
      xlab("Score") +
      ylab("Density") 
    
  } else if (is.null(scores_FDH)) {
    
    scores_EAT <- as.data.frame(scores_EAT)
    scores_EAT$Model <- factor("EAT")
    names(scores_EAT)[1] <- "score"
    
    scores_RFEAT <- as.data.frame(scores_RFEAT)
    scores_RFEAT$Model <- factor("RFEAT")
    names(scores_RFEAT)[1] <- "score"
    
    scores_df <- rbind(scores_EAT, scores_RFEAT)
    
    efficiencyDensity <- ggplot(scores_df, aes(x = score, fill = Model, colour = Model)) +
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
    
    scores_RFEAT <- as.data.frame(scores_RFEAT)
    scores_RFEAT$Model <- factor("RFEAT")
    names(scores_RFEAT)[1] <- "score"
    
    scores_df <- rbind(scores_EAT, scores_FDH, scores_RFEAT)
    
    efficiencyDensity <- ggplot(scores_df, aes(x = score, fill = Model, colour = Model)) +
      geom_density(alpha = .2) +
      xlab("Score") +
      ylab("Density") 
  }
  
  return(efficiencyDensity)
}




