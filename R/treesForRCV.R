#' @title Train and Test Sets Generation
#'
#' @description This function splits the original data in two new data sets: a train set and a test set.
#'
#' @param data Data to be split into train and test subsets.
#' @param fold Parts in which the original set is divided, to perform Cross-Validation.
#'
#' @return List structure with the train and the test set.
generateLv <- function(data, fold) {
  data <- data[sample(nrow(data), replace = FALSE), ] # Reorder to Cross-Validation
  data <- data %>% mutate(id = row_number())

  # Create test (Lv) y training (notLv)

  Lv <- vector("list", fold)
  notLv <- vector("list", fold)

  # Number of rows per fold

  numRowsFold <- floor(nrow(data) / fold)

  for (v in 0:(fold - 1)) {
    if (v == 0) {
      Lv[[v + 1]] <- data[1:numRowsFold, ] %>%
        mutate(id = row_number())

      notLv[[v + 1]] <- data[- (1:numRowsFold), ] %>%
        mutate(id = row_number())
    } else {
      Lv[[v + 1]] <- data[(v * numRowsFold + 1):((v + 1) * numRowsFold), ] %>%
        mutate(id = row_number())

      notLv[[v + 1]] <- data[- ((v * numRowsFold + 1):((v + 1) * numRowsFold)), ] %>%
        mutate(id = row_number())
    }
  }

  return(list(Lv, notLv))
}

#' @title Trees for RCV
#'
#' @description This function generates a deep EAT and all pruning for each train set.
#'
#' @param notLv Train set.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param fold Parts in which the original set is divided to perform Cross-Validation.
#' @param numStop Minimum number of observations in a node to be split.
#'
#' @return List with each possible pruning for the deep tree generated with train set and its associated alpha values.
treesForRCV <- function(notLv, x, y, fold, numStop) {
  TAiv <- vector("list", fold)

  for (v in 1:fold) {
    notLv[[v]] <- append(notLv[[v]], -1, 0)
    TAiv[[v]] <- deepEAT(notLv[[v]], x, y, numStop)
  }

  return(TAiv)
}
