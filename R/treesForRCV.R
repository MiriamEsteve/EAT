#' @title Train and test sets generation.
#'
#' @description This function split the original data in two new data sets: a train set and a test set.
#'
#' @param data Data to be splited in train and test.
#' @param fold Parts in which is divided the original set to do Cross-Validation.
#'
#' @return List structure with the train and the test set.
generateLv <- function(data, fold){

  data <- data[sample(nrow(data), replace=FALSE), ] #Reorder to Cross-Validation
  data <- data %>% mutate(id = row_number())

  #Create test (Lv) y training (notLv)

  Lv <- vector("list", fold)
  notLv <- vector("list", fold)

  #Number of rows per fold

  numRowsFold <- floor(nrow(data) / fold)

  for (v in 0:(fold - 1)){

    if(v == 0){

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
#' @description This function generates a deep EAT and all its prunings for each train set.
#'
#' @param notLv Train set.
#' @param x Column input indexes in data.
#' @param y Column output indexes in data.
#' @param fold Parts in which is divided the original set to do Cross-Validation.
#' @param numStop Minimun number of observations in a node to be splitted.
#'
#' @return List with each possible pruning for the deep tree generated with train set and its alpha values associated.
treesForRCV <- function(notLv, x, y, fold, numStop){

  TAiv <- vector("list", fold)

  for(v in 1:fold){

    #Insert row to know deepEAT is called by this one
    notLv[[v]] <- append(notLv[[v]], -1, 0)
    TAiv[[v]] <- deepEAT(notLv[[v]], x, y, numStop)

  }

  return(TAiv)
}

