#' @title EdSurvey Direct Estimation - TIMSS scoring
#' @description Scoring TIMSS data
#'
#' @param edf             a TIMSS \code{light.edsurvey.data.frame} or \code{edsurvey.data.frame}
#' @param polyParamTab    a dataframe containing IRT parameters for all polytomous items in \code{edf}
#' @param dichotParamTab  a dataframe containing IRT parameters for all dichotomous items in \code{edf}
#' @return                scored \code{edf}
#' @details This function scores TIMSS data. 
#' For multiple choice items, correct answers are assigned 1 point, and incorrect answers are assigned 0 points.
#' For constructed response items, correct answers are assigned 2 points, partially correct answers are assigned 1 point,
#' and incorrect answers are assigned 0 points. For both types of items, "NOT REACHED" and "OMITTED OR INVALID" are assigned 0 points.


scoreTIMSS <- function(edf, polyParamTab, dichotParamTab) {
  correct <- c("^CORRECT RESPONSE", "^CORRECT RESPONSE:1", "^CORRECT RESPONSE:2", "^CORRECT RESPONSE:3", "^CORRECT RESPONSE:4", "^CORRECT RESPONSE:5", "^CORRECT RESPONSE:6", "^CORRECT RESPONSE:7", "^CORRECT RESPONSE:8", "^CORRECT RESPONSE:9") 
  partial <- c("PARTIALLY CORRECT RESPONSE", "PARTIALLY CORRECT RESPONSE:1", "PARTIALLY CORRECT RESPONSE:2", "PARTIALLY CORRECT RESPONSE:3")
  incorrect <- c("INCORRECT RESPONSE", "INCORRECT RESPONSE:1", "INCORRECT RESPONSE:2", "INCORRECT RESPONSE:3", "INCORRECT RESPONSE:4", "INCORRECT RESPONSE:5", "INCORRECT RESPONSE:6", "INCORRECT RESPONSE:7", "INCORRECT RESPONSE:8", "INCORRECT RESPONSE:9")
  one_point <- c("A\\*", "B\\*", "C\\*", "D\\*") 
  zero_point <- c("^A", "^B", "^C", "^D")
  
  correct.string <- paste(correct, collapse = "|")
  partial.string <- paste(partial, collapse = "|")
  incorrect.string <- paste(incorrect, collapse = "|")
  one_point.string <- paste(c(correct, one_point), collapse = "|")
  zero_point.string <- paste(c(incorrect, zero_point), collapse = "|")
  
  valid1 <- c('0','1','2',NA)
  valid2 <- c('0','1',NA)
  
  for (item in polyParamTab$ItemID) {
    # Get rid of factor levels
    edf[[item]] <- as.character(edf[[item]])
    # Correct - 2 points
    edf[[item]] <- gsub(correct.string, replacement = 2, x = edf[[item]])
    # Partial - 1 point
    edf[[item]] <- gsub(partial.string, replacement = 1, x = edf[[item]])
    # Incorrect - 0 points
    edf[[item]] <- gsub(incorrect.string, replacement = 0, x = edf[[item]])
    # Not reached, invalid
    edf[[item]] <- gsub("NOT REACHED", replacement = 0, x = edf[[item]]) ####### change this later
    edf[[item]] <- gsub("OMITTED OR INVALID", replacement = 0, x = edf[[item]]) ##### change this later
    # check if all scored
    check <- unique(edf[[item]])
    check_sum <- sum(check %in% valid2)
    if (length(check) < check_sum) {
      stop (paste0(item,' was not scored properly'))
    }
    # Change to numeric
    edf[[item]] <- as.numeric(edf[[item]])
  }
  for (item in dichotParamTab$ItemID) {
    # Get rid of factor levels
    edf[[item]] <- as.character(edf[[item]])
    # Correct - 1 point
    edf[[item]] <- gsub(one_point.string, replacement = 1, x = edf[[item]])
    # Incorrect - 0 points
    edf[[item]] <- gsub(zero_point.string, replacement = 0, x = edf[[item]])
    # Not reached, invalid
    edf[[item]] <- gsub("NOT REACHED", replacement = 0, x = edf[[item]]) ####### change this later
    edf[[item]] <- gsub("OMITTED OR INVALID", replacement = 0, x = edf[[item]]) ##### change this later
    # check if all scored
    check <- unique(edf[[item]])
    check_sum <- sum(check %in% valid1)
    if (length(check) > check_sum) {
      stop (paste0(item,' was not scored properly'))
    }
    # Change to numeric
    edf[[item]] <- as.numeric(edf[[item]])
  }
  return(edf)
}
