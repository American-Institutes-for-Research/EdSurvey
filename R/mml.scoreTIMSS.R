#' @title EdSurvey Direct Estimation - TIMSS scoring
#' @description Scoring TIMSS data
#'
#' @param edf             a TIMSS \code{light.edsurvey.data.frame} or \code{edsurvey.data.frame}
#' @param polyParamTab    a dataframe containing IRT parameters for all polytomous items in \code{edf}
#' @param dichotParamTab  a dataframe containing IRT parameters for all dichotomous items in \code{edf}
#' @param scoreCard       unused
#' @return                scored \code{edf}
#' @details This function scores TIMSS data.
#' For multiple choice items, correct answers are assigned 1 point, and incorrect answers are assigned 0 points.
#' For constructed response items, correct answers are assigned 2 points, partially correct answers are assigned 1 point,
#' and incorrect answers are assigned 0 points. For both types of items, "NOT REACHED" and "OMITTED OR INVALID" are assigned 0 points.
#' these defaults can be changed by modifying the \code{scoreDict} columns \code{pointMult} and \code{pointConst}, respectively.
#' @export
scoreTIMSS <- function(edf, polyParamTab, dichotParamTab, scoreCard = NULL) {
  scoreDict <- defaultTIMSSScoreDict()
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

  valid1 <- unique(as.character(scoreDict$pointMult)) # multiple choice / dichot
  valid2 <- unique(as.character(scoreDict$pointConst)) # constructed response / poly

  constCorrect <- scoreDict[scoreDict$resCat == "Correct", "pointConst"]
  constPart <- scoreDict[scoreDict$resCat == "Partial", "pointConst"]
  constIncorrect <- scoreDict[scoreDict$resCat == "Incorrect", "pointConst"]
  constNotR <- scoreDict[scoreDict$resCat == "Not reached", "pointConst"]
  constOmit <- scoreDict[scoreDict$resCat == "Omitted", "pointConst"]

  multCorrect <- scoreDict[scoreDict$resCat == "Correct", "pointMult"]
  multIncorrect <- scoreDict[scoreDict$resCat == "Incorrect", "pointMult"]
  multNotR <- scoreDict[scoreDict$resCat == "Not reached", "pointMult"]
  multOmit <- scoreDict[scoreDict$resCat == "Omitted", "pointMult"]
  missing_items <- c()
  if(!is.null(polyParamTab)) {
    for (item in polyParamTab$ItemID) {
      if(item %in% colnames(edf)) {
        # Get rid of factor levels
        i0 <- edf[[item]] <- as.character(edf[[item]])
        # Correct - 2 points
        edf[[item]] <- gsub(correct.string, replacement = constCorrect, x = edf[[item]])
        # Partial - 1 point
        edf[[item]] <- gsub(partial.string, replacement = constPart, x = edf[[item]])
        # Incorrect - 0 points
        edf[[item]] <- gsub(incorrect.string, replacement = constIncorrect, x = edf[[item]])
        # Not reached, invalid
        edf[[item]] <- gsub("NOT REACHED", replacement = constNotR, x = edf[[item]])
        edf[[item]] <- gsub("OMITTED OR INVALID", replacement = constOmit, x = edf[[item]])
        edf[[item]] <- gsub("OMITTED", replacement = constOmit, x = edf[[item]])
        # check if all scored
        check <- unique(edf[[item]])
        check_sum <- sum(check %in% valid2)
        if (length(check) < check_sum) {
          print(table(original=i0, scored=edf[[item]]))
          stop(paste0(item, " was not scored properly"))
        }
        # Change to numeric
        edf[[item]] <- as.numeric(edf[[item]])
      } else {
        # could not find this item
        missing_items <- c(missing_items, item)
      }
    }
  }
  
  for (item in dichotParamTab$ItemID) {
    if(item %in% colnames(edf)) {
      # Get rid of factor levels
      i0 <- edf[[item]] <- as.character(edf[[item]])
      # Correct - 1 point
      edf[[item]] <- gsub(one_point.string, replacement = multCorrect, x = edf[[item]])
      # Incorrect - 0 points
      edf[[item]] <- gsub(zero_point.string, replacement = multIncorrect, x = edf[[item]])
      # Not reached, invalid
      edf[[item]] <- gsub("NOT REACHED", replacement = multNotR, x = edf[[item]])
      edf[[item]] <- gsub("OMITTED OR INVALID", replacement = multOmit, x = edf[[item]])
      edf[[item]] <- gsub("OMITTED", replacement = multOmit, x = edf[[item]])
      # check if all scored
      check <- unique(edf[[item]])
      check_sum <- sum(check %in% valid1)
      if (length(check) > check_sum) {
        print(table(original=i0, scored=edf[[item]]))
        stop(paste0(item, " was not scored properly"))
      }
      # Change to numeric
      edf[[item]] <- as.numeric(edf[[item]])
    } else {
      # could not find this item
      missing_items <- c(missing_items, item)
    }
  }
  if(length(missing_items) > 0) {
    warning(paste0("Could not find items: ", pasteItems(dQuote(missing_items))))
  }
  return(edf)
}


#' @export
defaultTIMSSScoreDict <- function() {
  scoreDict <- data.frame(
    resCat = c("Correct", "Partial", "Incorrect", "Not reached", "Omitted"),
    pointConst = c(2, 1, 0, 0, 0),
    pointMult = c(1, NA, 0, 0, 0)
  )
  return(scoreDict)
}
