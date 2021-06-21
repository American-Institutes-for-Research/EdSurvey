#' @title EdSurvey Direct Estimation - NAEP scoring
#' @description Producing the response-points table for NAEP data for use in \code{mml.sdf}
#'
#' @param filename      the name of the .fr2 file for the NAEP data  
#' @param ployItems     a vector of IDs of polytomous items to be scored, a subset of IDs in .fr2
#' @param dichotItems   a vector of IDs of dichotomous items to be scored, a subset of IDs in .fr2
#' @param adjustedData  a dataframe containing adjusted items, including the item ID, 
#'                      a string of points before adjustment, a and string of points after adjustment
#' @return              a dataframe containing item ID, response, and points
#' @details This function reads in the NAEP data's fr2 file and creates a table of the answer choices and 
#' corresponding number of points for each item. 
#' For example, "A *" is assigned 1 point and "C" is assigned 0 points for item m085501 in the NAEPprimer data. 
#' For multiple choice items, "Illegible", "Non-Rateable", and "Off Task" are assigned 0 (incorrect); 
#' "Multiple" and "Omitted" are assigned 8 (the omitted code); and 
#' "Not Reached" and "Missing" are assigned NA (missing).
#' For constructed response items, "Omitted", "Illegible", "Non-Rateable", "Off Task" are assigned 0 (incorrect); 
#' "Multiple" is assigned 8 (the omitted code); and "Not Reached" and "Missing" are assigned NA (missing).

getNAEPScoreCard <- function(filename, polyItems, dichotItems, adjustedData, scoreDict = defaultNAEPScoreCard()) {
  # all items
  itemColsClean <- c(polyItems, dichotItems)
  # fr2 file structure
  t <- try(mrcFile <- readLines(filename), silent=TRUE)
  
  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=T)
  mrcFile <- unlist(mrcFile)
  
  # read in the variables from the file, this is based on the information ETS
  # shared with us
  variableName <- trimws(substring(mrcFile, 1, 8)) # name of the variable
  variableName <- unlist(lapply(variableName, tolower))
  
  # indices/lines of question item variables
  indices <-  match(itemColsClean, variableName)
  itemLines <- mrcFile[indices]
  itemLines <- itemLines[!is.na(itemLines)]
  
  # initialize empty dataframe to build
  scoreCard <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(scoreCard) <- c('key', 'answer', 'score')
  
  # codes in EdSurvey NAEP data
  # scoreDict <- list(resCat=c("Multiple", "Not Reached", "Missing", "Omitted", "Illegible", "Non-Rateable", "Off Task"),
  #                       point=c(8, NA, NA, 8, 0, 0, 0))
  
  # items with scores that have been adjusted
  adjustedItems <- adjustedData$NAEPid
  
  # get item key, answers, and points per item
  for (line in itemLines) {
    # initialize portion of scorecard for item 
    subScoreCard <- data.frame(matrix(ncol = 3, nrow = 12))
    colnames(subScoreCard) <- c('key', 'answer', 'score')
    
    # get key (question id)
    itemId <- tolower(substring(line, 1,7))
    
    # get points awarded by splitting string (like 0010)
    points <- unlist(strsplit(substring(line, 71,90), ' '))[1]
    
    # switch to new score points if item scoring has been adjusted
    if (itemId %in% adjustedItems) {
      pointsNew <- trimws(gsub(',','', adjustedData[adjustedData$NAEPid==itemId, 'to']))
      pointsNew <- gsub(' ','', pointsNew)
      # check that number of points match old numbers
      if (nchar(points) != nchar(pointsNew)) {
        stop(paste0('Number of adjusted scores for item ', itemId, ' is different from original.'))
      } else {
        points <- pointsNew
      }
    }
    
    # split and process points
    if (nchar(points) != 0) {
      points <- unlist(strsplit(points,''))
      points <- replace(NA * c(1:12), c(1:length(points)), points)
    } else {
      points <- NA * c(1:12)
    }
    
    # get answers (like A, B *, C)
    labels <- c()
    start <- 93
    end <- 119
    for (i in 1:12) {
      the_label <- getLabel(line, start, end)
      labels <- c(labels, the_label)
      start <- start+28
      end <- end+28
    }
    
    # set points for other types of answers (like omitted, illegible, etc.)
    if (itemId %in% dichotItems) {
      # this is a multiple choice question
      for (l in 1:length(labels)) {
        # find the corresponding point score in dict
        newScore <- scoreDict$pointMult[match(labels[l], scoreDict$resCat)]
        if (! is.na(newScore)) {
          points[l] <- newScore
        }
      }
    } else {
      # this is a constructed answer question
      for (l in 1:length(labels)) {
        newScore <- scoreDict$pointConst[match(tolower(labels[l]), tolower(scoreDict$resCat))]
        if (! is.na(newScore)) {
          points[l] <- newScore
        }
      }
    }
    
    # build sub scorecard and bind 
    subScoreCard$answer <- labels
    subScoreCard$score <- points
    subScoreCard$key <- itemId
    scoreCard <- rbind(scoreCard, subScoreCard)
  }
  
  # reconfigure
  scoreCard$score <- as.numeric(scoreCard$score)
  scoreCard <- scoreCard[complete.cases(scoreCard),] # take out rows with NA ScorePoints
  colnames(scoreCard) <- c('key', 'answer', 'score')
  
  # extra configuration for partial scores (make labeling uniform)
  partialKeys <- scoreCard[scoreCard$answer=='Partial','key']
  partialNums <- table(partialKeys) 
  twoPartials <- names(partialNums[partialNums==2]) #items with 2 partial scores
  for (item in twoPartials) {
    scoreCard[scoreCard$key==item & scoreCard$answer=='Partial', 'answer'] <- c('Partial1','Partial2')
  }
  threePartials <-names(partialNums[partialNums==3]) #items with 3 partial scores
  for (item in threePartials) {
    scoreCard[scoreCard$key==item & scoreCard$answer=='Partial', 'answer'] <- c('Partial1','Partial2','Partial3')
  }
  
  return(scoreCard)
}

getLabel <- function(line, first, last) {
  #get answer labels (A, B *, C, Correct, Omitted, etc.)
  part <- trimws(substr(line, first, last)) #get the part of string needed with first and last index
  return (trimws(gsub(" \\d+$", "", part))) #grab label (everything before digits) and trim white space
}

defaultNAEPScoreCard <- function() {
  scoreDict <- data.frame(resCat=c("Multiple", "Not Reached", "Missing", "Omitted", "Illegible", "Non-Rateable", "Off Task"),
                           pointMult=c(8, NA, NA, 8, 0, 0, 0),
                           pointConst=c(0, NA, NA, 0, 0, 0, 0))
  return(scoreDict)
}
