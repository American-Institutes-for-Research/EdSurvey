# @title EdSurvey Direct Estimation - NAEP scoring
# @description Producing the response-points table for NAEP data for use in \code{mml.sdf}
#
# @param filename      the name of the .fr2 file for the NAEP data
# @param ployItems     a vector of IDs of polytomous items to be scored, a subset of IDs in .fr2
# @param dichotItems   a vector of IDs of dichotomous items to be scored, a subset of IDs in .fr2
# @param adjustedData  a dataframe containing adjusted items, including the item ID,
#                      a string of points before adjustment, a and string of points after adjustment
# @param scoreDict     a data frame described in details
# @return              a dataframe containing item ID, response, and points
# @details This function reads in the NAEP data's fr2 file and creates a table of the answer choices and
# corresponding number of points for each item.
# For example, "A *" is assigned 1 point and "C" is assigned 0 points for item m085501 in the NAEPprimer data.
# For multiple choice items, "Illegible", "Non-Rateable", and "Off Task" are assigned 0 (incorrect);
# "Multiple" and "Omitted" are assigned 8 (the omitted code); and
# "Not Reached" and "Missing" are assigned NA (missing).
# For constructed response items, "Omitted", "Illegible", "Non-Rateable", "Off Task" are assigned 0 (incorrect);
# "Multiple" is assigned 8 (the omitted code); and "Not Reached" and "Missing" are assigned NA (missing).
getNAEPScoreCard <- function(filename, polyItems, dichotItems, adjustedData, scoreDict = defaultNAEPScoreCard()) {
  # all items
  itemColsClean <- tolower(c(polyItems, dichotItems))
  # fr2 file structure
  t <- try(mrcFile <- readLines(filename), silent=TRUE)

  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=T)
  mrcFile <- unlist(mrcFile)

  specs <- getNAEP_FR2Specs(mrcFile) #in readNAEP.R file to get FR2 info
  # read in the variables from the file, this is based on the information ETS
  # shared with us
  variableName <- tolower(specs$variableName) # name of the variable

  # indices/lines of question item variables
  indices <-  match(itemColsClean, variableName)
  indices <- indices[!is.na(indices)]
  itemLines <- mrcFile[indices]

  # initialize empty dataframe to build
  scoreCard <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(scoreCard) <- c('key', 'answer', 'score')

  # codes in EdSurvey NAEP data

  # items with scores that have been adjusted
  adjustedItems <- adjustedData$NAEPid

  # get item key, answers, and points per item
  for (i in indices) {
    
    line <- mrcFile[i]
    # initialize portion of scorecard for item
    subScoreCard <- data.frame(matrix(ncol = 4, nrow = 12))
    colnames(subScoreCard) <- c("key", "answer", "score", "scorePoints")

    # get key (question id)
    itemId <- variableName[i]

    # get points awarded by splitting string (like 0010)
    points <- specs$scoreKey[i]

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
      #scorePoints will be the maxium value as defined by the 'scoring key'
      scorePoints <- max(as.numeric(points))
      points <- replace(NA * c(1:12), c(1:length(points)), points) #make it length 12 filled with NAs where applicable
    } else {
      points <- NA * c(1:12)
    }
    
    #parse the value labels from the .FR2 specification
    tokens <- strsplit(specs$labelValues[i], "^", fixed = TRUE)[[1]]
    
    vals <- as.numeric(NA * c(1:12)) #create numeric vector of length 12, (not ideal, but to be consistent)
    labels <- as.character(NA * c(1:12))
    
    for(ii in seq_along(tokens)){#use 1:12 here to keep consistent with sizing (yes, a bit strange)
      vals[ii] <- as.numeric(strsplit(tokens[ii], "=", fixed = TRUE)[[1]][1])
      labels[ii] <- paste0(strsplit(tokens[ii], "=", fixed = TRUE)[[1]][-1], collapse = "=")
    }
    
    # set points for other types of answers (like omitted, illegible, etc.)
    if (itemId %in% dichotItems) {
      # this is a multiple choice question
      for (l in 1:length(labels)) {
        # find the corresponding point score in dict
        newScore <- scoreDict$pointMult[match(tolower(labels[l]), tolower(scoreDict$resCat))]
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
    #set the score points, exclude NAs and
    subScoreCard$scorePoints <- scorePoints

    scoreCard <- rbind(scoreCard, subScoreCard)
  }

  # reconfigure
  scoreCard$score <- as.numeric(scoreCard$score)
  scoreCard <- scoreCard[complete.cases(scoreCard),] # take out rows with NA ScorePoints
  colnames(scoreCard) <- c("key", "answer", "score", "scorePoints")

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

#' @export
defaultNAEPScoreCard <- function() {
  scoreDict <- data.frame(resCat=c("Multiple", "Not Reached", "Missing", "Omitted", "Illegible", "Non-Rateable", "Off Task"),
                           pointMult=c(8, NA, NA, 8, 0, 0, 0),
                           pointConst=c(0, NA, NA, 0, 0, 0, 0))
  return(scoreDict)
}



#' @title set NAEP Score Card
#' @description add item response theory data necessary to use \code{mml.sdf} on NAEP data
#' @param data a NAEP \code{edsurvey.data.frame}
#' @param dctPath a file location that points to the location of a NAEP \code{.dct} file (usually in the \code{AM} folder). A \code{.dct} file can be
#'                used to input custom item response theory (IRT)
#'                parameters and subscale/subtest weights for NAEP assessments compared with those provided in the \code{NAEPirtparams} package. 
#'                
#' @return a NAEP \code{edsurvey.data.frame} with updated attributes
#' @example \man\examples\setNAEPScoreCard.R
#' @export
setNAEPScoreCard <- function(data, dctPath=NULL) {

  # check if we can continue
  if (is.null(dctPath)) {
    stop("You must provide dctPath.")
  }

  # get IRT params from dct
  allTables <- parseNAEPdct(dctPath)
  polyParamTab <- allTables$polyParamTab
  dichotParamTab <- allTables$dichotParamTab
  testDat <- allTables$testDat
  adjustments <- data.frame()
  # get the scoreCard
  scoreCard <- getNAEPScoreCard(getAttributes(data, "fr2Path"),
                                polyParamTab$ItemID,
                                dichotParamTab$ItemID,
                                adjustments)
  # update dichotparamtab: 1/k for missing value

  dscoreCard <- scoreCard[scoreCard$score %in% c(0,1) &
                          nchar(trimws(gsub("[*]", "", gsub("^[A-G]", "", scoreCard$answer)))) == 0, ]
  Ks <- aggregate(answer ~ key, dscoreCard, length)
  Ks$answer <- 1 / Ks$answer
  dichotParamTab <- merge(dichotParamTab, Ks, by.x = 'ItemID', by.y = 'key', all.x = TRUE, all.y=FALSE)
  # if it is not in this lookup table, it is constructed response and so the missing value should be zero
  dichotParamTab$missingValue <- ifelse(is.na(dichotParamTab$answer), 0, dichotParamTab$answer)
  dichotParamTab$answer <- NULL

  # set attribute
  data <- setAttributes(data, "dichotParamTab", dichotParamTab)
  data <- setAttributes(data, "polyParamTab", polyParamTab)
  data <- setAttributes(data, "testData", testDat)
  data <- setAttributes(data, "adjustedData", adjustments)
  data <- setAttributes(data, "scoreDict", scoreCard)
  data <- setAttributes(data, "scoreFunction", EdSurvey::scoreDefault)
  return(data)
}
