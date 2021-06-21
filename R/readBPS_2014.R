#' @title Beginning Postsecondary Students (BPS:12/14) Data
#'
#' @description Opens a connection to a BPS 2012 to 2014 cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the BPS extracted data files
#' @param csvFilename a character value of the derived data file (.csv) containing the raw BPS:12/14 data
#' @param formatFilename a character value of the format file (.txt) that describes the layout of the \code{csvFilename}
#' @param metadataFilename a character value of the metadata file (.txt) that describes additional metadata of the \code{csvFilename}
#' @details Reads in the unzipped files downloaded from the BPS 2012 to 2014 cohort longitudinal database.
#' 
#' @return An \code{edsurvey.data.frame} for the BPS longitudinal dataset.
#'
#' @seealso \code{\link{readBB_2012}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Sun-joo Lee
#' @example man/examples/readBPS_2014.R
#' 
readBPS_2014 <- function(path = getwd(),
                          csvFilename = "bps14derived_datafile.csv",
                          formatFilename = "bps14derived_format.txt",
                          metadataFilename = "bps14derived_metadata.txt") {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  
  if(!dir.exists(path)){
    stop(paste0("Cannot find specified folder path ", sQuote(path), "."))
  }
  if(!file.exists(file.path(path, csvFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("csvFilename"), " in path ", sQuote(file.path(path, csvFilename)), "."))
  }
  if(!file.exists(file.path(path, formatFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("formatFilename"), " in path ", sQuote(file.path(path, formatFilename)), "."))
  }
  if(!file.exists(file.path(path, metadataFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("metadataFilename"), " in path ", sQuote(file.path(path, metadataFilename)), "."))
  }
  
  fileFormat <- getMetaFormatDictionary(file.path(path, metadataFilename), file.path(path, formatFilename)) #get the file format based on the fileformat file and metadata file
  lafObj <- laf_open_csv(file.path(path, csvFilename), fileFormat$dataType, fileFormat$variableName, skip=1) #ensure to skip header row
  
  fileFormat <- validateFileFormat_BPS(lafObj, fileFormat) #this checks to ensure the 'missings' are accurately applied to the fileformat as they are not explictly defined in the metadata file for some fields
  
  fileFormat <- identifyBPSWeights_2014(fileFormat)
  
  weights <- buildBPSWeightList_2014(fileFormat)
  attr(weights, "default") <- "wta000"

  pvs <- list() #no plausible values or achievement levels?
  
  omittedLevels <- getOmittedLevels(fileFormat)
  
  dataList <- buildBPSDataList_2014(lafObj, fileFormat)
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2014",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "BPS",
                      country = "USA",
                      psuVar = "bps14psu",  
                      stratumVar = "bps14str", 
                      jkSumMultiplier = 0.005, #1/200 replicate weights
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBPSWeights_2014 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #weight vars all begin with wta
  wgtVars <- grep("^wta", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBPSWeightList_2014 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  tempVar <- "wta000" #full variable name of the weight var
  baseWgtVar <- substr(tempVar, 1, 3) #strip off the 000 from the end
  
  wgtPattern = paste0("^", baseWgtVar, "\\d+$")
  ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
  ujkz <- ujkz[ujkz != tempVar] #remove the weight value itself from the replicates
  ujkz <- sub(baseWgtVar, "", ujkz, ignore.case = TRUE) #strip away and leave just the numeric variable name ending as a string
    
  if(length(ujkz)>0){
    tmpWgt <- list()
    tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz))
    names(tmpWgt)[[1]] <- tempVar
    weights <- c(weights,tmpWgt)
  }
  return(weights)
}

# Omitted levels are enclosed in "{}"
getOmittedLevels <- function(fileFormat) {
  
  omittedLevs <- unique(sapply(fileFormat$labelValues, function(x) regmatches(x[1], gregexpr("(?=\\{).*?(?<=\\})", x, perl=TRUE))[[1]]))
  omittedLevs <- unique(unlist(omittedLevs))
  omittedLevs <- append(omittedLevs, '(Missing)')
  omittedLevs <- append(omittedLevs, NA)
  
  return(omittedLevs)
}

buildBPSDataList_2014 <- function(lafObj, fileFormat){
  dataList <- list()
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(lafObject = lafObj,
                                     fileFormat = fileFormat,
                                     levelLabel = "Data",
                                     forceMerge = TRUE,
                                     parentMergeLevels = NULL,
                                     parentMergeVars = NULL,
                                     mergeVars = NULL,
                                     ignoreVars = NULL,
                                     isDimLevel = TRUE)
  
  return(dataList)
}

#in the metadata.txt file, it doesn't include all missing/omitted levels that get applied in the 'makeFile' scripts.  this emulates that functionality
#this function is applicable for BPS 2014, BPS 2009, and BPS 2001 as they all use the same definitions for these
validateFileFormat_BPS <- function(lafObj, fileFormat){
  
  tolerance <- 0.000001
  ff <- fileFormat
  
  #defines the values and labels that are defined in the 'makeFile' script that define the 'missing' values in the output files
  missingDef <- list(val=c(-1, -2, -3, -4, -5, -6, -7, -8, -9, -14),
                     lbl=c("{-1 Undefined}",
                           "{-2 Undefined}",
                           "{-3 Legitimate Skip}",
                           "{-4 Undefined}",
                           "{-5 Undefined}",
                           "{-6 Out of Range}",
                           "{-7 Resp never saw Question}",
                           "{-8 Undefined}",
                           "{-9 Missing}",
                           "{-14 Multiple values possible}"))
  
  loopFF <- subset(fileFormat, nchar(fileFormat$labelValues)==0 & fileFormat$dataType %in% c("integer", "numeric"))
  
  dat <- lafObj[,loopFF$variableName] #just do one read call here to get it in memory for speed
  
  for(varName in loopFF$variableName){
    xVal <- dat[,varName]
    keep <- rep(FALSE, length(missingDef$val))
    
    for(i in seq_along(missingDef$val)){
      if(any(abs(missingDef$val[i] - xVal) < tolerance)){ #strange bug involving -14 being identified with an %in% operator.  using a tolerance check here for floats to ensure matches
        keep[i] <- TRUE
      }
    }
    if(any(keep)){
      idx <- which(keep, arr.ind = TRUE)
      ff[ff$variableName==varName,"labelValues"] <- paste(missingDef$val[idx], missingDef$lbl[idx], collapse = "^", sep = "=") #make the updates
    }
  }#end for(varName in loopFF$variableName)
  
  return(ff)
}