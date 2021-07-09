#' @title Beginning Postsecondary Students (BPS:04/09) Data
#'
#' @description Opens a connection to a BPS 2004 to 2009 cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the BPS extracted data files
#' @param csvFilename a character value of the derived data file (.csv) containing the raw BPS:04/09 data
#' @param formatFilename a character value of the format file (.txt) that describes the layout of the \code{csvFilename}
#' @param metadataFilename a character value of the metadata file (.txt) that describes additional metadata of the \code{csvFilename}
#' @details Reads in the unzipped derived data files from the BPS longitudinal database of the BPS 2004 to 2009 cohort.
#' 
#' @return An \code{edsurvey.data.frame} for the BPS longitudinal dataset.
#'
#' @seealso \code{\link{readBB_2012}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example man/examples/readBPS_2009.R
#' 
readBPS_2009 <- function(path = getwd(),
                         csvFilename = "bps09derived_datafile.csv",
                         formatFilename = "bps09derived_format.txt",
                         metadataFilename = "bps09derived_metadata.txt") {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl=TRUE, ignore.case=TRUE), dirname(path), path)
  
  if(!dir.exists(path)){
    stop(paste0("Cannot find specified folder path ", sQuote(path), "."))
  }
  if(length(path)!=1){
    stop(paste0("The ", sQuote("path"), " argument must be a length of one."))
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
  
  fileFormat <- identifyBPSWeights_2009(fileFormat)
  
  weights <- buildBPSWeightList_2009(fileFormat)
  if(!is.null(weights)){
    attr(weights, "default") <- "wta000"
  }
  
  pvs <- list() #no plausible values or achievement levels?
  
  omittedLevels <- getOmittedLevels(fileFormat) #defined in BPS 2014 R file
  
  dataList <- buildBPSDataList_2009(lafObj, fileFormat)
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2009",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "BPS",
                      country = "USA",
                      psuVar = NA, #psuVar==bps09psu; stratumVar==bps09str; IT SHOULD BE THESE VARIABLES ACCORDING TO DOCUMENTATION FOR TAYLOR; BUT NOT IN DERIVED FILE?  
                      stratumVar = NA, 
                      jkSumMultiplier = 1/200, #1/200 BRR replicates
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBPSWeights_2009 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #weight vars all begin with wta
  wgtVars <- grep("^wt(a|b|c|d)000$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBPSWeightList_2009 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(w in wgtVars){
    tempVar <- w #full variable name of the weight var
    baseWgtVar <- substr(tempVar, 1, 3) #strip off the 000 from the end
    
    if(length(baseWgtVar)==0){
      next #we don't have this weight defined in the lookup table
    }
    
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
  }

  return(weights)
}

buildBPSDataList_2009 <- function(lafObj, fileFormat){
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