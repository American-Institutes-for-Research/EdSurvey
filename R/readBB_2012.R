#' @title Connect to B&B 2008--2012 Data
#'
#' @description Opens a connection to a Baccalaureate & Beyond 2008--2012 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the root directory path containing the \code{csvFilename}, \code{formatFilename}, and \code{metadataFilename} data files.
#' @param csvFilename a character value of the derived data file (.csv) containing the raw B&B 2008--2012 data.
#' @param formatFilename a character value of the format file (.txt) that describes the layout of the \code{csvFilename}.
#' @param metadataFilename a character value of the metadata file (.txt) that describes additional metadata of the \code{csvFilename}.
#'                    
#' @details Reads in the specified \code{csvFilename} file for the B&B 2008--2012 longitudinal survey to an \code{edsurvey.data.frame}.
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the B&B 2008--2012 longitudinal dataset.
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readBB_2012.R
#' 
readBB_2012 <- function(path,
                        csvFilename = "b12derived_datafile.csv",
                        formatFilename = "b12derived_format.txt",
                        metadataFilename = "b12derived_metadata.txt") {
  
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
  
  #add certain omitted levels to the fileFormat 'valueLabels' where the valueLabels is empty
  #the 'format' file used for labels doesn't define these, but the 'makeFile' programs add them in their coding (omitting the id field)
  #ensure these items are defined as omittedLevels otherwise it will mess up continuous variables being typed as categorical
  fileFormat$labelValues[nchar(fileFormat$labelValues)==0 & fileFormat$variableName!="id"] <- paste0("-3={Skipped}",
                                                                                                     "^-6={Out of range}",
                                                                                                     "^-7={Not administered - abbreviated}",
                                                                                                     "^-9={Missing}",
                                                                                                     "^-14={Multiple values possible}")
  
  
  lafObj <- laf_open_csv(file.path(path, csvFilename), fileFormat$dataType, fileFormat$variableName, skip=1) #ensure to skip header row
  
  fileFormat <- identifyBBWeights_2012(fileFormat)
  weights <- buildBBWeightList_2012(fileFormat)
  attr(weights, "default") <- "" #no default weight

  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("{Missing}", "{Not applicable}",
                     "{Skipped}", "{Did not enroll after bachelor's}",
                     "{Not administered - abbreviated}", "{No post-bachelor's enrollment}",
                     "{Instrument error}", "{Out of range}",
                     "{Not classified}", "{Independent student}", 
                     "{Multiple values possible}", "{Don't know}", 
                     "{No dependent children}", 
                     "{Independent student}", "{Dependent student}",
                     "(Missing)", NA)

  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildBBDataList_2012(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2008-2012",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "B&B2012",
                      country = "USA",
                      psuVar = NULL,  #psu and stratum are weight specific
                      stratumVar = NULL, 
                      jkSumMultiplier = 0.005, #1/200 replicates
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBBWeights_2012 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #
  wgtVars <- grep("^wt(a|b|c|d|e|f)000$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBBWeightList_2012 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
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
      
      if(grepl("^wt(a|b|c)000$", tempVar, ignore.case = TRUE)){
        tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz), psuVar="bb9analpsu", stratumVar="bb9analstr")
      }else{
        tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz), psuVar="bb12analpsu", stratumVar="bb12analstr")
      }
      
      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights,tmpWgt)
    }
  }
  
  return(weights)
}

buildBBDataList_2012 <- function(lafObj, fileFormat){
  
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
