#' @title Beginning Postsecondary Students (BPS:96/01) Data
#'
#' @description Opens a connection to a BPS 1996 to 2001 cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the BPS extracted data files
#' @param csvFilename a character value of the derived data file (.csv) containing the raw BPS:96/01 data
#' @param formatFilename a character value of the format file (.txt) that describes the layout of the \code{csvFilename}
#' @param metadataFilename a character value of the metadata file (.txt) that describes additional metadata of the \code{csvFilename}
#' @details Reads in the unzipped derived data files from the BPS longitudinal database of the BPS 1996 to 2001 cohort.
#' 
#' @return An \code{edsurvey.data.frame} for the BPS longitudinal dataset.
#'
#' @seealso \code{\link{readBB_2012}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example man/examples/readBPS_2001.R
#' 
readBPS_2001 <- function(path = getwd(),
                         csvFilename = "bps01derived_datafile.csv",
                         formatFilename = "bps01derived_format.txt",
                         metadataFilename = "bps01derived_metadata.txt") {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  
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
  
  fileFormat <- identifyBPSWeights_2001(fileFormat)
  
  weights <- buildBPSWeightList_2001(fileFormat)
  if(!is.null(weights)){
    attr(weights, "default") <- "wta000"
  }
  
  pvs <- list() #no plausible values or achievement levels?
  
  omittedLevels <- getOmittedLevels(fileFormat) #defined in BPS 2014 R file
  
  dataList <- buildBPSDataList_2001(lafObj, fileFormat)
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2001",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "BPS",
                      country = "USA",
                      psuVar = "b01arep",
                      stratumVar = "b01astr", 
                      jkSumMultiplier = 1/51, #1/51 BRR replicates
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBPSWeights_2001 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #weight vars all begin with wta
  wgtVars <- grep("^wt(a|b|c|d|e)000$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBPSWeightList_2001 <- function(fileFormat){
  
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

buildBPSDataList_2001 <- function(lafObj, fileFormat){
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
validateFileFormat_BPS2001 <- function(lafObj, fileFormat){
  
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
  
  loopFF <- subset(fileFormat, nchar(fileFormat$labelValues)==0)
  
  dat <- lafObj[,loopFF$variableName] #just do one read call here to get it in memory for speed
  
  for(varName in loopFF$variableName){
    
    xVal <- dat[,varName]
    
    if(any(missingDef$val %in% xVal)){
      idx <- which(missingDef$val %in% xVal, arr.ind = TRUE)
      ff[ff$variableName==varName,"labelValues"] <- paste(missingDef$val[idx], missingDef$lbl[idx], collapse = "^", sep = "=") #make the updates
    }
  }
  
  return(ff)
}