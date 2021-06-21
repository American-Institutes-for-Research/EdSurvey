#' @title Connect to Education Longitudinal Study (ELS:2002) Data
#'
#' @description Opens a connection to an ELS data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the directory path of the extracted set of
#'             data files and layout files.
#' @param filename a character value of the name of the SPSS (.sav) data file
#'                 in the specified \code{path} to be read.
#' @param wgtFilename a character value of the name of the associated balanced
#'                    repeated replication (BRR) weight SPSS (.sav) data file
#'                    in the specified \code{path} to be read. This argument
#'                    is applicable only for the student-level data, which
#'                    contains a separate data file containing the weight
#'                    replicate information. If using default filenames
#'                    (recommended), then you shouldn't need to specify
#'                    this parameter because it will inspect the \code{filename}
#'                    argument. For data files with no BRR weight file
#'                    associated, specify a value of \code{NULL} or \code{NA}.
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the read
#'                    function by using existing read-in data already processed.
#'                    
#' @param verbose a logical value that will determine if you want verbose output
#'                while the \code{readELS} function is running to indicate
#'                processing progress.
#'                The default value is \code{TRUE}.
#' @details
#' Reads in the unzipped files downloaded from the ELS longitudinal dataset(s)
#' to an \code{edsurvey.data.frame}.  The ELS 2002 study consisted of
#' four distinct separate datasets that cannot be combined:
#' \itemize{
#'   \item{Student: bas -year through follow-up three (default)}
#'   \item{School: base year through follow-up one}
#'   \item{Institution: follow-up two}
#'   \item{Institution: follow-up three}
#' }
#'
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the ELS longitudinal dataset
#'
#' @importFrom haven read_sav
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, \code{\link{getData}}, and \code{\link{downloadECLS_K}}
#' @author Tom Fink
#' @example /man/examples/readELS.R
#' @export
readELS <- function(path = getwd(),
                    filename = "els_02_12_byf3pststu_v1_0.sav",
                    wgtFilename = ifelse(filename=="els_02_12_byf3pststu_v1_0.sav","els_02_12_byf3stubrr_v1_0.sav",NA),
                    forceReread = FALSE,
                    verbose = TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  
  #setup file list to work with
  fileList <- list(dataFile=unlist(file.path(path, filename))[1])
  
  #validate files::get the filecount to see if we have any missing or excess files
  validateData <- sapply(fileList$dataFile, file.exists)
  layoutData <- sapply(fileList$layoutFile, file.exists)
  
  if(!all(validateData==TRUE)){
    missingVars <- names(validateData==TRUE)
    if(length(missingVars)>0){
      stop(paste0("Cannot find specified data file ", sQuote(missingVars), " in path ", sQuote(path), "."))
    }
  }
  if(!all(layoutData==TRUE)){
    missingVars <- names(layoutData==TRUE)
    
    if(length(missingVars)>0){
      stop(paste0("Cannot find specified layout file ", sQuote(missingVars), " in path ", sQuote(path), "."))
    }
  }
  
  #convert NULL filename to NA to limit verbose warning output
  if(is.null(wgtFilename)){
    wgtFilename <- NA
  }
  
  #test and validate weighting files if they are specified
  if(!is.na(wgtFilename)){
    fileList$dataFileWgt <- unlist(file.path(path, wgtFilename))[1]
  
    #validate files::get the filecount to see if we have any missing or excess files
    validateData <- sapply(fileList$dataFileWgt, file.exists)
    layoutData <- sapply(fileList$layoutFileWgt, file.exists)
    
    if(!all(validateData==TRUE)){
      missingVars <- names(validateData==TRUE)
      if(length(missingVars)>0){
        stop(paste0("Cannot find specified weight data file ", sQuote(missingVars), " in path ", sQuote(path), "."))
      }
    }
    if(!all(layoutData==TRUE)){
      missingVars <- names(layoutData==TRUE)
      
      if(length(missingVars)>0){
        stop(paste0("Cannot find specified weight layout file ", sQuote(missingVars), " in path ", sQuote(path), "."))
      }
    }
  } #end if(!is.na(wgtFilename))
  
  cacheInfo <- list(cacheFilepath = gsub("\\.sav$", "\\.txt", fileList$dataFile, ignore.case = TRUE),
                    cacheFilepathWgt = gsub("\\.sav$", "\\.txt", fileList$dataFileWgt, ignore.case = TRUE),
                    cacheMetaFilepath = gsub("\\.sav$", "\\.meta", fileList$dataFile, ignore.case = TRUE))
  
  processArgs <- list(files = fileList,
                      cacheFileInfo = cacheInfo,
                      forceReread = forceReread,
                      verbose = verbose)

  retryProc <- tryCatch({processedData <- do.call("processELS", processArgs, quote = TRUE)
                          FALSE
                        }, error = function(e){
                          TRUE #flag to retry
                        }, warning = function(w){
                          TRUE #flag to retry
                        })
  
  if (retryProc){
    processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
    processedData <- tryCatch(do.call("processELS", processArgs, quote = TRUE),
                              error = function(e){
                                stop(paste0("Unable to process ELS data. Possible file corruption with source data.",
                                            " Error message: ", e))
                              })
  }
  
  weights <- buildELSWeightList(processedData$fileFormat$wgt)
  if(!is.null(weights)){
    attr(weights, "default") <- "" #no default weight
  }
  
  pvs <- list() #no plausible values or achievement levels?
  
  olLower <- c("Suppressed", 
               "Survey component legitimate skip/NA", 
               "Item legitimate skip/NA", 
               "Not administered; abbreviated interview or breakoff",
               "Missing one or Both assessment scores", 
               "Multiple response","Nonrespondent", 
               "Partial interview-breakoff", 
               "Refused", 
               "Don't know", 
               "Nonrespondent",
               "Out of range",
               "Missing")
  olUpper <- toupper(olLower)
  
  omittedLevels <- c(olLower, olUpper,
                     NA,
                     "(Missing)")
  
  #cleaup any zero label
  processedData$fileFormat$data <- valueLabelCleanupFF(processedData$fileFormat$data, omittedLevels, c("Zero", "zero", "ZERO")) #cleanup
  
  #remove any value labels for psu and strat_id variables
  processedData$fileFormat$data <- updateELS_PSUValueLabel(processedData$fileFormat$data, "psu")
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildELS_dataList(processedData$data$data, 
                                                   processedData$fileFormat$data,
                                                   processedData$data$wgt,
                                                   processedData$fileFormat$wgt),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2002",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL,
                      omittedLevels = omittedLevels,
                      survey = "ELS",
                      country = "USA",
                      psuVar = "psu",
                      stratumVar = "strat_id",
                      jkSumMultiplier = 0.005, #1/200 replicates for BRR
                      validateFactorLabels = TRUE,
                      reqDecimalConversion = FALSE) #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
}


processELS <- function (files,
                        cacheFileInfo,
                        forceReread,
                        verbose) {
  
  runProcessing <- TRUE #set default value
  hasWeightFile <- !is.null(files$dataFileWgt) #create wgt file flag
  fileFormatWgt <- NULL
  
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFileInfo$cacheFilepath)){
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      
      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "ELS")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat$data
        
        if(hasWeightFile){
          fileFormatWgt <- cacheRDS$fileFormat$wgt
          
          if(is.na(fileFormatWgt) || is.null(fileFormatWgt)){ #user previously ran the file without specifying weights and we must re-cache with weights
            runProcessing <- TRUE
          }
        }
      }
    }
  }
  
  #force reprocess if called for
  if(forceReread){
    runProcessing <- TRUE
  }
  
  if(runProcessing){
    #first delete the existing cache file if it exists in case the processing errors then it won't pickup the cache file
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      file.remove(cacheFileInfo$cacheMetaFilepath)
    }
    
    #get fileFormat from the SPSS file here
    if(verbose){
      eout(paste0("Reading SPSS file data."))
    }
    
    spssDF <- read_sav(files$dataFile, user_na = TRUE)
    spssDF <- UnclassCols(spssDF)
    
    if(verbose){
      eout(paste0("Reading SPSS file data."))
    }
    
    fileFormat <- getSPSSFileFormat(spssDF)
    fileFormat <- writeDF_FWF(spssDF, fileFormat, cacheFileInfo$cacheFilepath, verbose)
    fileFormat <- identifyELSWeights(fileFormat)
    
    #build the file format of the weight file if exists
    fileFormatWgt <- NULL
    if(hasWeightFile){
      spssDFWgt <- read_sav(files$dataFileWgt, user_na = TRUE)
      spssDFWgt <- UnclassCols(spssDFWgt)
      
      fileFormatWgt <- getSPSSFileFormat(spssDFWgt)
      fileFormatWgt <- writeDF_FWF(spssDFWgt, fileFormatWgt, cacheFileInfo$cacheFilepathWgt, verbose)
      fileFormatWgt <- identifyELSWeights(fileFormatWgt)
    }
      

    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=list(data = fileFormat, wgt = fileFormatWgt))
    
    saveRDS(cacheFile, cacheFileInfo$cacheMetaFilepath)
    
  }else{ #if(runProcessing==TRUE)
    
    if(verbose==TRUE){
      cat(paste0("Found cached data for file ", dQuote(files$dataFile),"\n"))
    }
  }#end if(runProcessing==TRUE) 
  
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)
  
  #ensure LaF throws no errors by reading first line
  tryCatch(dataLAF[1,],
           error=function(e) {
             close(dataLAF)
             stop(e)
           })
  
  dataLAFWgt <- NULL
  if(!is.null(fileFormatWgt)){
    dataLAFWgt <- laf_open_fwf(cacheFileInfo$cacheFilepathWgt, column_types = fileFormatWgt$dataType, column_widths = fileFormatWgt$Width, column_names = fileFormatWgt$variableName)
    
    #ensure LaF throws no errors by reading first line
    tryCatch(dataLAFWgt[1,],
             error=function(e) {
               close(dataLAFWgt)
               stop(e)
             })
  }
  #do caching and testing
  return(list(data = list(data = dataLAF, wgt = dataLAFWgt),
              fileFormat = list(data = fileFormat, wgt = fileFormatWgt)))
}

#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyELSWeights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  wgtVars <- grep("^(by|f1|f2|f3|ps).*wt$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  #for some reason there is a label of '0=Zero' defined for continuous weight vars.  this removes that value label
  #retain the label if it's 'suppressed' or a true omitted level
  fileFormat$labelValues[fileFormat$weights==TRUE] <- gsub("0=Zero", "", fileFormat$labelValues[fileFormat$weights==TRUE], ignore.case = TRUE)
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildELSWeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  #build lookup for the replicate weight variable name format as the formats vary by weight
  repWgtLookup <- data.frame(wgtVar = c("bystuwt",
                                        "f1qwt",
                                        "f1pnlwt",
                                        "f1trscwt",
                                        "f2qwt",
                                        "f2qtscwt",
                                        "f2bywt",
                                        "f2f1wt",
                                        "f3qwt",
                                        "f3bypnlwt",
                                        "f3f1pnlwt",
                                        "f3qtscwt",
                                        "f3bytscwt",
                                        "f3f1tscwt",
                                        "pswt",
                                        "f3bypnlpswt",
                                        "f3bytscpswt",
                                        "f3f1pnlpswt",
                                        "f3f1tscpswt",
                                        "f3qpswt",
                                        "f3qtscpswt",
                                        "pstscwt",
                                        "byexpwt", #restricted dataset only
                                        "f1expwt", #restricted dataset only
                                        "f1xpnlwt",#restricted dataset only
                                        "byschwt"), #byf1 school data file weight
                             baseVar = c("bystu",
                                           "f1q",
                                           "f1pnl",
                                           "f1trs",
                                           "f2q",
                                           "f2trs",
                                           "f2byp",
                                           "f2f1p",
                                           "f3q",
                                           "f3byp",
                                           "f3f1p",
                                           "f3trs",
                                           "f3byt",
                                           "f3f1t",
                                           "pswt",
                                           "f3bypnlpswt",
                                           "f3bytscpswt",
                                           "f3f1pnlpswt",
                                           "f3f1tscpswt",
                                           "f3qpswt",
                                           "f3qtscpswt",
                                           "pstscwt",
                                           "byexp", #restricted only; guessing here::need to confirm
                                           "f1exp", #restricted only; guessing here::need to confirm
                                           "f1xpnl", #restricted only; guessing here::need to confirm
                                           "bysch"),  #byf1 school data file weight
                             stringsAsFactors = FALSE)
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
    baseWgtVar <- repWgtLookup[repWgtLookup$wgtVar==tempVar, "baseVar", drop=TRUE] #only the base of the weight
    
    
    if(length(baseWgtVar)==0){
      next #we don't have this weight defined in the lookup table
    }
    
    wgtPattern = paste0("^", baseWgtVar, "\\d+$")
    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
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

buildELS_dataList <- function(dataLaf, dataFF, wgtLaf, wgtFF){
  
  dataList <- list()
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(lafObject = dataLaf,
                                     fileFormat = dataFF,
                                     levelLabel = "Data",
                                     forceMerge = TRUE,
                                     parentMergeLevels = NULL,
                                     parentMergeVars = NULL,
                                     mergeVars = NULL,
                                     ignoreVars = NULL,
                                     isDimLevel = TRUE)
  
  if(!is.null(wgtFF)){
    dataList[["Weight"]] <- dataListItem(lafObject = wgtLaf,
                                         fileFormat = wgtFF,
                                         levelLabel = "Weight",
                                         forceMerge = FALSE,
                                         parentMergeLevels = c("Data"),
                                         parentMergeVars = c("stu_id"),
                                         mergeVars = c("stu_id"),
                                         ignoreVars = wgtFF$variableName[wgtFF$variableName %in% dataFF$variableName],
                                         isDimLevel = FALSE)
  }
    
  
  return(dataList)
}

#this function will remove any value labels greater than or equal than a 'key' value of 0.
#will retain any omitted label definitions as those are handled by 'getData' to remove them properly
#PSU or Stratum variables cannot be factors/lfactors for analysis functions since it makes them non-comparible
#see issue #1419 for details
updateELS_PSUValueLabel <- function(fileFormat, psuVarName){
  
  ff <- subset(fileFormat, fileFormat$variableName %in% psuVarName)
  
  if(is.null(ff) || nrow(ff)==0){
    return(fileFormat)
  }
  
  for(i in 1:nrow(ff)){
    
    loopLV <- ff$labelValues[[i]]
    
    if(is.null(loopLV) || nchar(loopLV)==0){
      next
    }
    
    x <- strsplit(loopLV,"^", fixed = TRUE)
    y <- sapply(x, function(z){strsplit(z, "=", fixed=TRUE)})
    key <- sapply(y, function(z){z[1]})
    lbl <- sapply(y, function(z){paste(z[-1], sep="", collapse = "=")})
    
    rmvIdx <- which(key >= 0, arr.ind = TRUE) #need to retain any omitted levels for example: "-8=SURVEY COMPONENT LEGITIMATE SKIP/NA"
    
    key <- key[-rmvIdx]
    lbl <- lbl[-rmvIdx]
    
    #apply this back to the fileFormat for returning
    fileFormat$labelValues[fileFormat$variableName==ff$variableName[[i]]] <- paste(key, lbl, sep="=", collapse="^")
  }
  
  return(fileFormat)
}