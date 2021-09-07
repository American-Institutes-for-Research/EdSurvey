#' @title Connect to NHES Survey Data
#'
#' @description Opens a connection to a National Household Education Survey (NHES) data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param savFiles a character vector to the full file path(s) to the NHES
#'             extracted SPSS (*.sav) data files.
#' @param surveyCode a character vector of the \code{surveyCode} to identify the year and survey type of the passed \code{savFiles} data file(s).
#'                   The default value is set to \code{auto} which attempts to automatically identify the survey/year based on the file attributes.
#'                   Occasionally, the \code{auto} lookup may be unable to determine the \code{surveyCode} and must be explicitly set by the user.
#'                   The lengths of the \code{savFiles} vector and \code{surveyCode} vector must match, unless \code{surveyCode} is set to \code{auto}.
#'                   To view the \code{surveyCodes} available, use the \code{getNHES_SurveyInfo}, or \code{viewNHES_SurveyCodes} function to view the codes.
#' @param forceReread a logical value to force a rereading of all processed data.
#'                    The default value of \code{FALSE} speeds up the
#'                    \code{readNHES} function by using existing read-in data if already processed.
#' @param verbose a logical value that defaults to \code{TRUE} for verbose console output that indicates progress information.  
#'                If \code{verbose = FALSE}, no information will be printed.
#' 
#' @details
#' Reads in the unzipped public-use files downloaded from the NCES Online Codebook (\url{https://nces.ed.gov/OnlineCodebook}) in SPSS (*.sav) format.
#' Other sources of NHES data, such as restricted-use files or other websites, may require additional conversion steps to generate the required SPSS data format
#' and/or explicitly setting the \code{surveyCode} parameter.
#' 
#' @return an \code{edsurvey.data.frame} if only one NHES file is specified for the \code{savFiles} argument,
#' or an \code{edsurvey.data.frame.list} if multiple files are passed to the \code{savFiles} argument
#'
#' @seealso \code{\link{downloadNHES}}, \code{\link{getNHES_SurveyInfo}}, and \code{\link{viewNHES_SurveyCodes}}
#' @author Tom Fink
#' @example man/examples/readNHES.R
#' @importFrom haven read_sav
#' @importFrom tools md5sum
#' @export
readNHES <- function(savFiles,
                     surveyCode = "auto",
                     forceReread = FALSE,
                     verbose = TRUE){

  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  userOp2 <- options(scipen = 999)
  on.exit(options(userOp2), add = TRUE)
  
  if(missing(savFiles) || length(savFiles)==0){
    stop(paste0("The argument ", dQuote("savFiles"), " must be specified."))
  }
  
  #validate the folder path(s) the user specified to make sure they exist
  savFiles <- suppressWarnings(normalizePath(unique(savFiles), winslash = "/"))
  
  if(!all(file.exists(savFiles))){
    stop(paste0("Unable to locate ", dQuote("savFiles"), " file(s): ", pasteItems(dQuote(savFiles[!file.exists(savFiles)]))))
  }

  #meta-data about the surveys that is necessary for using with EdSurvey
  fileLookupInfo <- readRDS(system.file("extdata", "NHES_FileLookup.rds", package="EdSurvey"))
  
  #check if using 'auto' mode or specifying survey codes and validate them
  surveyCode = tolower(surveyCode)
  
  if(surveyCode[1]=="auto" || missing(surveyCode) || is.null(surveyCode) || is.na(surveyCode)){
    autoSearch <- TRUE
  }else{
    autoSearch <- FALSE
    
    if(length(savFiles)!=length(surveyCode)){
      stop(paste0("The lengths of ", dQuote("savFiles"), " and ", dQuote("surveyCode"), " must be equal."))
    }
    
    surveyCode <- toupper(surveyCode)
    surveyCode <- match.arg(surveyCode, fileLookupInfo$SurveyCode, several.ok = TRUE) #will throw error here if unmatched to list, maybe custom handler here?
  }
  
  returnList <- vector(mode = "list", length = length(savFiles))

  for(iFile in seq_along(savFiles)){

    if(autoSearch){
      calcMD5 <- tools::md5sum(savFiles[iFile])
      
      regexFileMatch <- sapply(fileLookupInfo$FileMatchRegex, function(testRegex){
        grepl(testRegex, basename(savFiles[iFile]), ignore.case = TRUE)
      })
      
      fileInfoSub <- subset(fileLookupInfo, regexFileMatch | fileLookupInfo$md5_hash==calcMD5)
      
      if(nrow(fileInfoSub)==0){
        stop(paste0("Unable to locate background information to correctly build survey information for analysis for the provided file: ",
                       dQuote(savFiles[iFile]), ". Please try explicitly setting the ", dQuote("surveyCode"), " argument for this file."))
      }else if(nrow(fileInfoSub)>1){
        warning(paste0("Excess file information found for file: ", dQuote(savFiles[iFile]), ". Using the first found record.\n 
                       Try setting the ", dQuote("surveyCode"), " argument to ensure you have the correct survey specified."))
        fileInfoSub <- fileInfoSub[1, ]
      }
    }else{ #no auto search
      fileInfoSub <- subset(fileLookupInfo, fileLookupInfo$SurveyCode==surveyCode[iFile])
      
      if(nrow(fileInfoSub)!=1){
        stop(paste0("Error parsing SurveyCode (", surveyCode[iFile], ") for data file: ", dQuote(savFiles[iFile])))
      }
    }

    #uses double pipes (||) as delimiter in case of multiple weight specifications
    wgtVars <- fileInfoSub$WeightVars[1]
    wgtVars <- unlist(strsplit(wgtVars, "||", fixed = TRUE))

    wgtRepRegex <- fileInfoSub$WeightRepRegex[1]
    wgtRepRegex <- unlist(strsplit(wgtRepRegex, "||", fixed = TRUE))

    procArgs <- list(savFile = savFiles[iFile],
                     weightVars = wgtVars,
                        forceReread = forceReread,
                        verbose = verbose)

    retryProc <- tryCatch({processedData <- do.call("processNHES", procArgs, quote = TRUE)
                            FALSE
                          }, error = function(e){
                            TRUE #flag to retry
                          }, warning = function(w){
                            TRUE #flag to retry
                          })

    if (retryProc){
      procArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
      eout("Cache corrupt, attempting to reread the data.")
      processedData <- tryCatch(do.call("processNHES", procArgs, quote = TRUE),
                                error = function(e){
                                  stop(paste0("Unable to process NHES data. Possible file corruption with source data.",
                                              " Error message: ", e))
                                })
    }

    wgtList <- buildNHESWeightList(processedData$fileFormat, wgtVars, wgtRepRegex)
    #set a default weight
    if(length(wgtList)>0){
      attr(wgtList, "default") <- names(wgtList)[1]
    }

    omitLvls <- unlist(strsplit(fileInfoSub$OmittedLevels, "||", fixed = TRUE))
    omitLvls <- c(omitLvls, NA, "NA", "(Missing)")

    returnList[[iFile]] <- edsurvey.data.frame(userConditions = list(),
                                                defaultConditions = NULL,
                                                dataList = buildNHESDataList(processedData$lafObj, processedData$fileFormat),
                                                weights = wgtList,
                                                pvvars = NULL,
                                                subject = fileInfoSub$Description,
                                                year = fileInfoSub$Year,
                                                assessmentCode = "Cross Sectional",
                                                dataType = "Survey Data",
                                                gradeLevel = "",
                                                achievementLevels = NULL, #no achievement levels
                                                omittedLevels = omitLvls,
                                                survey = "NHES",
                                                country = "USA",
                                                psuVar = fileInfoSub$PSU_Var, 
                                                stratumVar = fileInfoSub$Stratum_Var, 
                                                jkSumMultiplier = fileInfoSub$Jackknife_Multiplier, 
                                                validateFactorLabels = TRUE,
                                                reqDecimalConversion = FALSE)

  }#end for(iFile in seq_along(savFiles))

  if(length(returnList)>1){
    return(edsurvey.data.frame.list(returnList))
  }else{
    return(returnList[[1]])
  }
}

#processes a single datFile and single layoutFile as a singular unit and returns a list of all the processed information to caller
processNHES <- function (savFile,
                         weightVars,
                         forceReread,
                         verbose) {

  reprocessData <- FALSE #default until checked
  cacheFileMeta <- gsub("\\.sav$", "\\.meta", savFile, ignore.case = TRUE)
  cacheFileName <- gsub("\\.sav$", "\\.txt", savFile, ignore.case = TRUE)

  #test here if the cache (either the .meta file, or the .txt fixed-width file)
  if(!file.exists(cacheFileMeta) || forceReread){
      reprocessData <- TRUE
  }else{ #cache .meta file exists, examine it further
    metaInfo <- readRDS(cacheFileMeta)
    fileFormat <- metaInfo$fileFormat

    if(cacheMetaReqUpdate(metaInfo$cacheFileVer, "NHES")){
      reprocessData <- TRUE
    }
  }

  if(!file.exists(cacheFileName)){
    reprocessData <- TRUE
  }

  if(reprocessData){
    spssData <- read_sav(savFile, user_na = TRUE)
    spssData <- UnclassCols(spssData)

    fileFormat <- getSPSSFileFormat(spssData)
    fileFormat <- writeDF_FWF(spssData, fileFormat, cacheFileName, verbose)

    #calculate TRUE/FALSE for the weights before the fileFormat is cached
    fileFormat$weights <- tolower(fileFormat$variableName) %in% tolower(weightVars)

    #write out .meta file
    metaFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                     cacheFileVer=1,
                     ts=Sys.time(),
                     fileFormat=fileFormat)

    saveRDS(metaFile, cacheFileMeta)
  }else{
    if(verbose){
      eout(paste0("Cached data found for file: ", dQuote(savFile)))
    }
  }

  lafObj <- laf_open_fwf(cacheFileName, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)

  #ensure no LaF errors are present, if so re-throw that error
  tryCatch(lafObj[1,],
           error=function(e) {
             close(lafObj)
             stop(e)
           })

  return(list(lafObj = lafObj,
              fileFormat = fileFormat))
}


#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
#the wgtVars and the associated wgtRepRegex should be in the same vector index position to correctly identify the replicate variables by name
buildNHESWeightList <- function(fileFormat, wgtVars, wgtRepRegex){
  #no wgts found
  if(length(wgtVars)==0 || is.null(wgtVars)){
    return(NULL)
  }

  wgtList <- vector(mode = "list", length = length(wgtVars)) #build list to return

  for(i in seq_along(wgtVars)){
    repVars <- grep(wgtRepRegex[i], fileFormat$variableName, ignore.case = TRUE, value = TRUE)

    if(length(repVars)>0 && !is.null(repVars)){

      base <- unique(gsub("\\d{1,2}$", "", repVars))
      reps <- unique(gsub(base, "", repVars))

      tempWgt <- list(jkbase=tolower(base), jksuffixes=as.character(reps))

      wgtList[[i]] <- tempWgt
      tempWgt <- NULL
    }
  }

  names(wgtList) <- wgtVars
  return(wgtList)
}

buildNHESDataList <- function(dataLaF, fileFormat){

  dataList <- list()

  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(lafObject = dataLaF,
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

#' @title Get NHES Survey Code Definitions and Survey Meta-data
#' @description This function returns a \code{data.frame} object that defines NHES Survey Codes and survey parameters that are compatible with the \code{readNHES} function for use.
#'              The resulting \code{data.frame} object is useful for user reference or other advanced techniques.
#'              
#' @note Any changes or modifications to the \code{data.frame} object will not change the behavior of \code{readNHES}.  
#'       This function should be treated only as a read-only source of information.
#'              
#' @author Tom Fink
#' @seealso \code{readNHES}, \code{viewNHES_SurveyCodes}
#' @example man/examples/getNHES_SurveyInfo.R
#' @export
getNHES_SurveyInfo <- function(){
  x <- readRDS(system.file("extdata", "NHES_FileLookup.rds", package="EdSurvey")) #data.frame here
  return(x)
}

#' @title View NHES Survey Code Definitions
#' @description This function prints the defined NHES Survey Codes to console output that are compatible with the \code{readNHES} function for use.
#'              Typically a user will only need to manually set these codes if the 'auto' survey parameter is not able to correctly identify the
#'              correct survey type, or for other unusual situations.
#'             
#' @author Tom Fink
#' @seealso \code{readNHES}, \code{getNHES_SurveyInfo}
#' @example man/examples/viewNHES_SurveyCodes.R
#' @export
viewNHES_SurveyCodes <- function(){

  x <- readRDS(system.file("extdata", "NHES_FileLookup.rds", package="EdSurvey"))
  
  fwf1 <- max(nchar(x$SurveyCode)) + 5
  fwf2 <- max(nchar(x$Year)) + 5
  fwf3 <- max(nchar(x$Description)) + 0
  
  fmtSurveyCode <- format(as.character(x$SurveyCode), justify = "left", width = fwf1)
  fmtYear <- format(as.character(x$Year), justify = "left", width = fwf2)
  fmtDesc <- format(as.character(x$Description), justify = "left", width = fwf3)
  
  printDF <- data.frame("SurveyCode" = fmtSurveyCode,
                        "Year" = fmtYear,
                        "Description" = fmtDesc)
  
  
  eout("NHES Survey Code Information for readNHES function.\n")
  print(printDF, right=FALSE)
  
  return(invisible(NULL))
}