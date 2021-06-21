#' @title Connect to NLS-72 Study Data
#'
#' @description Opens a connection to a National Longitudinal Study of the High
#'              School Class of 1972 (NLS-72) cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param NLS7286_PRI_FilePath a character value to the main study derived
#'                             analyticial data file (NLS7286_REV.PRI).
#'                             Located within the \code{REVISED_ASCII} folder.
#' @param NLS7286_SASSyntax_Path a character value to the SAS syntax file for
#'                               parsing the \code{NLS7286_REV.PRI} data file.
#'                               Located within the \code{SAS_EXTRACT_LOGIC} folder.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read
#'                    function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output
#'                while the \code{readNLS72} function is running to
#'                indicate processing progress.
#'                The default value is \code{TRUE}.
#'                    
#' @details Reads in the specified \code{NLS7286_SASSyntax_Path} file to parse
#'          the \code{NLS7286_PRI_FilePath} file.
#'          A cached data file and metadata file will be saved in the same
#'          directory and filename as the \code{NLS7286_PRI_FilePath} file,
#'          having new file extensions of .csv and .meta, respectively.  
#' 
#' @return
#' an \code{edsurvey.data.frame} for the National Longitudinal Study of the
#' High School Class of 1972 longitudinal dataset
#'
#' @seealso \code{\link{readHSB_Sophomore}}, \code{\link{readHSB_Senior}}, and \code{\link{getData}}
#' @importFrom utils write.table
#' @author Tom Fink
#' 
readNLS72 <- function(NLS7286_PRI_FilePath,
                      NLS7286_SASSyntax_Path,
                      forceReread=FALSE,
                      verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  NLS7286_PRI_FilePath <- suppressWarnings(normalizePath(unique(NLS7286_PRI_FilePath), winslash = "/"))
  NLS7286_SASSyntax_Path <- suppressWarnings(normalizePath(unique(NLS7286_SASSyntax_Path), winslash = "/"))
  
  if(!file.exists(NLS7286_PRI_FilePath)){
    stop(paste0("Cannot find specified data file ", sQuote("NLS7286_PRI_FilePath"), " in path ", sQuote(file.path(NLS7286_PRI_FilePath)), "."))
  }
  if(!file.exists(NLS7286_SASSyntax_Path)){
    stop(paste0("Cannot find specified data file ", sQuote("NLS7286_SASSyntax_Path"), " in path ", sQuote(file.path(NLS7286_SASSyntax_Path)), "."))
  }
  
  #prep the data cache items
  cacheFilename <- gsub("\\.pri$", ".csv", NLS7286_PRI_FilePath, ignore.case = TRUE)
  metaFilename <- gsub("\\.pri$", ".meta", NLS7286_PRI_FilePath, ignore.case = TRUE)
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFilename)){
    if(file.exists(metaFilename)){
      
      cacheRDS <- readRDS(metaFilename) #get the cache info from the .meta file
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "HS&B")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }
  
  #define omittedLevels for the clean step below
  omittedLevels <- c("{DONT KNOW}", "{BLANK}", "{LEGITIMATE SKIP}",
                     "{UNCODEABLE VERBATIM}", "{ILLEGITIMATE SKIP}",
                     "{PARTIAL RESPONSE}", "{PARTIAL RESPONSE ??}",
                     "{MULTIPLE RESPONSE}", "{MISSING}", "{WAGE MISSING}",
                     "{REFUSED}", "{OUTRANGE}", "{OUT RANGE FOR CENSU}",
                     "{NO FFU INSTRUMT}", "{NO SFU INSTRUMENT}",
                     "{NOMAT SEI SCORE}", "{NO TFU INSTRU}",
                     "{NO MATCHING SEI SCR}", "{ROUTING ERROR}",
                     "{UNCODEABLE RESPONSE}",
                     "(Missing)", NA)
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    if(verbose){
      cat(paste0("Processing SAS syntax file.\n"))
    }
    
    fileFormat <- parseSAS_FileFormat_HSB(NLS7286_SASSyntax_Path) #get the file format from the master.txt file
    
    #this definition interferes with LaF FWF width spacing, so remove it
    fileFormat <- subset(fileFormat, fileFormat$variableName!="id_hstest")
    
    lafObj <- laf_open_fwf(NLS7286_PRI_FilePath, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)
    
    fileFormat <- identifyNLS72_Weights(fileFormat)
    fileFormat <- valueLabelCleanupFF(fileFormat, omittedLevels, c("{0 %}", "{0}", "{0.00}", "{0.00000}","{$0}", "{0 HRS}", "{O HOURS}",
                                                                   "{ZERO HRS}", "{ZERO}", "{zero}", "{NO WEEKS}",
                                                                   "{MIXED ALPHA & NUMERIC}", "{ALPHA&NUMERIC GRADES}",
                                                                   "{CITY NAME}", "{EDITED}"))
    fileFormat <- writeCacheWithRepWgt_NLS72(lafObj, fileFormat, "finstno", "pst", cacheFilename, verbose) #TODO::This not currently working.  need guidance on JK reps
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, metaFilename)
  }#end if(runProcessing==TRUE)
  
  lafObj <- laf_open_csv(cacheFilename, fileFormat$dataType, fileFormat$variableName, skip=0) #reopen to the cached .csv file
  
  weights <- buildNLS72_WeightList(fileFormat)
  attributes(weights)$default <- "" #set default weight
  
  pvs <- list() #no plausible values or achievement levels
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildHSB_SODataList(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "1971-1986",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "NLS72",
                      country = "USA",
                      psuVar = "pst",
                      stratumVar = "finstno", 
                      jkSumMultiplier = 0.5,
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the HS&B Senior weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyNLS72_Weights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight variables
  wgtVars <- grep("wt$", varNames, ignore.case = TRUE, value=TRUE)
  wgtVars <- c(wgtVars, grep("^w\\d+$", varNames, ignore.case = TRUE, value=TRUE))
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildNLS72_WeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    
    tempVar <- wgtVars[i] #full variable name of the weight var
    baseWgtVar <- paste0(tempVar, "_jk2_")
    wgtPattern = paste0("^", baseWgtVar,"\\d+$")
    
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

buildNLS72_DataList <- function(lafObj, fileFormat){
  
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

writeCacheWithRepWgt_NLS72 <- function(dataLaF, fileFormat, stratumVar, psuVar, cachePath, verbose){
  
  # if(verbose){
  #   cat(paste0("Generating JK2 replicate weights.\n"))
  # }
  # 
  # wgtVars <- fileFormat$variableName[fileFormat$weights==TRUE]
  # 
  # wgtDFAll <- NULL #overall for all weights
  # wgtDFtemp <- NULL #temporary for weight loop
  # 
   newFF <- fileFormat[FALSE, ]
  # 
  # #build the JK2 replicate weights================
  # for(w in wgtVars){
  #   
  #   weight <- dataLaF[ , w]
  #   jkrep <- dataLaF[ , psuVar]
  #   jkzone <- dataLaF[ , stratumVar]
  #   ujkz <- sort(unique(jkzone[,1]))
  #   jkvars <- c()
  #   
  #   for(i in ujkz) {
  #     #first group
  #     coli <- paste0(w, "_JK2_" ,-1+i*2) #create replicate variable name
  #     jkvars <- c(jkvars, coli)
  #     jkw <- weight
  #     jkw[jkzone == i & jkrep == 1] <- 0
  #     jkw[jkzone == i & jkrep != 1] <- 2 * jkw[jkzone == i & jkrep != 1]
  #     jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
  #     
  #     if(!is.null(wgtDFtemp)){
  #       wgtDFtemp <- cbind(wgtDFtemp, jkw[,1])
  #     }else{
  #       wgtDFtemp <- jkw[,1]
  #     }
  #     
  #     
  #     tempFF <- fileFormat[fileFormat$variableName==w, ]
  #     tempFF$variableName <- coli
  #     tempFF$Labels <- paste0(tempFF$Labels, " Replicate ", -1+i*2)
  #     tempFF$labelValues <- ""
  #     tempFF$weights <- FALSE
  #     newFF <- rbind(newFF, tempFF)
  #     
  #     #second group
  #     coli <- paste0(w, "_JK2_", i*2)
  #     jkvars <- c(jkvars, coli)
  #     jkw <- weight
  #     jkw[jkzone == i & jkrep == 1] <- 2 * jkw[jkzone == i & jkrep == 1]
  #     jkw[jkzone == i & jkrep != 1] <- 0
  #     jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
  #     
  #     wgtDFtemp <- cbind(wgtDFtemp, jkw[,1])
  #     
  #     tempFF <- fileFormat[fileFormat$variableName==w, ]
  #     tempFF$variableName <- coli
  #     tempFF$Labels <- paste0(tempFF$Labels, " Replicate ", i*2)
  #     tempFF$labelValues <- ""
  #     tempFF$weights <- FALSE
  #     newFF <- rbind(newFF, tempFF)
  #   }
  #   
  #   if(!is.null(wgtDFAll)){
  #     wgtDFAll <- cbind(wgtDFAll, wgtDFtemp)
  #   }else{
  #     wgtDFAll <- wgtDFtemp
  #   }
  #   
  #   wgtDFtemp <- NULL
  # }
  # 
  # 
   newFF <- rbind(fileFormat, newFF)
  # 
  # #recalibrate the start/end positions so they are correct
  # newFF$Start <-  c(1,1 + cumsum(newFF$Width))[1:length(newFF$Width)]
  # newFF$End <- cumsum(newFF$Width)
  # 
  # if(verbose){
  #   cat(paste0("Writing cache data file.\n"))
  # }
  # #write out the cachefile
  # cacheFull <- cbind(dataLaF[], wgtDFAll)
  
  cacheFull <- dataLaF[] #temporary
  
  #write out data to cache .csv file
  userSciPen <- getOption("scipen")
  options(scipen = 999) #ensure no scientific notation
  
  #use write.table instead of write.csv to avoid issues with col.names argument
  write.table(cacheFull, cachePath, row.names = FALSE, col.names = FALSE, na="", sep = ",", qmethod = "double")
  
  options(scipen = userSciPen) #rest option back to user setting
  
  return(newFF)
}
