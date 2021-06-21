#' @title Connect to HS&B Study Senior Data
#'
#' @description Opens a connection to a High School & Beyond 1980--1986 Senior cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param HSR8086_PRI_FilePath a character value to the main study-derived
#'                             analytical data file (HSR8086_REV.PRI).
#'                             Located within the \code{REVISED_ASCII} Folder.
#' @param HSR8086_SASSyntax_Path a character value to the SAS syntax file for
#'                               parsing the \code{HSR8086_REV.PRI} data file.
#'                               Located within the \code{SAS_EXTRACT_LOGIC} Folder.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read
#'                    function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output
#'                while the \code{readHSB_Senior} function is running to
#'                indicate processing progress. The default value is \code{TRUE}.
#'                    
#' @details
#' Reads in the specified \code{HSR8086_SASSyntax_Path} file to parse
#' the \code{HSR8086_PRI_FilePath} file.
#' A cached data file and metadata file will be saved in the same
#' directory and filename as the \code{HSR8086_PRI_FilePath} file,
#' having new file extensions of .txt and .meta, respectively.  
#'          
#' Please note the original source \code{repcode} variable has been split
#' into two variables named \code{repcode_str} for the stratum value
#' and \code{repcode_psu} for the primary sampling unit (PSU) value in the resulting
#' cache data.
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the HS&B Senior 1980--1986 longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readHSB_SR.R
#' 
readHSB_Senior <- function(HSR8086_PRI_FilePath,
                           HSR8086_SASSyntax_Path,
                           forceReread=FALSE,
                           verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  HSR8086_PRI_FilePath <- suppressWarnings(normalizePath(unique(HSR8086_PRI_FilePath), winslash = "/"))
  HSR8086_SASSyntax_Path <- suppressWarnings(normalizePath(unique(HSR8086_SASSyntax_Path), winslash = "/"))
  
  if(!file.exists(HSR8086_PRI_FilePath)){
    stop(paste0("Cannot find specified data file ", sQuote("HSR8086_PRI_FilePath"), " in path ", sQuote(file.path(HSR8086_PRI_FilePath)), "."))
  }
  if(!file.exists(HSR8086_SASSyntax_Path)){
    stop(paste0("Cannot find specified data file ", sQuote("HSR8086_SASSyntax_Path"), " in path ", sQuote(file.path(HSR8086_SASSyntax_Path)), "."))
  }
  
  #prep the data cache items
  cacheFilename <- gsub("\\.pri$", ".txt", HSR8086_PRI_FilePath, ignore.case = TRUE)
  metaFilename <- gsub("\\.pri$", ".meta", HSR8086_PRI_FilePath, ignore.case = TRUE)
  
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
  omittedLevels <- c("{DON'T KNOW}", "{DONT'T KNOW}",
                     "{LEGITIMATE SKIP}", "{MISSING, LEGIT SKIP}",
                     "{MISSING}", "{MULTIPLE PUNCH}", "{OUT OF RANGE}",
                     "{REFUAL}", "{REFUSAL}", "{UNC VERBATIM}", 
                     "{UNC. VERBATIM}", "{UNCODABLE VERBATIM}", 
                     "(Missing)", NA)
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    if(verbose){
      cat(paste0("Processing SAS syntax file.\n"))
    }
    
    fileFormat <- parseSAS_FileFormat_HSB(HSR8086_SASSyntax_Path) #get the file format from the master.txt file
    
    #the bypart field is defined in the layout file, but doesn't actually exist in the datafile!
    #this causes all sorts of FWF issues when reading in the original .PRI file as well as subsequent output
    fileFormat <- subset(fileFormat, fileFormat$variableName != "bypart") 
    
    #the REPCODE field contains both the stratum and PSU in one field, redefine the fileformat to split it into two vars for analysis
    rowIdx <- which(fileFormat$variableName=="repcode", arr.ind = TRUE)
    
    tempFF <- fileFormat[1:(rowIdx-1), ] 
    repcodeFF <- fileFormat[rowIdx, ]
    tempFF2 <- fileFormat[(rowIdx+1):nrow(fileFormat), ]
    repcodeFF2 <- repcodeFF #duplicate copy of the repCodeFF
    
    repcodeFF$variableName <- "repcode_str"
    repcodeFF$End <- repcodeFF$End - 1
    repcodeFF$Width <- repcodeFF$Width - 1
    repcodeFF$Labels <- "REPLICATED SAMPLE CODE - stratum"
    
    repcodeFF2$variableName <- "repcode_psu"
    repcodeFF2$Start <- repcodeFF2$Start + 2
    repcodeFF2$Width <- repcodeFF2$Width - 2
    repcodeFF2$Labels <- "REPLICATED SAMPLE CODE - psu"
    
    #rebuild the parts after the repcode variable was dissected into it's parts
    fileFormat <- rbind.data.frame(tempFF, repcodeFF, repcodeFF2, tempFF2, stringsAsFactors = FALSE)
    rownames(fileFormat) <- 1:nrow(fileFormat)
    
    #recalibrate the start/end positions so they are correct
    fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)]
    fileFormat$End <- cumsum(fileFormat$Width)
    
    lafObj <- laf_open_fwf(HSR8086_PRI_FilePath, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)
    
    fileFormat <- identifyHSB_SRWeights(fileFormat)
    fileFormat <- valueLabelCleanupFF(fileFormat, omittedLevels, c("{$ ZERO}", "{NON-PARTICIPANT}"))
    fileFormat <- writeCacheWithRepWgt_HSB(lafObj, fileFormat, "repcode_str", "repcode_psu", cacheFilename, verbose)
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, metaFilename)
  }#end if(runProcessing==TRUE)
  
  lafObj <- laf_open_fwf(cacheFilename, column_types=fileFormat$dataType, column_widths=fileFormat$Width, column_names=fileFormat$variableName)
  
  weights <- buildHSB_SRWeightList(fileFormat)
  if(!is.null(weights)){
    attributes(weights)$default <- "" #set default weight
  }
  
  pvs <- list() #no plausible values or achievement levels
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildHSB_SRDataList(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "1980-1986",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "Senior",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "HS&B",
                      country = "USA",
                      psuVar = "repcode_psu",
                      stratumVar = "repcode_str", 
                      jkSumMultiplier = 1/184, #184 replicate weights
                      validateFactorLabels = TRUE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the HS&B Senior weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyHSB_SRWeights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight variables
  wgtVars <- grep("wt(2|3|4){0,1}$", varNames, ignore.case = TRUE, value=TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}


writeCacheWithRepWgt_HSB <- function(dataLaF, fileFormat, stratumVar, psuVar, cachePath, verbose){
  
  if(verbose){
    cat(paste0("Generating JK2 replicate weights.\n"))
  }
  
  wgtVars <- fileFormat$variableName[fileFormat$weights==TRUE]
  
  wgtDFAll <- NULL #overall for all weights
  wgtDFtemp <- NULL #temporary for weight loop
  
  newFF <- fileFormat[FALSE, ]
  
  #build the JK2 replicate weights================
  for(w in wgtVars){
    
    weight <- dataLaF[ , w]
    jkrep <- dataLaF[ , psuVar]
    jkzone <- dataLaF[ , stratumVar]
    ujkz <- sort(unique(jkzone[,1]))
    jkvars <- c()
    
      for(i in ujkz) {
        #first group
        coli <- paste0(w, "_JK2_" ,-1+i*2) #create replicate variable name
        jkvars <- c(jkvars, coli)
        jkw <- weight
        jkw[jkzone == i & jkrep == 1] <- 0
        jkw[jkzone == i & jkrep != 1] <- 2 * jkw[jkzone == i & jkrep != 1]
        jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
        
        if(!is.null(wgtDFtemp)){
          wgtDFtemp <- cbind(wgtDFtemp, jkw[,1])
        }else{
          wgtDFtemp <- jkw[,1]
        }
        
        
        tempFF <- fileFormat[fileFormat$variableName==w, ]
        tempFF$variableName <- coli
        tempFF$Labels <- paste0(tempFF$Labels, " Replicate ", -1+i*2)
        tempFF$labelValues <- ""
        tempFF$weights <- FALSE
        
        #ensure we have adequate size for the fileFormat otherwise it will throw off our spacing
        testFmt <- format(jkw[,1], scientific = FALSE, width = tempFF$Width, nsmall = tempFF$Decimal, justify = "right", drop0trailing=FALSE)
        tempFF$Width <- max(nchar(testFmt))
        
        newFF <- rbind(newFF, tempFF)
        
        #second group
        coli <- paste0(w, "_JK2_", i*2)
        jkvars <- c(jkvars, coli)
        jkw <- weight
        jkw[jkzone == i & jkrep == 1] <- 2 * jkw[jkzone == i & jkrep == 1]
        jkw[jkzone == i & jkrep != 1] <- 0
        jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
        
        wgtDFtemp <- cbind(wgtDFtemp, jkw[,1])
        
        tempFF <- fileFormat[fileFormat$variableName==w, ]
        tempFF$variableName <- coli
        tempFF$Labels <- paste0(tempFF$Labels, " Replicate ", i*2)
        tempFF$labelValues <- ""
        tempFF$weights <- FALSE
        
        #ensure we have adequate size for the fileFormat otherwise it will throw off our spacing
        testFmt <- format(jkw[,1], scientific = FALSE, width = tempFF$Width, nsmall = tempFF$Decimal, justify = "right", drop0trailing=FALSE)
        tempFF$Width <- max(nchar(testFmt))
        
        newFF <- rbind(newFF, tempFF)
      }
    
    if(!is.null(wgtDFAll)){
      wgtDFAll <- cbind(wgtDFAll, wgtDFtemp)
    }else{
      wgtDFAll <- wgtDFtemp
    }
    
    wgtDFtemp <- NULL
  }
  
  
  newFF <- rbind(fileFormat, newFF)
  
  #recalibrate the start/end positions so they are correct
  newFF$Start <-  c(1,1 + cumsum(newFF$Width))[1:length(newFF$Width)]
  newFF$End <- cumsum(newFF$Width)
  
  if(verbose){
    cat(paste0("Writing cache data file.\n"))
  }
  #write out the cachefile
  cacheFull <- cbind(dataLaF[], wgtDFAll)
  
  newFF <- writeDF_FWF(df=cacheFull, fileFormat=newFF, savePath=cachePath, verbose=verbose)
  
  return(newFF)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildHSB_SRWeightList <- function(fileFormat){
  
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

buildHSB_SRDataList <- function(lafObj, fileFormat){
  
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