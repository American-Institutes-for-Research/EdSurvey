#' @title Connect to ECLS-B Data
#'
#' @description Opens a connection to an ECLS-B data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path(s) to the ECLS-B extracted fixed-with-format (.dat) set of datafiles.
#' @param filename a character value of the name of the fixed-width-file (.dat) data file in the specificed \code{path} to be read.
#' @param layoutFilename a character value of the filename of either the ASCII text (.txt) layout file of the \code{filename} within the specified \code{path}, 
#'                       OR a character value of the  filename of the SPSS syntax (.sps) layout file of the \code{filename} within the specified \code{path}
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#'                    
#' @param verbose a logical value that will determine if you want verbose output while the \code{readECLS-K2011} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#' @details Reads in the unzipped files downloaded from the ECLS-B longitudinal Database.
#'
#' 
#' @return An \code{edsurvey.data.frame} for the ECLS-B longitudinal dataset.
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{getData}}
#' @author Trang Nguyen
#' @export
readECLS_B <- function(path = getwd(),
                       filename,
                       layoutFilename,
                       forceReread=FALSE,
                       verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  
  # 1. Extract data file information from parsed arguments ====
  # a. Data file name
  if (missing(filename)) {
    filename <- list.files(path,pattern = "\\.dat$",ignore.case=TRUE)
  }
  if (length(filename) > 1) {
    stop(paste0("There is more than one .dat file in ", sQuote(path),". Please specify data file in the ", sQuote(filename), " argument."))
  }
  if (length(filename) == 0) {
    stop(paste0("There is no .dat file in ", sQuote(path),". Please specify data file in the ", sQuote(filename), " argument."))
  }
  if (!file.exists(file.path(path,filename))) {
    stop(paste0("Cannot find specified file ", sQuote(file.path(path, filename)),"."))
  }
  
  # b. Layout file name
  if (missing(layoutFilename)) {
    layoutFilename <- list.files(path,pattern = "\\.sps$",ignore.case=TRUE)
  }
  if (length(layoutFilename) > 1) {
    stop(paste0("There is more than one .sps file in ", sQuote(path),". Please specify data file in the ", sQuote(layoutFilename), " argument."))
  }
  if (length(layoutFilename) == 0) {
    stop(paste0("There is no .sps file in ", sQuote(path),". Please specify data file in the ", sQuote(layoutFilename), " argument."))
  }
  if (!file.exists(file.path(path,layoutFilename))) {
    stop(paste0("Cannot find specified layout file: ", sQuote(file.path(path, layoutFilename)),"."))
  }
  fileList <- list(dataFile=file.path(path,filename),
                   layoutFile=file.path(path,layoutFilename))
  # 2. Process Data =====
  cacheInfo <- list(cacheFilepath = file.path(path, gsub("\\.dat$", "\\.txt", filename, ignore.case = TRUE)),
                    cacheMetaFilepath = file.path(path, gsub("\\.dat$", "\\.meta", filename, ignore.case = TRUE)))
  
  processArgs <- list(files = fileList,
                      cacheFileInfo = cacheInfo,
                      forceReread = forceReread,
                      verbose = verbose)
  
  processedData <- do.call("processECLS_B", processArgs, quote = TRUE)
  weights <- buildECLSBWeightList(processedData$fileFormat)
  # no default weight
  attr(weights, "default") <- "" 
  
  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("NOT APPLICABLE", 
                     "REFUSED",
                     "DON'T KNOW",
                     "NOT ASCERTAINED",
                     NA,
                     "")
  # 3. Return data
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildECLSB_dataList(processedData$data,processedData$fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "Children's Early School Experience",
                      year = "2007",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "Kindergarten",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "ECLS_B",
                      country = "USA",
                      psuVar = "", 
                      stratumVar = "", 
                      jkSumMultiplier = 1,
                      validateFactorLabels = TRUE)
                      
  
  
  
}

# Helper Function ==========
processECLS_B <- function (files,
                          cacheFileInfo,
                               forceReread,
                               verbose) {
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFileInfo$cacheFilepath)){
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      
      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "ECLS_K2011")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    #first delete the existing cache file if it exists in case the processing errors then it won't pickup the cache file
    if(file.exists(cacheFileInfo$cacheFilepath)){
      file.remove(cacheFileInfo$cacheFilepath)
    }
    
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      file.remove(cacheFileInfo$cacheMetaFilepath)
    }
    
    
    if(grepl("\\.txt$", files$layoutFile, ignore.case=TRUE)){
      if(verbose){
        cat(paste0("Processing text file format file.\n"))
      }
      fileFormat <- parseTEXTFileFormat_NCES(files$layoutFile)
    }else if(grepl("\\.sps$", files$layoutFile, ignore.case=TRUE)){
      if(verbose){
        cat(paste0("Processing SPSS syntax file.\n"))
      }
      fileFormat <- parseSPSSFileFormat(files$layoutFile)
    }else{
      stop(paste0("File layout file must be either an ASCII (.txt) layout file or an SPSS (.sps) syntax file."))
    }
    
    #record index is for multi-lined .dat file processing
    #find max recordindex if SPSS syntax, or set to value of 1 otherwise
    if(is.null(fileFormat$RecordIndex) || max(fileFormat$RecordIndex)==1){
      maxRecordIndex = 1 #no need to do any additional processing
      tempFilename <- NULL
    }else{
      
      if(verbose==TRUE){
        cat("Flattening multiline *.dat file to temp file.\n")
      }
      
      maxRecordIndex <- max(fileFormat$RecordIndex) #no NAs should be present here
      tempFilename <- gsub("\\.dat$", ".tmp", files$dataFile, ignore.case = TRUE)
      
      if(file.exists(tempFilename)){
        file.remove(tempFilename)
      }
      
      readConnection <- file(files$dataFile, "r")
      writeConnection <- file(tempFilename, "w")
      lineWidth <- sapply(1:maxRecordIndex, function(i) {sum(fileFormat$Width[fileFormat$RecordIndex==i])})
      while(TRUE){
        linePart <- readLines(readConnection, maxRecordIndex)
        #be sure to exit once done
        if(length(linePart)==0){
          break
        }
        # all lines have width of 4999 (with extra spaces) 
        linePart <- sapply(1:length(linePart), function(i) {
          substr(linePart[i],1,lineWidth[i])
        })
        writeStr <- paste0(linePart, collapse = "")
        writeLines(writeStr, writeConnection)
      }
      
      close(readConnection)
      close(writeConnection)
      
      #prep now to read the .tmp data file
      files$dataFile <- tempFilename
    }
    
    dataLAF <- LaF::laf_open_fwf(files$dataFile, column_types = rep("character", length(fileFormat$variableName)), column_widths = fileFormat$Width, column_names = fileFormat$variableName)
    
    #define chunk size to read the values in:: chunk size should be large enough to accurately detect correct column data types, but small enough to not take up all the memory
    maxRows <- nrow(dataLAF)
    rowChunkSize <- 5000
    
    rowChunks <- split(1:maxRows, ceiling(seq_along(1:maxRows)/rowChunkSize)) #break up the number of rows into our chunk size
    
    for(rci in 1:length(rowChunks)){
      
      if(verbose==TRUE){
        cat(paste0("Processing data, number of columns ", nrow(fileFormat), ", rows ", min(rowChunks[[rci]]), " to ", max(rowChunks[[rci]]), " of ", maxRows, ".\n"))
      }
      
      dataChunk <- dataLAF[rowChunks[[rci]], ] #get the rows of our specific row chunk
      formattedTxt <- matrix(nrow=nrow(dataChunk), ncol=ncol(dataChunk))
      
      for(coli in 1:ncol(dataChunk)){
        xCol <- dataChunk[,coli]
        #remove any null indicators, will be strictly '.' value
        xCol[xCol=="."] <- NA 
        xCol[trimws(xCol, which="both")==""] <- NA
        
        #determine data types as the types are not defined in the ascii file layout on first group
        #no need to change FWF widths based on this since the original .dat file widths as adequate size
        if(rci==1){
          if(suppressWarnings(all(!is.na(as.numeric(xCol[!is.na(xCol)]))))){ #determine if all the NA values are numeric or character
            zCol <- xCol[!is.na(xCol)]
            hasDec <- grepl(".", zCol, fixed = TRUE)
            if(!any(hasDec)){
              precision <- 0
            }else{
              decPos <- regexpr(".", zCol[hasDec], fixed = TRUE)
              precision <- nchar(substring(zCol, decPos+1))
            }
            scale <- nchar(sub(".", "", zCol, fixed = TRUE))
            
            if(max(scale)<8 && max(precision)==0){
              fileFormat$dataType[coli] <- "integer"
              fileFormat$Decimal[coli] <- 0
            }else{
              fileFormat$dataType[coli] <- "numeric"
              fileFormat$Decimal[coli] <- max(as.numeric(precision))
            }
          }else{
            fileFormat$dataType[coli] <- "character"
            fileFormat$Decimal[coli] <- NA
          }
        }
        
        if(fileFormat$dataType[coli] %in% c("numeric") && fileFormat$Decimal[coli]>0){
          multiplier <- 10^as.numeric(fileFormat$Decimal[coli])
          xCol <- as.numeric(xCol) * multiplier
          xColChar <- format(xCol, scientific = FALSE)
          xColChar[is.na(xCol)] <- " "
          
          #test if the multiplier expanded the width beyond the intial set width otherwise FWF spacing issues will pop up
          if(any(nchar(xColChar)>fileFormat$Width[coli])){
            fileFormat$Width[coli] <- max(nchar(xColChar))
            
            #recalibrate the start/end positions for user
            fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)]
            fileFormat$End <- cumsum(fileFormat$Width)
          }
          
          xCol <- xColChar #swap back names
          xColChar <- NULL
        }
        
        # convert to blank for writing to FWF
        xCol[is.na(xCol)] <- " " 
        #store formatted column into matrix for writing
        formattedTxt[,coli] <- format(xCol, scientific = FALSE, width = fileFormat$Width[coli], justify = "right")
      }
      
      #remove the file if it exists and we are reprocessing
      if(rci==1 && file.exists(cacheFileInfo$cacheFilepath)){
        file.remove(cacheFileInfo$cacheFilepath)
      }
      
      if(verbose==TRUE){
        cat(paste0("Processing data, writing data chunk to disk.\n"))
      }
      #write the fwf formatted matrix
      a <- sapply(1:nrow(formattedTxt), function(rowi){
        cat(paste(formattedTxt[rowi,], collapse=""), file=cacheFileInfo$cacheFilepath, append = TRUE)
        cat(paste("\n"), file=cacheFileInfo$cacheFilepath, append = TRUE)
      })
      
      #minimize memory footprint
      a <- NULL
      dataChunk <- NULL
      formattedTxt <- NULL
    }
    
    #close the existing LAF connection to the .dat file and pickup new LaF handle for the FWF .txt file we just wrote
    LaF::close(dataLAF)
    
    if(grepl("\\.tmp$", files$dataFile, ignore.case = TRUE)){
      file.remove(files$dataFile)  
    }
    
    #parse weight variables for the fileFormat
    fileFormat <- identifyECLSBWeights(fileFormat)
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, cacheFileInfo$cacheMetaFilepath)
    
  }else{ #if(runProcessing==TRUE)
    
    if(verbose==TRUE){
      cat(paste0("Found cached data for file ", dQuote(files$dataFile),".\n"))
    }
  }#end if(runProcessing==TRUE)
  
  #return LAF as open to edsurvey.data.frame constructor where it needs it open to first build, then it handles closing within there
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)
  
  #do caching and testing
  return(list(data = dataLAF,
              fileFormat = fileFormat))
}





#identified the ECLS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyECLSBWeights <- function(fileFormat){
  
  varNames <- fileFormat$variableName[!grepl("replicate|rpl",fileFormat$Labels,ignore.case=TRUE)]
  
  #unable to grepl as it picked up too many replicate weights
  wgtVars <- grep("^w.*0$",varNames,value=TRUE, ignore.case=TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- tolower(fileFormat$variableName) %in% tolower(wgtVars)
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildECLSBWeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  varNames <- fileFormat$variableName
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i]
    # strip the ending '0' from the variable::all the replicates will have the same name but numbered 1-n 
    testJKprefix <- substr(tempVar, 1, nchar(tempVar)-1)

    ujkz <- unique(tolower(grep(paste0("^", testJKprefix,"[0-9]"), fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    
    # Remove weight from jk weights list
    ujkz <- ujkz[ujkz != tempVar]
    
    #remove jk to leave the numeric values
    ujkz <- gsub(tolower(testJKprefix), "", ujkz, fixed = TRUE)
    suppressWarnings(ujkz <- ujkz[!is.na(as.numeric(ujkz))])
    
    if (tempVar %in% c("w1r0", "w1c0", "w1f0", "w1fc0")) {
      psuVar <- grep("w1rrpsu", varNames, ignore.case = TRUE, value = TRUE)
      strVar <- grep("w1rstr", varNames, ignore.case = TRUE, value = TRUE)
    } else {
      #gather the psu variable, it will either be the weight variable name ending in 'psu', OR the variable less the trailing '0' ending in 'psu'
      psuVar <- c(grep(paste0(tempVar, "psu"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "psu"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "ps$"), varNames, ignore.case = TRUE, value = TRUE))
      
      #gather the stratum variable, it will either be the weight variable name ending in 'str', OR the variable less the trailing '0' ending in 'str'
      strVar <- c(grep(paste0(tempVar, "str"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "str"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "st$"), varNames, ignore.case = TRUE, value = TRUE))
    }

    if(length(psuVar)!=1){
      warning(paste0("Cannot find primary sampling unit variable for weight ", sQuote(tempVar), "." ))
    }
    
    if(length(psuVar)!=1){
      warning(paste0("Cannot find stratum variable for weight ", sQuote(tempVar), "."))
    }
    
    if(length(ujkz)>0){
      tmpWgt <- list()
      tmpWgt[[1]] <- list(jkbase=testJKprefix, jksuffixes=as.character(ujkz), psuVar=psuVar, stratumVar=strVar)
      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights,tmpWgt)
    }
  }
  
  return(weights)
}

#builds the ECLS_B dataList object
buildECLSB_dataList <- function(LaF, FF){
  
  dataList <- list()
  
  dataList[["Data"]] <- dataListItem(lafObject = LaF,
                                     fileFormat = FF,
                                     levelLabel = "Data",
                                     forceMerge = TRUE,
                                     parentMergeLevels = NULL,
                                     parentMergeVars = NULL,
                                     mergeVars = NULL,
                                     ignoreVars = NULL,
                                     isDimLevel = TRUE)
  
  return(dataList)
}
