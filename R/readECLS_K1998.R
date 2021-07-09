#' @title Connect to ECLS--K 1998 Data
#'
#' @description Opens a connection to an ECLS--K 1998 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path(s) to the 
#'             ECLS--K-extracted fixed-width-format (.dat) set of data files
#' @param filename a character value of the name of the fixed-width (.dat)
#'                 data file in the specified \code{path} to be read
#' @param layoutFilename a character value of the filename of either the ASCII
#'                       (.txt) layout file of the \code{filename} within
#'                       the specified \code{path}
#'                       or a character value of the  filename of the SPSS syntax (.sps) layout file of the \code{filename} within the specified \code{path}
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output while the \code{readECLS-K1998} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#' @details Reads in the unzipped files downloaded from the ECLS--K 1998 longitudinal dataset(s) to an \code{edsurvey.data.frame}.  The ECLS--K 1998--99 study consisted of
#'          three distinct separate datasets that cannot be combined: (1) Child Grades K--8 Data, (2) School Base-Year Data, and (3) Teacher Base-Year Data.
#'          The \code{filename} and \code{layoutFilename} arguments default to the corresponding Child K--8 default filenames.
#'
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the ECLS--K 1998 longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, \code{\link{getData}}, \code{\link{downloadECLS_K}}
#' @author Tom Fink
#'
#' @example \man\examples\readECLS_K1998.R
#' 
#' @export
readECLS_K1998 <- function(path = getwd(),
                           filename = "eclsk_98_99_k8_child_v1_0.dat",
                           layoutFilename = "Layout_k8_child.txt",
                           forceReread = FALSE, verbose = TRUE) {

  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl=TRUE, ignore.case=TRUE), dirname(path), path)
  
  #setup file list to work with
  fileList <- list(dataFile=unlist(file.path(path, filename))[1],
                   layoutFile=unlist(file.path(path, layoutFilename))[1])
  
  #validate files::get the filecount to see if we have any missing or excess files
  validateData <- sapply(fileList$dataFile, function(x){
    file.exists(x)
  })
  layoutData <- sapply(fileList$layoutFile, function(x){
    file.exists(x)
  })
  
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
  
  cacheInfo <- list(cacheFilepath = file.path(path, gsub("\\.dat$", "\\.txt", filename, ignore.case = TRUE)),
                    cacheMetaFilepath = file.path(path, gsub("\\.dat$", "\\.meta", filename, ignore.case = TRUE)))
  
  processArgs <- list(files = fileList,
                      cacheFileInfo = cacheInfo,
                      forceReread = forceReread,
                      verbose = verbose)

  retryProc <- tryCatch({processedData <- do.call("processECLS_K1998", processArgs, quote = TRUE)
                          FALSE
                        }, error = function(e){
                          TRUE #flag to retry
                        }, warning = function(w){
                          TRUE #flag to retry
                        })

  if (retryProc){
    processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
    processedData <- tryCatch(do.call("processECLS_K1998", processArgs, quote = TRUE),
                              error = function(e){
                                stop(paste0("Unable to process ECLS_K data. Possible file corruption with source data. ",
                                            "Error message: ", e))
                              })
  }
  
  weights <- buildECLSKWeightList(processedData$fileFormat)
  attr(weights, "default") <- "" #no default weight
  
  dataList <- buildECLSK_dataList(processedData$data, processedData$fileFormat)
  
  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("NOT APPLICABLE", 
                     "DATA SUPPRESSED", 
                     "SUPPRESSED", 
                     "REFUSED", 
                     "DON'T KNOW", 
                     "NOT ASCERTAINED", 
                     NA,
                     "(Missing)")
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = "Children's Early School Experience",
                      year = "1998-1999",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "K-8 Grade(s)",
                      achievementLevels = NULL,
                      omittedLevels = omittedLevels,
                      survey = "ECLS_K",
                      country = "USA",
                      psuVar = NULL, #psu is specific to each weight variable
                      stratumVar = NULL, #stratum is specific to each weight variable
                      jkSumMultiplier = 1,
                      validateFactorLabels = TRUE) #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
}

processECLS_K1998 <- function (files,
                           cacheFileInfo,
                           forceReread,
                           verbose) {
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFileInfo$cacheFilepath)){
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      
      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "ECLS_K")){
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
    
    #must open with all columns as character fields first:: For numeric values they use a '.' marker in the FWF datafile so we will need to convert those before converting to numeric
    dataLAF <- laf_open_fwf(files$dataFile, column_types = rep("character", length(fileFormat$variableName)), column_widths = fileFormat$Width, column_names = fileFormat$variableName)
    
    #define chunk size to read the values in:: chunk size should be large enough to accurately detect correct column data types, but small enough to not take up all the memory
    rowChunkSize <- 5000
    maxRows <- nrow(dataLAF)
    
    rowChunks <- split(1:maxRows, ceiling(seq_along(1:maxRows)/rowChunkSize)) #break up the number of rows into our chunk size
    
    for(rci in 1:length(rowChunks)){
      
      if(verbose==TRUE){
        cat(paste0("Processing Data, n columns ", nrow(fileFormat), ", rows ", min(rowChunks[[rci]]), " to ", max(rowChunks[[rci]]), " of ", maxRows, ".\n"))
      }
      
      dataChunk <- dataLAF[rowChunks[[rci]], ] #get the rows of our specific row chunk
      formattedTxt <- matrix(nrow=nrow(dataChunk), ncol=ncol(dataChunk))
      
      for(coli in 1:ncol(dataChunk)){
          xCol <- dataChunk[,coli]
          xCol[xCol=="."] <- NA #remove any null indicators, will be strictly '.' value
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
          
          xCol[is.na(xCol)] <- " "
          formattedTxt[,coli] <- format(xCol, scientific = FALSE, width = fileFormat$Width[coli], justify = "right") #store formatted column into matrix for writing
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
    
    #parse weight variables for the fileFormat
    fileFormat <- identifyECLSKWeights(fileFormat)
    
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
  
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)
  
  #do caching and testing
  return(list(data = dataLAF,
              fileFormat = fileFormat))
}

#identified the ECLS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyECLSKWeights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #BY= BaseYear; S=School; B=Teacher; C=Child
  wgtVars <- grep("^(S|B|C).*(W|C|P|S|M|E|R)0$", varNames, value=TRUE, ignore.case = TRUE)
  wgtVarsSpecial <- grep("Y2COMW0", varNames, value=TRUE, ignore.case = TRUE)
  wgtVarsSpecial <- grep("BYCOMW0", varNames, value=TRUE, ignore.case = TRUE)
  wgtVars <- c(wgtVars, wgtVarsSpecial)

  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildECLSKWeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  varNames <- fileFormat$variableName
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i]
    testJKprefix <- substr(tempVar, 1, nchar(tempVar)-1) #strip the ending '0' from the variable::all the replicates will have the same name but numbered 1-n 
    testJKprefix2 <- substr(tempVar, 1, nchar(tempVar)-2) #strip the ending '[w]0' from the variable
    
    ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix ,")","[1-9]"), fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- gsub(tolower(testJKprefix), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values
    
    #gather PSU an Stratum info, For 1998 there isn't much consistancy for the naming conventions
    #between the weight variable name and it's associated PSU and Stratum variable names
    if(tolower(tempVar)=="c4_7cw0"){
      psuVar <- grep("^c47fcpsu$", varNames, ignore.case = TRUE, value = TRUE)
      strVar <- grep("^c47fcstr$", varNames, ignore.case = TRUE, value = TRUE)
    }else if(tolower(tempVar)=="c4_7pw0"){
      psuVar <- grep("^c47fppsu$", varNames, ignore.case = TRUE, value = TRUE)
      strVar <- grep("^c47fpstr$", varNames, ignore.case = TRUE, value = TRUE)
    }else{
      #gather the psu variable, rules vary widely between the original weight variable name
      psuVar <- c(grep(paste0(tempVar, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "ps$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(gsub("_", "", testJKprefix, fixed = TRUE), "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix2, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(substr(testJKprefix,1,2), "t", substr(testJKprefix,3,nchar(testJKprefix)),"psu$"), varNames, ignore.case = TRUE, value = TRUE))
      
      #gather the stratum variable, rules vary widely between the original weight variable name
      strVar <- c(grep(paste0(tempVar, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "st$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(gsub("_", "", testJKprefix, fixed = TRUE), "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix2, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(substr(testJKprefix,1,2), "t", substr(testJKprefix,3,nchar(testJKprefix)),"str$"), varNames, ignore.case = TRUE, value = TRUE))
    }
    psuVar <- unique(psuVar)
    strVar <- unique(strVar)
    
    if(length(psuVar)!=1){
      stop(paste0("Cannot Find Primary Sampling Unit Variable for Weight: ", tempVar ))
    }
    
    if(length(psuVar)!=1){
      stop(paste0("Cannot Find Stratum Variable for Weight: ", tempVar ))
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

#builds the ECLS_K dataList object
buildECLSK_dataList <- function(LaF, FF){
  
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


