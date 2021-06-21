#' @title Connect to B&B 1993--2003 Data
#'
#' @description Opens a connection to a Baccalaureate & Beyond 1993--2003 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param b03DAS_Filepath a character value to the main study-derived analytical data file (\code{B03DAS.dat}).  Located within the \code{ECBW/Data} Folder.
#' @param b03WEIGHT_FilePath a character value to the study weight data file (\code{B03WEIGHT.dat}). Located within the \code{ECBW/Data} Folder.
#' @param masterTxtFilepath a character value of the master format file (\code{master.txt}) that contains the metadata 
#'                          for the \code{b03DAS_Filepath} and \code{b03WEIGHT_FilePath}.  Located in the \code{ECBW} folder.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output while the \code{readBB_2003} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#'                    
#' @details Reads in the specified \code{masterTxtFilepath} file to parse the \code{b03DAS_Filepath} and \code{b03WEIGHT_FilePath} files 
#'          for the B&B 1993--2003 longitudinal survey to an \code{edsurvey.data.frame}.
#'          The two created cached data files will be saved in the same directory and filename as the \code{b03DAS_Filepath} file,
#'          having new file extensions of .txt and .meta.  
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the B&B 1993--2003 longitudinal dataset.
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readBB_2003.R
#' 
readBB_2003 <- function(b03DAS_Filepath,
                        b03WEIGHT_FilePath,
                        masterTxtFilepath,
                        forceReread=FALSE,
                        verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  b03DAS_Filepath <- suppressWarnings(normalizePath(unique(b03DAS_Filepath), winslash = "/"))
  b03WEIGHT_FilePath <- suppressWarnings(normalizePath(unique(b03WEIGHT_FilePath), winslash = "/"))
  masterTxtFilepath <- suppressWarnings(normalizePath(unique(masterTxtFilepath), winslash = "/"))
  
  if(!file.exists(b03DAS_Filepath)){
    stop(paste0("Cannot find specified data file ", sQuote("b03DAS_Filepath"), " in path ", sQuote(file.path(b03DAS_Filepath)), "."))
  }
  if(!file.exists(b03WEIGHT_FilePath)){
    stop(paste0("Cannot find specified data file ", sQuote("b03WEIGHT_FilePath"), " in path ", sQuote(file.path(b03WEIGHT_FilePath)), "."))
  }
  if(!file.exists(masterTxtFilepath)){
    stop(paste0("Cannot find specified data file ", sQuote("masterTxtFilepath"), " in path ", sQuote(file.path(masterTxtFilepath)), "."))
  }
  
  cacheFilename <- gsub("\\.dat$", ".txt", b03DAS_Filepath, ignore.case = TRUE)
  metaFilename <- gsub("\\.dat$", ".meta", b03DAS_Filepath, ignore.case = TRUE)
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFilename)){
    if(file.exists(metaFilename)){
      
      cacheRDS <- readRDS(metaFilename) #get the cache info from the .meta file
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "B&B2003")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }
  
  omittedLevels <- c("{Skipped}", "{BB03 nonrespondent}", "{Missing}",
                     "{Refused}", "{Don't know}", "{Missing, blank}",
                     "{Missing, legitimate skip}", "{Missing, unavailable}",
                     "{Uncodable, city not selected}", "{Not reached}",
                     "{B&B:97 nonrespondent not sampled B&B:03}",
                     "{BA date missing}", "{CATI 94 Non-respondents}",
                     "{Married prior to BA}", "{BA date missing}",
                     "{Earned graduate degree before BA}", "{Missing degree date}",
                     "{Missing BA date}", "{Enrolled in grad school before BA}",
                     "{Missing enrollment date}", "{Legitimate skip, no}",
                     "{Had children, birthdates missing}", "{Became parent prior to BA}",
                     "{Missing-CATI error}", "{Prior to 1980}", "{CATI error}", 
                     "{Missing, taught before or never taught}", "{Legitimate skip}",
                     "{Uncodeable, out of range}", "{Uncodeable, city not selected}", 
                     "{Uncodeable, state/city not selected}", "{Legitimate skip (Did not borrow UG)}",
                     "(Missing)", NA)
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    if(verbose){
      cat(paste0("Processing master text file.\n"))
    }
    
    fileFormat <- parseBBTxtMaster_2003(masterTxtFilepath) #get the file format from the master.txt file
    #retain the original file format multi-line specs until after the write loop
    
    maxRecIndex <- max(fileFormat$RecordIndex)
    
    if(maxRecIndex>1){
      if(file.exists(cacheFilename)){
        file.remove(cacheFilename)
      }
      
      derivedConn <- tryCatch({file(b03DAS_Filepath, "r")},
                              error = function(e){
                                stop(paste0("Unable to read data file.\n Please ensure you have read permissions to the read path: ", sQuote(b03DAS_Filepath), "\n",
                                            "Error message: ", e))
                              })
      
      tempFP <- sub("\\.dat$", "\\.tmp", b03DAS_Filepath, ignore.case = TRUE)
      
      tempFlatFile <- tryCatch({file(tempFP, "w")},
                                  error = function(e){
                                    stop(paste0("Unable to write to cache file.\n Please ensure you have write permissions to the save path: ", sQuote(tempFP), "\n",
                                                "Error message: ", e))
                                  })

      wgtData <- readLines(b03WEIGHT_FilePath)
      wgtDataID <- substr(wgtData, 1, 8)
      
      offset <- 9 #the ID value is contained on each row of data, we want to offset lines after 1 to start at column position 9
      rowIndex <- 1
      
      if(verbose){
        cat(paste0("Flattening multi-line .dat file to temporary file.\n"))
      }
      while(TRUE){
        linePart <- readLines(derivedConn, maxRecIndex) #get the number of lines as a group for each flattened row
        #be sure to exit once done
        if(length(linePart)==0){
          break
        }
        
        linePart[-1] <- substr(linePart[-1], offset, nchar(linePart[-1])) #trim off the duplicate ID values for all lines except the first
        
        idVal <- substr(linePart[1],1,8)
        wgtLine <- wgtData[which(wgtDataID==idVal, arr.ind = TRUE)]
        wgtLine <- substr(wgtLine, 9, nchar(wgtLine)) #strip id value from the list
        
        writeStr <- paste0(c(linePart, wgtLine), collapse = "")
        writeLines(writeStr, tempFlatFile)
      }#end while(TRUE)
    }#end if(maxRecIndex>1)
    
    close(derivedConn)
    close(tempFlatFile)
    #recalibrate start/end positions for flattened FWF datafile
    fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)] #recalc the start/end for a flattened file
    fileFormat$End <- cumsum(fileFormat$Width) 
    
    #load the flattened file into data.frame and delete the temporary file as it's no longer needed
    lafCon <- laf_open_fwf(tempFP, rep("character", times=nrow(fileFormat)), fileFormat$Width, trim=FALSE) #read back in the flattened-file to LaF object
    rawData <- lafCon[] #read the data into a data.frame
    close(lafCon)
    file.remove(tempFP)
    
    #these columns need special processing to correctly format the decimal place
    #see SAS documentation for specialized decimal formatting
    colsToValidate <- which(!is.na(fileFormat$DecimalInputFmt), arr.ind = TRUE)
    
    for(i in colsToValidate){
      colData <- rawData[,i]
      
      for(ii in 1:length(colData)){
        if(!grepl(".", colData[ii], fixed = TRUE)){ #perform the decimal conversion here
          dataVal <- as.numeric(colData[ii]) / as.numeric(fileFormat$DecimalInputFmt[i])
          dataVal <- ifelse(is.na(dataVal),"", dataVal)
          dataVal <- format(dataVal, width = fileFormat$Width[i], justify = "right") #if dataVal is 'wider' than specified width it will still be a wider width here
          
          #if we detect an out of range value, we need to expand any previous values to be the same length
          if(nchar(dataVal)>fileFormat$Width[i]){
            width <- nchar(dataVal)
            fileFormat$Width[i] <- width
            colData[ii] <- format(dataVal, width = width, justify = "right")#resize the specific value
            colData <- format(colData, width = width, justify = "right") #resize all values in the column for consistant spacing
          }else{
            colData[ii] <- dataVal
          }#end if(nchar(dataVal)>width)
        }#end if(grepl(".", colData[ii], fixed = TRUE))
      }#end  for(ii in 1:length(colData))
      
      rawData[,i] <- colData #be sure to save back to raw data.frame
    }#end for(i in colsToValidate)
    
    #recalibrate start/end positions again after any shifting
    fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)] #recalc the start/end for a flattened file
    fileFormat$End <- cumsum(fileFormat$Width) 
    
    #close the read connection and write the matrix to 
    outFile <- tryCatch({file(cacheFilename, "w")},
                             error = function(e){
                               stop(paste0("Unable to write to cache file.\n Please ensure you have write permissions to the save path: ", sQuote(cacheFilename), "\n",
                                           "Error message: ", e))
                             })
    
    fileFormat$DecimalInputFmt <- NULL # we can now remove this as it's not needed and may lead to confusion
    
    for(i in 1:nrow(rawData)){
      
      if(verbose && ((i %% 2500)==0 || i==1 || i==nrow(rawData))){
        cat(paste0("Writing data row ", i, " of ", nrow(rawData), "\n"))
      }
      writeStr <- paste0(rawData[i, ], collapse = "")
      writeLines(writeStr, outFile)
    }
    
    close(outFile)
    
    fileFormat <- valueLabelCleanupFF(fileFormat, omittedLevels, c("{Zero}", "{zero}"))
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, metaFilename)
    
  }#end if(runProcessing==TRUE)
  
  lafObj <- laf_open_fwf(cacheFilename, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)
  
  fileFormat <- identifyBBWeights_2003(fileFormat)
  weights <- buildBBWeightList_2003(fileFormat)
  attributes(weights)$default <- "bnbwt3" #set default weight
  
  pvs <- list() #no plausible values or achievement levels
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildBBDataList_2003(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "1993-2003",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "B&B2003",
                      country = "USA",
                      psuVar = NULL,  #psu and stratum are weight specific
                      stratumVar = NULL, 
                      jkSumMultiplier = 1/44, #weights have 44 replicates
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBBWeights_2003 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight variables
  wgtVars <- c("bnbwt3", "bnbpanl3", "bb03_w1", "bnbwt0")
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBBWeightList_2003 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  wgtLookupDF <- data.frame(wgt=wgtVars, 
                            brrBase=rep("", times=length(wgtVars)), 
                            psuVar=rep("", times=length(wgtVars)), 
                            stratumVar=rep("", times=length(wgtVars)), 
                            stringsAsFactors = FALSE)
  
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt3", "brrBase"] <- "b03brr"
  wgtLookupDF[wgtLookupDF$wgt=="bnbpanl3", "brrBase"] <- "b3pbrr"
  wgtLookupDF[wgtLookupDF$wgt=="bb03_w1", "brrBase"] <- "b3ibrr"
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt0", "brrBase"] <- "b00brr"
  
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt3", "psuVar"] <- "tayrep03"
  wgtLookupDF[wgtLookupDF$wgt=="bnbpanl3", "psuVar"] <- "tayrep03"
  wgtLookupDF[wgtLookupDF$wgt=="bb03_w1", "psuVar"] <- "tayrep97"
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt0", "psuVar"] <- "tayrep97"
  
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt3", "stratumVar"] <- "taystr03"
  wgtLookupDF[wgtLookupDF$wgt=="bnbpanl3", "stratumVar"] <- "taystr03"
  wgtLookupDF[wgtLookupDF$wgt=="bb03_w1", "stratumVar"] <- "taystr97"
  wgtLookupDF[wgtLookupDF$wgt=="bnbwt0", "stratumVar"] <- "taystr97"
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
    
    baseWgtVar <- wgtLookupDF$brrBase[wgtLookupDF$wgt==tempVar]
    psuVar <- wgtLookupDF$psuVar[wgtLookupDF$wgt==tempVar]
    stratumVar <- wgtLookupDF$stratumVar[wgtLookupDF$wgt==tempVar]
    
    wgtPattern = paste0("^", baseWgtVar,"\\d+$")
    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- sub(baseWgtVar, "", ujkz, ignore.case = TRUE) #strip away and leave just the numeric variable name ending as a string
    
    if(length(ujkz)>0){
      tmpWgt <- list()
      
      tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz), psuVar=psuVar, stratumVar=stratumVar)
      
      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights,tmpWgt)
    }
  }
  
  return(weights)
}

buildBBDataList_2003 <- function(lafObj, fileFormat){
  
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

#parses the 'master.txt' file to create and return a fileFormat object
parseBBTxtMaster_2003 <- function(masterTxtFilepath){
  
  #prepare return dictionary
  dict <- list("variableName" = character(0),
               "Start" = integer(0),
               "End" = integer(0),
               "Width" = integer(0),
               "Decimal" = integer(0),
               "DecimalInputFmt" = character(0),
               "Labels" = character(0),
               "labelValues" = character(0),
               "Type" = character(0),
               "pvWt" = character(0),
               "dataType" = character(0),
               "weights" = character(0),
               "FileNo" = character(0),
               "RecordIndex" = character(0))
  
  lines <- readLines(masterTxtFilepath)
  isVarLine <- grepl("^\\\\[^\\].*", lines, ignore.case = TRUE) #pickup variable lines such as: "\SCHL2AID  01  1      9-14   Federal aid at non-NPSAS inst 99-00   "  
  
  varLines <- lines[isVarLine]
  varLines <- varLines[varLines!="\\ENDFILE"] #remove ENDFILE item as it's not a valid item
  
  xMatch <- regexpr("\\w+", varLines) #get first word
  varNames <- tolower(trimws(regmatches(varLines, xMatch), which="both")) #use lower case variable names
  
  xMatch <- regexpr(" \\d+-\\d+", varLines) #get digits of fwf ###-###
  pos <- trimws(regmatches(varLines, xMatch), which="both")
  
  xMatch <- regexpr("\\d+-", pos) #get digits before '-' char
  posStart <- trimws(gsub("-", "", regmatches(pos, xMatch)), which="both")
  posStart <- as.integer(posStart)
  
  xMatch <- regexpr("-\\d+", pos)  #get digits after '-' char           
  posEnd <- trimws(gsub("-", "", regmatches(pos, xMatch)), which="both")
  posEnd <- as.integer(posEnd)
  
  #get the variable description text to the left of the numeric position ###-###
  xMatch <- regexpr(" \\d+-\\d+", varLines) #get digits of fwf ###-###
  varDesc <- xMatch + attr(xMatch, "match.length") #figure out the ending position of the FWF position field so we can grab the description to the left of that
  varDesc <- trimws(substring(varLines, varDesc), which = "both")
  
  varDesc <- gsub("^", "'", varDesc, fixed=TRUE) #replace any carrots with single quotes
  
  hasDecMulti <- grepl("^\\d ", varDesc)
  decMulti <- rep(NA, length(varDesc))
  decMulti[hasDecMulti] <- as.numeric(substr(varDesc[hasDecMulti],1,1))
  decMulti <- 10^decMulti
  
  #trim off the decimal multipler value
  varDesc <- sub("^\\d ", "", varDesc)
  
  #get the fileID value and the multi-line Record Indicator
  xMatch <- regexpr(" \\d+-\\d+", varLines)
  lineChunk <- substring(varLines, 1, xMatch)
  lineChunk <- gsub("\\", "", lineChunk, fixed=TRUE) #strip the \\ that begins the line
  lineChunk <- trimws(sub("\\w+", "", lineChunk), which="both") #strip off the first word of the string which is the variable name.
  
  lineChunk <- strsplit(lineChunk, " ") #creates list based on the spacing between the numbers
  
  fileNo <- sapply(lineChunk, function(x){as.numeric(x[1])})
  recordNo <- sapply(lineChunk, function(x){as.numeric(x[3])})
  
  dict$variableName <- varNames
  dict$Start <- posStart
  dict$End <- posEnd
  dict$Width <- (dict$End - dict$Start) + 1
  dict$Decimal <- rep("", length(varNames))
  dict$DecimalInputFmt <- decMulti
  dict$Labels <- varDesc
  dict$labelValues <- rep("", length(varNames))
  dict$Type <- rep("", length(varNames))
  dict$pvWt <- rep("", length(varNames))
  dict$dataType <- rep("integer", length(varNames)) #integer will be default
  dict$weights <- rep("", length(varNames))
  dict$FileNo <- fileNo
  dict$RecordIndex <- recordNo
  
  fileFormat <- as.data.frame(dict, stringsAsFactors=FALSE)
  
  #need to get the value labels and determine dataTypes based on the lines between the variable lines
  varLineIdx <- which(isVarLine, arr.ind = TRUE)
  
  if(!is.null(varLineIdx) && length(varLineIdx)>0){
    for(i in 1:(length(varLineIdx)-1)){
      currentLine <- varLineIdx[i] + 1
      currentVar <- varNames[i]
      currentFileNo <- fileNo[i]
      
      if((i+1)>=length(varLineIdx)){
        endLine <- length(lines)
      }else{
        endLine <- varLineIdx[i+1] - 1
      }
      
      varCode <- c()
      varDesc <- c()
      
      for(j in currentLine:endLine){
        testLine <- lines[j]
        if(grepl("^\\\\", testLine)){ #test if reached a string starting with "\\" as that ends the labels section
          break
        }
        
        xCode <- trimws(substr(testLine, 1, 11), which="both")
        
        if(xCode=="c"){
          #this signals the variable is a numeric value. Don't add as a value label, but mark it as numeric
          fileFormat$dataType[fileFormat$variableName %in% currentVar] <- paste0("numeric")
        }else{
          xDesc <- trimws(substr(testLine, 26, nchar(testLine)), which="both")
          
          if(grepl("Alpha string", xDesc, ignore.case = TRUE)){
            #signals that the variable is a string datatype
            fileFormat$dataType[fileFormat$variableName %in% currentVar] <- paste0("string")
          }else{
              
            xDesc <- gsub("^", "'", xDesc, fixed=TRUE) #replace any carrots with single quotes
            
            varCode <- c(varCode, xCode)
            varDesc <- c(varDesc, xDesc)
          }
        }
      } #end for(j in currentLine:endLine)
      
      if(length(varCode)>0){
        if(length(varCode)==1 && varCode[1]==0 && length(varDesc)==1 && varDesc[1]=="{zero}"){
          #if the only varcode and vardesc is 0={zero} then we want to skip adding the definition
        }else{
          fileFormat$labelValues[fileFormat$variableName==currentVar & fileFormat$FileNo==currentFileNo] <- paste(varCode, varDesc, sep="=", collapse="^")
        }
        
      }
    }
  }#end if(!is.null(varLineIdx) && length(varLineIdx)>0)
  
  #subset for the specific file number we need and also append the ID field as it's not present in the master.txt file
  fileFormat <- subset(fileFormat, fileFormat$FileNo %in% c(1,6)) #filter the file listing for only the derived (fileNo==1) and weight (fileNo==6) files
  
  fileFormat <- validateMultiLine_FileFormat(fileFormat) #ensure no spacing for each FileNo and RecordIndex within that FileNo
  
  #next we need to solve any duplicate name issues between the derived file (FileNo==1) and the weight file (FileNo==6)
  derNames <- fileFormat[fileFormat$FileNo==1, "variableName"]
  wgtNames <- fileFormat[fileFormat$FileNo==6, "variableName"]
  
  dupeNames <- wgtNames[wgtNames %in% derNames]
  dupeNameNew <- paste0(dupeNames , ".weight")
  
  if(!is.null(dupeNameNew) && length(dupeNameNew)>0){
    for(i in 1: length(dupeNameNew)){
      fileFormat[fileFormat$variableName==dupeNames[i] & fileFormat$FileNo==6, "variableName"] <- dupeNameNew[i]
    }
  }

  #for whatever reason the 'ID' value is not included in the master.txt file.  Specify it specifically here
  #it's also on each row of multi-line data but we will remove those in the main readBB function when we flatten the file.
  fileFormat[nrow(fileFormat)+1, ] = list("id", #variableName
                                          1,  #start
                                          8,  #end
                                          8,  #width
                                          "", #decimal
                                          NA, #DecimalInputFmt
                                          "Participant ID", #label
                                          "",  #labelValues
                                          "",  #type
                                          "",  #pvWt
                                          "integer", #dataType 
                                          "",  #weights
                                          1,   #fileNo
                                          1)   #recordIndex
  
  fileFormat <- fileFormat[order(fileFormat$FileNo, fileFormat$RecordIndex, fileFormat$Start, fileFormat$End), ]
  row.names(fileFormat) <- 1:nrow(fileFormat) #reset row.names
  
  return(fileFormat)
}

#returns a validated fwf file format
#1) checks for any missing numeric gaps between the start and end postions between each FileNo and RecordIndex and inserts 'xGAP' fields for LaF to operate properly
validateMultiLine_FileFormat <- function(fileFormat){
  
  origFF <- fileFormat
  newFF <- fileFormat[FALSE, ]
  
  fileNo <- sort(unique(origFF$FileNo))
  
  for(fn in fileNo){
    
    recIdx <- sort(unique(origFF[origFF$FileNo==fn, "RecordIndex"]))
    
    for(ri in recIdx){
      xFF <- origFF[origFF$FileNo==fn & origFF$RecordIndex==ri, ]
      
      #must add the id value for positions for each FileNo and RecordIndex to perform accurate check
      xFF[nrow(xFF)+1, ] = list("id", 1, 8, 8, "", "Participant ID", "", "", "", "integer", "", fn, ri) #fileNo 1
      
      xFF <- validateFWF_FileFormat(xFF) #this was originally used in the readECLS_K1998.R file and adapted here for use with multi-line .dat files
      
      #then remove the id field after the check is complete
      xFF <- xFF[xFF$variableName!="id", ]
      
      #if any 
      xFF$FileNo <- rep(fn, times=nrow(xFF))
      xFF$RecordIndex <- rep(ri, times=nrow(xFF))
      
      newFF <- rbind.data.frame(newFF, xFF, stringsAsFactors = FALSE)
    }
  }
  
  return(newFF)
}