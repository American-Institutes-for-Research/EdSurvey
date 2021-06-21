#' @title Beginning Postsecondary Students (BPS:90/94) Data
#'
#' @description Opens a connection to a BPS 1990 to 1994 cohort data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param b94DAT_Filepath a character value of the file path to the student BPS:90/94 (.dat) file
#' @param masterTxtFilepath a character value of the file path to the \code{master.txt} file contained within the electronic codebook directory
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output while the \code{readHSB_Sophomore} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#' @details Reads in the student BPS 1990 to 1994 cohort data file to an \code{edsurvey.data.frame}.
#' 
#' @return An \code{edsurvey.data.frame} for the BPS longitudinal dataset.
#'
#' @seealso \code{\link{readBB_2001}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Sun-joo Lee
#' @example man/examples/readBPS_1994.R
#' 
readBPS_1994 <- function(b94DAT_Filepath,
                         masterTxtFilepath,
                         forceReread=FALSE,
                         verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  b94DAT_Filepath <- suppressWarnings(normalizePath(unique(b94DAT_Filepath), winslash = "/"))
  masterTxtFilepath <- suppressWarnings(normalizePath(unique(masterTxtFilepath), winslash = "/"))
  
  if(!file.exists(b94DAT_Filepath)){
    stop(paste0("Cannot find specified data file ", sQuote("b94DAT_Filepath"), " in path ", sQuote(file.path(b94DAT_Filepath)), "."))
  }
  if(!file.exists(masterTxtFilepath)){
    stop(paste0("Cannot find specified data file ", sQuote("masterTxtFilepath"), " in path ", sQuote(file.path(masterTxtFilepath)), "."))
  }
  
  cacheFilename <- gsub("\\.dat$", ".txt", b94DAT_Filepath, ignore.case = TRUE)
  metaFilename <- gsub("\\.dat$", ".meta", b94DAT_Filepath, ignore.case = TRUE)
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFilename)){
    if(file.exists(metaFilename)){
      
      cacheRDS <- readRDS(metaFilename) #get the cache info from the .meta file
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "BPS1994")){
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
    if(verbose){
      cat(paste0("Processing master text file.\n"))
    }
    
    fileFormat <- parseBPSTxtMaster_94(masterTxtFilepath) #get the file format from the master.txt file
    
    #the ldate field seems odd that its not defined as a character in the master.txt file, but really is
    fileFormat[fileFormat$variableName=="ldate", ]$dataType <- "character"
    fileFormat[fileFormat$variableName=="ldate", ]$labelValues <- ""
    
    #the number of rows per record
    maxRecIndex <- max(fileFormat$RecordIndex)
    
    #recalc the start/end positions for the final merged file
    fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)] #recalc the start/end for a flattened file
    fileFormat$End <- cumsum(fileFormat$Width) 
    
    #the .dat file is a multi-line file that needs to be flattened
    if(verbose && maxRecIndex > 1){
      cat(paste0("Flattening multi-line .dat file.\n"))
    }
    if(maxRecIndex > 1){
      if(file.exists(cacheFilename)){
        file.remove(cacheFilename)
      }
      
      derivedConn <- tryCatch({file(b94DAT_Filepath, "r")},
                              error = function(e){
                                stop(paste0("Unable to read data file.\n Please ensure you have read permissions to the read path: ", sQuote(b94DAT_Filepath), "\n",
                                            "Error message: ", e))
                              })
      
      writeConnection <- tryCatch({file(cacheFilename, "w")},
                               error = function(e){
                                 stop(paste0("Unable to write to cache file.\n Please ensure you have write permissions to the save path: ", sQuote(cacheFilename), "\n",
                                             "Error message: ", e))
                               })
      
      offset <- 9 #the ID value is contained on each row of data, we want to offset lines after 1 to start at column position 8
      
      while(TRUE){
        linePart <- readLines(derivedConn, maxRecIndex) #get the number of lines as a group for each flattened row
        #be sure to exit once done
        if(length(linePart)==0){
          break
        }
        
        linePart[-1] <- substr(linePart[-1], offset, nchar(linePart[-1])) #trim off the duplicate ID values for all lines except the first
        
        writeStr <- paste0(linePart, collapse = "")
        writeLines(writeStr, writeConnection)
      }
    }
    
    close(writeConnection)
    close(derivedConn)
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, metaFilename)
    
  }#end if(runProcessing==TRUE)
  
  lafObj <- laf_open_fwf(cacheFilename, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)
  
  fileFormat <- identifyBPSWeights_94(fileFormat)
  
  weights <- buildBPSWeightList_94(fileFormat)
  
  attributes(weights)$default <- "bps94awt" #set default weight
  
  pvs <- list() #no plausible values or achievement levels
  omittedLevels <- c("{Missing, legitimate skip}", "{Missing, blank}", "{Missing, insufficient data to compute}", 
                     "{Missing: not applicable}", "{Missing: Q not reached}", "{MISSING: NOT APPLICABLE}",               
                     "{Missing, don't know}", "{Legitimate skip}", "{Don't know}",                            
                     "{Missing}", "{Refused}", "{Missing, unavailable}",                              
                     "{Missing, refuse}", "{Missing, not reached}", "{Missing, BPS:94 var., no BPS:92 equiv.}",
                     "{Missing, BPS:92 var., no BPS:94 equiv.}", "{Missing, refused}", "{Uncodeable/uncoded}", 
                     NA, "(Missing)")
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildBPSDataList_94(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "1990-1994",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "BPS1994",
                      country = "USA",
                      psuVar = NULL,  #psu and stratum are weight specific
                      stratumVar = NULL, 
                      jkSumMultiplier = 1, #has 35 replicate weights, but sum multiplier is set to 1
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
  
}




#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBPSWeights_94 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight variables
  wgtVars <- c("bps92cwt", "bps92lwt", "bps92nwt", "bps94awt")
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}


#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBPSWeightList_94 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  wgtLookupDF <- data.frame(wgt=wgtVars, 
                            jkBase=rep("", times=length(wgtVars)), 
                            psuVar=rep("", times=length(wgtVars)), 
                            stratumVar=rep("", times=length(wgtVars)), 
                            stringsAsFactors = FALSE)
  
  wgtLookupDF[wgtLookupDF$wgt=="bps92cwt", "jkBase"] <- "bps92c"
  wgtLookupDF[wgtLookupDF$wgt=="bps92lwt", "jkBase"] <- "bps92l"
  wgtLookupDF[wgtLookupDF$wgt=="bps92nwt", "jkBase"] <- "bps92c" ### not certain -- check again
  wgtLookupDF[wgtLookupDF$wgt=="bps94awt", "jkBase"] <- "bps94w"
  
  wgtLookupDF[wgtLookupDF$wgt=="bps92cwt", "psuVar"] <- "psu"
  wgtLookupDF[wgtLookupDF$wgt=="bps92lwt", "psuVar"] <- "psu"
  wgtLookupDF[wgtLookupDF$wgt=="bps92nwt", "psuVar"] <- "psu"
  wgtLookupDF[wgtLookupDF$wgt=="bps94awt", "psuVar"] <- "psu"
  
  wgtLookupDF[wgtLookupDF$wgt=="bps92cwt", "stratumVar"] <- "ofcon2"
  wgtLookupDF[wgtLookupDF$wgt=="bps92lwt", "stratumVar"] <- "ofcon2"
  wgtLookupDF[wgtLookupDF$wgt=="bps92nwt", "stratumVar"] <- "ofcon2"
  wgtLookupDF[wgtLookupDF$wgt=="bps94awt", "stratumVar"] <- "ofcon2"
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
    
    baseWgtVar <- wgtLookupDF$jkBase[wgtLookupDF$wgt==tempVar]
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


buildBPSDataList_94 <- function(lafObj, fileFormat){
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
parseBPSTxtMaster_94 <- function(masterTxtFilepath){
  
  #prepare return dictionary
  dict <- list("variableName" = character(0),
               "Start" = integer(0),
               "End" = integer(0),
               "Width" = integer(0),
               "Decimal" = integer(0),
               "Labels" = character(0),
               "labelValues" = character(0),
               "Type" = character(0),
               "pvWt" = character(0),
               "dataType" = character(0),
               "weights" = character(0),
               "FileNo" = character(0),
               "RecordIndex" = character(0))
  
  lines <- readLines(masterTxtFilepath, warn=FALSE)
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
          xDesc <- gsub("^", "'", xDesc, fixed=TRUE) #replace any carrots with single quotes
          
          if(xCode=="0" && xDesc=="{zero}"){
            #skip 0={zero} value label as it causes isusses in getData function
          }else{
            varCode <- c(varCode, xCode)
            varDesc <- c(varDesc, xDesc)
          }
        }
      } #end for(j in currentLine:endLine)
      
      if(length(varCode)>0){
        if(length(varCode)==1 && varCode[1]==0 && length(varDesc)==1 && varDesc[1]=="{zero}"){
          #if the only varcode and vardesc is 0={zero} then we want to skip adding the definition
        }else{
          
          #check if the codes are all numeric or if they are alphanumeric
          if(any(is.na(suppressWarnings(as.numeric(varCode))))){
            fileFormat$dataType[fileFormat$variableName==currentVar & fileFormat$FileNo==currentFileNo] <- "character"
          }
          
          fileFormat$labelValues[fileFormat$variableName==currentVar & fileFormat$FileNo==currentFileNo] <- paste(varCode, varDesc, sep="=", collapse="^")
        }
      }
    }
  }#end if(!is.null(varLineIdx) && length(varLineIdx)>0)
  
  #subset for the specific file number we need and also append the ID field as it's not present in the master.txt file
  fileFormat <- subset(fileFormat, fileFormat$FileNo == 1) #filter the file listing for only the derived file (STUDENT.DAT)
  
  #ensure no spacing for each FileNo and RecordIndex within that FileNo
  fileFormat <- validateMultiLine_FileFormat(fileFormat) 
  
  #for whatever reason the 'ID' value is not included in the master.txt file.  Specify it specifically here
  #it's also on each row of multi-line data but we will remove those in the main readBB function when we flatten the file.
  fileFormat[nrow(fileFormat)+1, ] = list("id", 1, 8, 8, "", "Participant ID", "", "", "", "integer", "", 1, 1) #fileNo 1
  
  fileFormat <- fileFormat[order(fileFormat$FileNo, fileFormat$RecordIndex, fileFormat$Start, fileFormat$End), ]
  row.names(fileFormat) <- 1:nrow(fileFormat) #reset row.names
  
  return(fileFormat)
}
