#' @title Connect to B&B 2000--2001 Data
#'
#' @description Opens a connection to a Baccalaureate & Beyond 2000--2001 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param b01DER_Filepath a character value to the main study-derived analytical data file (\code{B01DER.dat}).  Located within the ECBW/Data Folder.
#' @param b01WT_FilePath a character value to the study weight data file (\code{B01WT.dat}). Located within the \code{ECBW/Data} Folder.
#' @param masterTxtFilepath a character value of the master format file (\code{master.txt}) that contains the metadata 
#'                          for the \code{b01DER_Filepath} and \code{b01WT_FilePath}.  Located in the \code{ECBW} folder.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose output while the \code{readBB_2001} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#'                    
#' @details Reads the \code{masterTxtFilepath} file to parse the \code{b01DER_Filepath} and \code{b01WT_FilePath} files to an \code{edsurvey.data.frame}.
#'          This function creates two cached data files will be saved in the same directory and filename as the \code{b01DER_Filepath} file for the B&B 2000--2001 longitudinal survey.
#'          The two cached data files will have file extensions of .txt and .meta.
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the B&B 2000--2001 longitudinal dataset.
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readBB_2001.R
#' 
readBB_2001 <- function(b01DER_Filepath,
                        b01WT_FilePath,
                        masterTxtFilepath,
                        forceReread=FALSE,
                        verbose=TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  b01DER_Filepath <- suppressWarnings(normalizePath(unique(b01DER_Filepath), winslash = "/"))
  b01WT_FilePath <- suppressWarnings(normalizePath(unique(b01WT_FilePath), winslash = "/"))
  masterTxtFilepath <- suppressWarnings(normalizePath(unique(masterTxtFilepath), winslash = "/"))
  
  if(!file.exists(b01DER_Filepath)){
    stop(paste0("Cannot find specified data file ", sQuote("b01DER_Filepath"), " in path ", sQuote(file.path(b01DER_Filepath)), "."))
  }
  if(!file.exists(b01WT_FilePath)){
    stop(paste0("Cannot find specified data file ", sQuote("b01WT_FilePath"), " in path ", sQuote(file.path(b01WT_FilePath)), "."))
  }
  if(!file.exists(masterTxtFilepath)){
    stop(paste0("Cannot find specified data file ", sQuote("masterTxtFilepath"), " in path ", sQuote(file.path(masterTxtFilepath)), "."))
  }

  cacheFilename <- gsub("\\.dat$", ".txt", b01DER_Filepath, ignore.case = TRUE)
  metaFilename <- gsub("\\.dat$", ".meta", b01DER_Filepath, ignore.case = TRUE)
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFilename)){
    if(file.exists(metaFilename)){
      
      cacheRDS <- readRDS(metaFilename) #get the cache info from the .meta file
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "B&B2001")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }
  
  omittedLevels <- c("{Don't know}", "{Refuse}", "{Refused}", "{Not reached}",
                     "{Missing}", "{Skipped}", "{CATI error}",
                     "{Out of range}", "{Uncodeable}", "{Not applicable}",
                     "{Accepted to program but hasn't started}",
                     "{at least one zipcode was -1}", "{at least one zipcode was -2}",
                     "{at least one zipcode was out of the US}",
                     "{at least one zipcode was bad data}",
                     "{at least one zipcode was not found}",
                     "{at least one zipcode was military code}",
                     "{at least one zipcode was missing}",
                     "{Does not plan to complete this degree}",
                     "{Not living in USA}", "{One time event}",
                     "{Missing-CATI error}", "{Pass/fail}",
                     "{No grades awarded}", "{Bad or missing school code}",
                     "{No match PSS}", "{No match, PSS}","{No match, CCD}", "{Match but value missing in CCD/PSS}",
                     "{Not degree granting/unknown}",
                     "(Missing)", NA)
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    if(verbose){
      cat(paste0("Processing master text file.\n"))
    }
    
    fileFormat <- parseBBTxtMaster_2001(masterTxtFilepath) #get the file format from the master.txt file
    fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)] #recalc the start/end for a flattened file
    fileFormat$End <- cumsum(fileFormat$Width) 
    
    maxRecIndex <- max(fileFormat$RecordIndex)
    
    #the .dat file is a multi-line file that needs to be flattened
    if(verbose && maxRecIndex > 1){
      cat(paste0("Flattening multi-line .dat file.\n"))
    }
    if(maxRecIndex>1){
      if(file.exists(cacheFilename)){
        file.remove(cacheFilename)
      }
      
      #write the file out to the cache location
      derivedConn <- tryCatch({file(b01DER_Filepath, "r")},
                          error = function(e){
                            stop(paste0("Unable to read data file.\n Please ensure you have read permissions to the read path: ", sQuote(b01DER_Filepath), "\n",
                                        "Error message: ", e))
                          })
      
      writeConnection <- tryCatch({file(cacheFilename, "w")},
                          error = function(e){
                            stop(paste0("Unable to write to cache file.\n Please ensure you have write permissions to the save path: ", sQuote(cacheFilename), "\n",
                                        "Error message: ", e))
                          })
      
      wgtData <- readLines(b01WT_FilePath)
      wgtDataID <- substr(wgtData, 1, 8)
      
      offset <- 9 #the ID value is contained on each row of data, we want to offset lines after 1 to start at column position 9
      
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
        writeLines(writeStr, writeConnection)
      }
    }
    
    close(writeConnection)
    close(derivedConn)
    
    fileFormat <- valueLabelCleanupFF(fileFormat, omittedLevels, c("{Zero}", "{zero}"))
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, metaFilename)
    
  }#end if(runProcessing==TRUE)
  
  lafObj <- laf_open_fwf(cacheFilename, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)
  
  fileFormat <- identifyBBWeights_2001(fileFormat)
  weights <- buildBBWeightList_2001(fileFormat)
  attributes(weights)$default <- "bb01awt" #set default weight
  
  pvs <- list() #no plausible values or achievement levels
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildBBDataList_2001(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2000-2001",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "B&B2001",
                      country = "USA",
                      psuVar = "bb01apsu",  #psu and stratum are weight specific
                      stratumVar = "bb01astr", 
                      jkSumMultiplier = 0.015625, #1/64 replicates
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBBWeights_2001 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight variables
  wgtVars <- grep("^bb01awt$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBBWeightList_2001 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
    baseWgtVar <- "bb1brr"
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

buildBBDataList_2001 <- function(lafObj, fileFormat){
  
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
parseBBTxtMaster_2001 <- function(masterTxtFilepath){
  
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
  fileFormat <- subset(fileFormat, fileFormat$FileNo %in% c(1,9)) #filter the file listing for only the derived (fileNo==1) and weight (fileNo==9) files
  maxLineRec <- max(fileFormat$RecordIndex)
  
  #for whatever reason the 'ID' value is not included in the master.txt file.  Specify it specifically here
  #it's also on each row of multi-line data but we will remove those in the main readBB function when we flatten the file.
  fileFormat[nrow(fileFormat)+1, ] = list("id", 1, 8, 8, "", "Participant ID", "", "", "", "integer", "", 1, 1) #fileNo 1
  
  fileFormat <- fileFormat[order(fileFormat$FileNo, fileFormat$RecordIndex, fileFormat$Start), ]
  row.names(fileFormat) <- 1:nrow(fileFormat) #reset row.names
  
  return(fileFormat)
}


