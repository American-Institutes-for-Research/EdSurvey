#' @title PISA YAFS (Young Adult Follow-up Study) 
#'
#' @description Opens a connection to the Programme for International Student Assessment (PISA) YAFS 2016 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param datPath a character value of the file location where the data file (.dat) file is saved.
#' @param spsPath a character value of the file location where the SPSS (.sps) script file is saved to parse the \code{datPath} data file.
#' @param esdf_PISA2012_USA (optional) an \code{edsurvey.data.frame} of the USA PISA 2012 data if planning to analyze the PISA YAFS data alongside the USA PISA 2012 dataset.
#' 
#' @details Reads in the unzipped files for the PISA YAFS.  The PISA YAFS dataset is a follow-up study of a subset of the students who
#' participated in the PISA 2012 USA study.  It can be analyzed on its own as a singular dataset or optionally merged with the PISA 2012 USA data, 
#' in which case there will be two sets of weights in the merged dataset (the default PISA YAFS weights and the PISA 2012 USA weights).
#' 
#' @return An \code{edsurvey.data.frame} for the PISA YAFS dataset if the \code{esdf_PISA2012_USA} parameter is \code{NULL}.  If the PISA 2012 USA \code{edsurvey.data.frame} is specified for the \code{esdf_PISA2012_USA}
#' parameter, then the resulting dataset will return an \code{edsurvey.data.frame} allowing analysis for a combined dataset.
#'
#' @seealso \code{\link{readPISA}}
#' @author Tom Fink
#' @example man/examples/readPISA_YAFS.R
#' @export
readPISA_YAFS <- function(datPath = file.path(getwd(), "PISA_YAFS2016_Data.dat"),
                         spsPath = file.path(getwd(), "PISA_YAFS2016_SPSS.sps"),
                         esdf_PISA2012_USA = NULL) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  datPath <- suppressWarnings(normalizePath(unique(datPath), winslash = "/"))
  spsPath <- suppressWarnings(normalizePath(unique(spsPath), winslash = "/"))
  
  if(!file.exists(datPath)){
    stop(eout(paste0("Cannot find specified ", sQuote("datPath"), " file ", sQuote(datPath), ".")))
  }
  if(!file.exists(spsPath)){
    stop(eout(paste0("Cannot find specified ", sQuote("spsPath"), " file ", sQuote(spsPath), ".")))
  }
  
  if(is.null(esdf_PISA2012_USA) || is.na(esdf_PISA2012_USA)){
    esdf_PISA2012_USA <- NULL
    hasPISA2012_USA <- FALSE
  }else{
    
    checkDataClass(esdf_PISA2012_USA, c("edsurvey.data.frame"))
    
    if(esdf_PISA2012_USA$survey!="PISA"){
      stop(paste0("The argument ", sQuote("esdf_PISA2012_USA"), " must be for the 2012 United States of American PISA survey."))
    }
    
    if(esdf_PISA2012_USA$country!="United States of America"){
      stop(paste0("The argument ", sQuote("esdf_PISA2012_USA"), " must be for the 2012 United States of American PISA survey."))
    }
    
    if(esdf_PISA2012_USA$year!="2012"){
      stop(paste0("The argument ", sQuote("esdf_PISA2012_USA"), " must be for the 2012 United States of American PISA survey."))
    }
    
    hasPISA2012_USA <- TRUE
  }
  
  fileFormat <- parseSPSS_PISA_YAFS(spsPath)
  fileFormat <- validateFWF_FileFormat(fileFormat) #ensure no FWF spacing gaps
  
  lafObj <- laf_open_fwf(datPath, fileFormat$dataType, fileFormat$Width, column_names = fileFormat$variableName)
  
  #validate LaF can read data
  tryCatch(lafObj[1,],
           error=function(e) {
             close(lafObj)
             stop(e)
           })
  
  fileFormat <- identifyPISA_YAFS_Weights(fileFormat)
  
  weights <- buildPISA_YAFS_WeightList(fileFormat)
  attr(weights, "default") <- "w_yfstuwt"
  
  fileFormat <- identifyPVVARS_PISA_YAFS(fileFormat)
  pvs <- buildPVVARS_PISA_YAFS(fileFormat)
  attr(pvs, "default") <- "lit"
  
  omittedLevels <- c("NA", "N/A", "Not Applicable", "Invalid", "No Response", NA, "(Missing)", "ESO non-respondents", "Valid Skip", "Not Answered")
  
  #achievement level definition
  achList <- list()
  achLevel <- c(176, 226, 276, 326, 376)
  names(achLevel) <- c("Proficiency Level 1", "Proficiency Level 2", "Proficiency Level 3", "Proficiency Level 4", "Proficiency Level 5")
  achList$Literacy <- achLevel
  achList$Numeracy <- achLevel
  
  subj <- c("Literacy", "Numeracy")
  surveyName <- "PISA YAFS"
  
  if(hasPISA2012_USA){
    subj <- c(subj, esdf_PISA2012_USA$subject)
    
    dataList <- buildPISA_YAFS_DataListMerged(lafObj, fileFormat, esdf_PISA2012_USA)
    
    #adjust other details about the edsurvey.data.frame to accomodate both datasets
    weights <- c(weights, esdf_PISA2012_USA$weights)
    attr(weights, "default") <- "w_yfstuwt"
    
    pvs <- c(pvs, esdf_PISA2012_USA$pvvars)
    attr(pvs, "default") <- "lit"
    
    achList <- c(achList, esdf_PISA2012_USA$achievementLevels)
    
    psuVar <- esdf_PISA2012_USA$psuVar
    stratumVar <- esdf_PISA2012_USA$stratumVar
    
    omittedLevels <- unique(c(omittedLevels, esdf_PISA2012_USA$omittedLevels))
    
    validateFactorLabels <- TRUE
    
    surveyName <- "PISA YAFS merged w/ USA PISA 2012"
  }else{
    dataList <- buildPISA_YAFS_DataList(lafObj, fileFormat) #PISA YAFS Only
    
    psuVar <- NULL
    stratumVar <- NULL
    
    validateFactorLabels <- FALSE
  }
  
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = subj,
                      year = "2016",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = achList, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = surveyName,
                      country = "USA",
                      psuVar = psuVar,
                      stratumVar = stratumVar, 
                      jkSumMultiplier = 0.05, #same as PISA specification
                      validateFactorLabels = validateFactorLabels, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyPISA_YAFS_Weights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #identify weight vars
  wgtVars <- grep("^W_YFSTUWT$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildPISA_YAFS_WeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(xWgt in wgtVars){
    tempVar <- tolower(substr(xWgt,1, 6)) #grab the left 6 characters
    
    wgtPattern = paste0("^", tempVar, "R\\d+$")
    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- ujkz[ujkz != tempVar] #remove the weight value itself from the replicates
    
    ujkz <- gsub(tempVar, "", ujkz, ignore.case = TRUE) #strip away and leave just the numeric variable name ending as a string
    ujkz <- gsub("R", "", ujkz, ignore.case = TRUE)
    
    if(length(ujkz)>0){
      tmpWgt <- list()
      tmpWgt[[1]] <- list(jkbase=paste0(tempVar, "r"), jksuffixes=as.character(ujkz))
      names(tmpWgt)[[1]] <- xWgt
      weights <- c(weights,tmpWgt)
    }
  }

  return(weights)
}

buildPISA_YAFS_DataList <- function(dataLaF, fileFormat){
  
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

buildPISA_YAFS_DataListMerged <- function(dataLaF, fileFormat, esdf_pisa2012_USA){
  
  dataList <- list()
  
  #fileformat columns are different between PISA USA 2012 (14 cols) and PISA YAFS (11 cols) from the read-in
  #normalize them otherwise errors occur in getData
  
  pisaDL <- esdf_pisa2012_USA$dataList[[1]]
  pisaDL$levelLabel <- "PISA2012_USA"
  
  pFF <- pisaDL$fileFormat
  pFF <- pFF[colnames(fileFormat)]
  
  #fix issue for having 'repeat' as a variable name which is a reserved keyword in R.
  #this gets automatically fixed in the LaF package to a variable name of 'repeat.' so this corrects that issue!
  pFF$variableName[pFF$variableName %in% c("repeat")] <- paste0(pFF$variableName[pFF$variableName %in% c("repeat")], ".")
  
  pisaDL$fileFormat <- pFF
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["PISA2012_USA"]] <- pisaDL
  
  dataList[["PISA_YAFS"]] <- dataListItem(lafObject = dataLaF,
                                             fileFormat = fileFormat,
                                             levelLabel = "PISA_YAFS",
                                             forceMerge = FALSE,
                                             parentMergeLevels = c("PISA2012_USA", "PISA2012_USA"),
                                             parentMergeVars = c("schoolid", "stidstd"),
                                             mergeVars = c("schoolid", "stidstd"),
                                             ignoreVars = NULL, #only overlapping variables are the merge variables so you don't need to specify anything
                                             isDimLevel = FALSE)
  
  return(dataList)
}

identifyPVVARS_PISA_YAFS <- function (fileFormat){
  
  #find the index postions of the PVVAR variables
  pvIdx <- which(grepl("^PV[0-9]+(LIT|NUM)$", fileFormat$variableName, ignore.case = TRUE), arr.ind = TRUE)
  
  #pull out the weight (1 to 10) value
  wtVal <- gsub("[A-z]", "", fileFormat$variableName[pvIdx], ignore.case = TRUE)
  
  fileFormat$pvWt[pvIdx] <- wtVal
  
  pvvar <- tolower(substr(fileFormat$variableName[pvIdx], nchar(fileFormat$variableName[pvIdx])-2, nchar(fileFormat$variableName[pvIdx])))
  
  fileFormat$Type[pvIdx] <- pvvar
  
  return(fileFormat)
}

#builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_PISA_YAFS <- function(fileFormat, defaultPV = "lit"){
  
  pvFields <- subset(fileFormat, nchar(fileFormat$Type)>0) #type is identified in identifyPVVARS_PISA_YAFS function
  constructs <- unique(pvFields$Type)
  pvvars <- vector("list", length(constructs))
  names(pvvars) <- constructs
  
  for(i in names(pvvars)){
    varList <- tolower(pvFields$variableName[pvFields$Type == i]) #don't sort here to keep them in 1-10 order (as ordered in the data)
    
    achLevel <- c(176, 226, 276, 326, 376)
    names(achLevel) <- c("Proficiency Level 1", "Proficiency Level 2", "Proficiency Level 3", "Proficiency Level 4", "Proficiency Level 5")
    
    pvvars[[i]] <- list(varnames=varList, achievementLevel=achLevel)
  }
  
  #test if defaultPV in the list and make it default::otherwise set it to the first pvvar in the list
  if (defaultPV %in% names(pvvars)){
    attr(pvvars, "default") <- defaultPV
  }else{
    attr(pvvars, "default") <- names(pvvars)[1]
  }
  
  return (pvvars)
}

#parses the SPSS script accompanying the PISA YAFS fixed-width data file into a formated fileFormat object
parseSPSS_PISA_YAFS <- function(spssFP){
  
  if(!file.exists(spssFP)){
    stop(paste0("Unable to locate ", dQuote("spssFP"), " file at location: ", dQuote(spssFP)))
  }
  
  lines <- readLines(spssFP)
  lines <- lines[!(nchar(trimws(lines, which = "both"))==0)] #remove empty lines
  
  functionStartRow<- list()
  functionStartRow$fileHandle <- which(grepl("^FILE HANDLE", lines, ignore.case = TRUE), arr.ind = TRUE)
  functionStartRow$dataList <- which(grepl("^DATA LIST", lines, ignore.case = TRUE), arr.ind = TRUE)
  functionStartRow$variableLabels <- which(grepl("^VARIABLE LABELS", lines, ignore.case = TRUE), arr.ind = TRUE)
  functionStartRow$missingValues <- which(grepl("^MISSING VALUES", lines, ignore.case = TRUE), arr.ind = TRUE)
  functionStartRow$valueLabels <- which(grepl("^VALUE LABELS", lines, ignore.case = TRUE), arr.ind = TRUE)

  functionPart<- list()
  functionPart$fileHandle <- getSPSS_PISA_YAFS_Part(lines, functionStartRow$fileHandle)
  functionPart$dataList <- getSPSS_PISA_YAFS_Part(lines, functionStartRow$dataList)
  functionPart$variableLabels <- getSPSS_PISA_YAFS_Part(lines, functionStartRow$variableLabels)
  functionPart$missingValues <- getSPSS_PISA_YAFS_Part(lines, functionStartRow$missingValues)
  functionPart$valueLabels <- getSPSS_PISA_YAFS_Part(lines, functionStartRow$valueLabels)
  
  #get a list of each individual parts pieces of info
  dl <- parseSPSS_PISA_YAFS_DataList(functionPart$dataList)
  varLbl <- parseSPSS_PISA_YAFS_VariableLabels(functionPart$variableLabels)
  valLbl <- parseSPSS_PISA_YAFS_ValueLabels(functionPart$valueLabels)

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
               "weights" = character(0))
  
  dict$variableName <- dl$variableName
  dict$Start <- dl$Start
  dict$End <- dl$End
  dict$Width <- dl$Width
  
  i <- 1
  for(var in dict$variableName){
    
    xLbl <- varLbl$varDesc[tolower(varLbl$variableName)==tolower(var)]
    xValLbl <- valLbl$valLblDesc[tolower(valLbl$variableName)==tolower(var)]
    xAttr <- dl$Attributes[tolower(dl$variableName)==tolower(var)]
    xAttr <- gsub("(", "", xAttr, fixed = TRUE)
    xAttr <- gsub(")", "", xAttr, fixed = TRUE)
    
    dict$Labels[[i]] <- ifelse(length(xLbl)>0, xLbl, "")
    dict$labelValues[[i]] <- ifelse(length(xValLbl)>0, xValLbl, "")
    
    if(grepl("^[0-9]+$", xAttr)){ #a numeric digit here indicates it's decimal formatting, meaning it's a decimal value
      dict$Decimal[[i]] <- as.numeric(xAttr)
      dict$dataType[[i]] <- "numeric"
    }else if(grepl("^A$",xAttr, ignore.case = TRUE)){ #the 'A' tells you it's alphanumeric/character value
      dict$Decimal[[i]] <- NA
      dict$dataType[[i]] <- "character"
    }else{ #otherwise the default is integer
      dict$Decimal[[i]] <- 0
      dict$dataType[[i]] <- "integer"
    }
    
    i <- i + 1
  }#end for(var in dict$variableName)
  
  dict$Type <- rep("", times = length(dict$variableName)) #default
  dict$pvWt <- rep("", times = length(dict$variableName)) #default
  dict$weights <- rep(FALSE, times = length(dict$variableName)) #default value::to be calced later
              
  return(data.frame(dict, stringsAsFactors = FALSE))        
}

getSPSS_PISA_YAFS_Part <- function(lines, startLineNo){
  
  linePart <- lines[startLineNo:length(lines)]
  
  endPositions <- grepl("[.]$", linePart)
  endPos <- which(endPositions, arr.ind = TRUE)[1]
  
  return(linePart[1:endPos])
}

parseSPSS_PISA_YAFS_DataList <- function(lines){
  
  dl <- list(variableName = character(0),
             Start = integer(0),
             End = integer(0),
             Width = integer(0),
             Attributes = character(0))
  
  fullStr <- trimws(paste0(lines, collapse = " "), which = "both") #easier to work with one large string vs an array in this context
  
  argItems <- unlist(strsplit(fullStr, "/", fixed = TRUE)) #split by '/' character.  creates vector of 2 items.  first item is the function call; second item are the arguments
  valParts <- unlist(strsplit(trimws(argItems[2], which="both"), " ", fixed = TRUE))  #get the function arguments and parse them into individual pieces
  valParts <- valParts[!(nchar(trimws(valParts, which = "both"))==0)]
  
  varIdx <- which(grepl("^[A-z].*", valParts, ignore.case = TRUE), arr.ind = TRUE)
  
  vars <- tolower(valParts[varIdx])
  pos <- valParts[varIdx+1]
  attr <- rep("", times=length(vars))
  
  #gather the attributes as applicable for the variable
  iPos <- 1 #need incremental index for assigning back to the 'attr' vector
  for(i in varIdx){
    
    x <- i + 2 #attributes are 3rd agument after the variable name in the sequence. these are always contained in parenthesis: "(A)" or "(6)" for example
    
    if(x <= length(valParts)){
      testVal <- valParts[x]
      
      if(grepl("^[(].*[)]", testVal)){
        attr[iPos] <- grep("^[(].*[)]", testVal, value = TRUE) #only grabs the parenthesis and the value inside the parens
      }
    }else{
      attr[iPos] <- ""
    }
    
    iPos <- iPos + 1
  }#end for(i in varIdx)
  
  attr <- gsub(".","",attr, fixed = TRUE) #remove any periods, only really applicable for the last item if next to the '.' terminator
  
  #clean up the start/end positions and calculate the width of the field
  sp <- rep(-1, times=length(vars))
  ep <- rep(-1, times=length(vars))
  
  posTemp <- strsplit(pos,"-", fixed = TRUE)
  
  for(i in seq_along(posTemp)){
    if(length(posTemp[[i]]) > 1){
      sp[i] <- as.numeric(posTemp[[i]][1])
      ep[i] <- as.numeric(posTemp[[i]][2])
    }else{
      sp[i] <- as.numeric(posTemp[[i]][1])
      ep[i] <- as.numeric(posTemp[[i]][1])
    }
  }
  
  #build the output list
  dl$variableName <- tolower(vars)
  dl$Start <- sp
  dl$End <- ep
  dl$Width <- (ep-sp) + 1
  dl$Attributes <- attr
  
  return(dl)
}

parseSPSS_PISA_YAFS_VariableLabels <- function(lines){
  
  varLbl <- list(variableName = character(0),
                 varDesc = character(0))
  
  parts <- trimws(lines, which="both")
  
  parts <- parts[!grepl("^VARIABLE LABELS",parts)]
  
  #splitting by the double-quote character of the line as each line is it's own variable
  subParts <- strsplit(parts, "\"", fixed = TRUE)
  vars <- rep("", times = length(subParts))
  desc <- rep("", times = length(subParts))
  
  for(i in seq_along(subParts)){
    vars[i] <- tolower(trimws(subParts[[i]][1], which = "both"))
    desc[i] <- paste(trimws(subParts[[i]][-1], which = "both"), collapse = "")
  }
  
  #prep output
  varLbl$variableName <- vars
  varLbl$varDesc <- desc
  
  return(varLbl)
}

parseSPSS_PISA_YAFS_ValueLabels <- function(lines){
  
  valLbl <- list(variableName = character(0),
                 valLblDesc = character(0))
  
  parts <- lines[!grepl("^VALUE LABELS",lines)]
  
  fullStr <- paste(parts, collapse = "|@*@|") #picked this delimiter as it's highly unlikely to be included as a value label or variable name
  
  subParts <- strsplit(fullStr, "   /") #the '/' character denotes a separate assignment definition item so split on that, must have three spaces before the '/'
  subParts <- subParts[[1]] #only on item, that will have 1 to n character vectors
  
  #need index for applying labels to the list as there can be multiple variables assigned to the same valueLabel definition
  iLbl <- 1
  
  for(i in seq_along(subParts)){
    strVal <- subParts[i]
    strVal <- unlist(strsplit(strVal, "|@*@|", fixed = TRUE)) #resplit by this to get each line separately
    
    lblDefIdx <- which(grepl("\\d.*[\"].*[\"]",strVal),arr.ind = TRUE)
    
    lblDef <- strVal[lblDefIdx]
    varDef <- strVal[-lblDefIdx]
    
    varDef <- paste(trimws(varDef, which = "both"), collapse = " ") #merge all the lines together as one string
    varDef <- unlist(strsplit(varDef, " ", fixed = TRUE)) #split the joined string into each token
    varDef <- varDef[!(nchar(varDef)==0)] #remove any empty tokens; only the var names remain
    
    lblDef <- strsplit(lblDef, "\"", fixed = TRUE)
    
    val <- rep(-1, times=length(lblDef))
    txt <- rep("", times=length(lblDef))
    
    testVal <- trimws(lblDef[[1]][1], which = "both") #check the first element of the lblDef to see if the value argument has a value or is empty.  empty signifies it's a character value
    if(nchar(testVal)>0 && grepl("^[0-9]*$",testVal,ignore.case = TRUE)){
      
      for(j in seq_along(lblDef)){
        val[j] <- as.numeric(trimws(lblDef[[j]][1]))
        txt[j] <- trimws(paste(lblDef[[j]][-1], sep="",collapse = ""))
      }
    }else{
      for(j in seq_along(lblDef)){
        
        lblDef[[j]] <- lblDef[[j]][!(nchar(trimws(lblDef[[j]]))==0)] #remove any empty tokens
        
        val[j] <- trimws(lblDef[[j]][1])
        txt[j] <- trimws(paste(lblDef[[j]][-1], sep="",collapse = ""))
      }#end for(j in seq_along(lblDef)
    }#end else if(nchar(testVal)>0 && grepl("^[0-9]*$",testVal,ignore.case = TRUE))
    
    #loop through all the item variables and paste in the valueLabel definition to flatten it
    for(xVar in varDef){
      valLbl$variableName[[iLbl]] <- tolower(xVar)
      valLbl$valLblDesc[[iLbl]] <- paste(val, txt, sep = "=", collapse = "^")
      
      iLbl <- iLbl + 1 #increment the label index
    }
  }#end if(nchar(testVal)>0 && grepl("^[0-9]*$",testVal,ignore.case = TRUE))
  
  
  return(valLbl)
}