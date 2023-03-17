
#parses the NAEP XML layout file and returns a list object of the following items:
#1) 'Contents' - A list of data.frames containing all of the XML data in an easy to use format of data.frames
#2) 'FileFormat' - An 'EdSurvey' fileFormat data.frame that mirrors a fileFormat data.frame from an .fr2 file
#3) 'XML' - The XML node structure as originally parsed from the XML layout file with the xml2 package
parseNAEP_XML <- function(filename, mmlScoreDict = defaultNAEPScoreCard()) {
  if(!file.exists(filename)){
    stop(paste0("Specified XML File Not Found: ", filename))
  }
  
  #read in the text using readLines first as raw.  
  #if read_xml is used with a filepath it can cause parsing errors for these specific XML files.
  txt <- readLines(filename) #read in as raw text
  txt <- paste0(txt, collapse = "\n")
  
  #ASCII encoding for all NAEP XML layout files
  xml <- read_xml(txt, encoding = "ASCII", as_html = FALSE, options = c("RECOVER", "NOBLANK"))
  catalog <- as_list(xml_find_all(xml, "//NAEPDataCatalog")) #main/root node here, must be only be one entry per file
  
  #check catalog length
  if(length(catalog) < 1){
    stop(paste0("No NAEP Data Catalog Entry Found in File: ", filename))
  }else if(length(catalog) > 2){
    stop(paste0("Multiple NAEP Data Catalog Entry Found in File: ", filename, "\n\n", "Only 1 is expected."))
  }
  
  catalog <- catalog[[1]] #will only be one entry for the 'NAEPDataCatalog'
  
  #list object to hold information at the entire file level
  dataFileDetail <- attributes(catalog$DataFile)
  #datafields returned as a list of data.frames(DataFields, DataCounts, ClassItems, IRTItems)
  dataFields <- parseNAEP_XML_DataFields(catalog$DataFields)
  #dataformats returned as data.frame
  dataFormats <- parseNAEP_XML_DataFormats(catalog$DataFormats)
  #IRT scaling information returned as data.frame
  IRTScales <- parseNAEP_XML_IRTScales(catalog$IRTScales)
  #Classifications returned as data.frame
  classifications <- parseNAEP_XML_Classifications(catalog$Classifications)
  #Samples
  samples <- parseNAEP_XML_Samples(catalog$Samples)
  #Groups
  groups <- parseNAEP_XML_Groups(catalog$Groups)
  
  fileContents <- list(DataFileDetails = dataFileDetail, 
                       DataFields = dataFields,
                       DataFormats = dataFormats,
                       IRTScales = IRTScales,
                       Classifications = classifications,
                       Samples = samples,
                       Groups = groups)
  
  #fileFormat object will be same format as the 'readMRC' function with the .fr2 file
  #note that the data types are going to be slightly off due to knowing more about the data fields
  fileFormat <- parseNAEP_XML_ToEdSurveyFileFormat(fileContents)
  
  #pvvars are needed ahead of time to properly name the scales to match the pvvars
  fileFormat_pvvars <- trimws(unique(fileFormat$Type)) #get unique pvvars
  fileFormat_pvvars <- fileFormat_pvvars[nchar(fileFormat_pvvars) > 0] #filter out blanks/empty
  fileFormat_pvvars <- fileFormat_pvvars[!grepl("_theta", fileFormat_pvvars, ignore.case = TRUE)] #filter out theta pvvars
  irtParam <- parseNAEP_XML_mmlIRT_Params(fileContents, mmlScoreDict, fileFormat_pvvars)
  
  return(list(Contents = fileContents, FileFormat = fileFormat, IRTParams = irtParam, xml = xml))
}

#converts the processed data.frame XML layout file contents to a fileFormat object for EdSurvey use
parseNAEP_XML_ToEdSurveyFileFormat <- function(fileContents){
  
  nfields <- nrow(fileContents$DataFields$DataFields)
  
  ff <- data.frame(variableName = make.names(tolower(fileContents$DataFields$DataFields$fieldname), unique = TRUE), #make.names here will ensure names match to LaF object and are valid for R
                   Start = fileContents$DataFields$DataFields$start,
                   End = cumsum(fileContents$DataFields$DataFields$width),
                   Width = fileContents$DataFields$DataFields$width,
                   Decimal = fileContents$DataFields$DataFields$decimal,
                   Labels =  trimws(substr(fileContents$DataFields$DataFields$fieldLabel, 1, 50)),
                   labelValues = rep("", times = nfields),
                   pvWt = rep("", times = nfields),
                   Type = rep("", times = nfields),
                   dataType = rep("", times = nfields),
                   weights = rep("", times = nfields))
  
  #yes, these datatypes are strange, but it's to match the existing NAEP fileformat FR2 parser
  #there are strange cases of NAEP files having NULL (ascii code 0) character instead of a blank space and LaF throws error trying to convert
  #getData will convert to numerics where applicable
  ff$dataType <- "character"
  ff$dataType[ff$Width >= 8 & ff$Decimal > 0] <- "numeric"
  ff$dataType[ff$Width < 8 & ff$Decimal > 0] <- "integer"
  
  #value label matching based on the format code
  vlCode <- fileContents$DataFields$DataFields$formatname
  vlIdx <- which(nchar(vlCode)>0, arr.ind = TRUE)
  
  for(i in vlIdx){
    tmpCode <- tolower(vlCode[i])
    ff$labelValues[i] <- fileContents$DataFormats$formatdefinition[tmpCode == tolower(fileContents$DataFormats$formatname)]
  }
  
  #==== THIS BLOCK OF CODE IS UTILIZED FROM readMRC function in the `readNAEP.R` file. Perhaps better methods exist in the XML file, but unable to find documentation, so re-using prior method====
  # keep the original labels
  oLabels <- ff$Labels
  # Finding the plausible weights and jacknife replicates.
  # normally there is just one set of PVs (with multiple subjects or subscales).
  # When there are multiple sets then one starts with an "A" and is the
  # accommodations permitted values
  # main PVs:
  ff$Labels[grepl("plausible", tolower(ff$Labels)) & grepl("value", tolower(ff$Labels)) & "A" != toupper(substring(ff$variableName, 1, 1))] <- "PV"
  # theta PVs, these do not say "value"
  ff$Labels[grepl("plausible", tolower(ff$Labels)) & grepl("theta", tolower(ff$Labels))] <- "PVT"
  # accommodations perimted PVs:
  ff$Labels[grepl("plausible", tolower(ff$Labels)) & grepl("value", tolower(ff$Labels)) & "A" == toupper(substring(ff$variableName, 1, 1))] <- "PV2"
  
  # normally there is just one set of weights. When there are multiple sets
  # then one starts with an "A" and is the accommodations permitted values
  ff$Labels[grepl("weight", tolower(ff$Labels)) & grepl("replicate", tolower(ff$Labels)) & "A" != toupper(substring(ff$variableName, 1, 1))] <- "JK"
  ff$Labels[grepl("weight", tolower(ff$Labels)) & grepl("replicate", tolower(ff$Labels)) & "A" == toupper(substring(ff$variableName, 1, 1))] <- "JK2"
  
  # Check if there is at least one Plausible Value, and then finding their names
  tempValue <- applyPV("PV", ff$Labels, ff$pvWt, oLabels, ff$Type) #from readMRC
  ff$Labels <- tempValue[["Labels"]]
  ff$pvWt <- tempValue[["pvWt"]]
  ff$Type <- tempValue[["Type"]]
  
  # note theta PVs
  tempValue <- applyPV("PVT", ff$Labels, ff$pvWt, oLabels, ff$Type) #from readMRC
  ff$Labels <- tempValue[["Labels"]]
  ff$pvWt <- tempValue[["pvWt"]]
  ff$Type <- tempValue[["Type"]]
  
  # note AP PVs
  tempValue <- applyPV("PV2", ff$Labels, ff$pvWt, oLabels, ff$Type) #from readMRC
  ff$Labels <- tempValue[["Labels"]]
  ff$pvWt <- tempValue[["pvWt"]]
  ff$Type <- tempValue[["Type"]]
  
  # Check if there is at least one JK replicate.
  if(sum(ff$Labels == "JK")>0) {
    # get the number of the JK replicate
    ff$pvWt[ff$Labels == "JK"] <- as.numeric(gsub("[^\\d]+", "", oLabels[ff$Labels=="JK"], perl=TRUE))
    ff$pvWt[ff$Labels == "JK2"] <- as.numeric(gsub("[^\\d]+", "", oLabels[ff$Labels=="JK2"], perl=TRUE))
  }
  
  #identify weights (tries to use new method first, then old method of )
  #the weight variable will be 'preselected' and have a 'designrole' of "W"
  varDF <- fileContents$DataFields$DataFields
  wgtVar <- varDF$fieldname[trimws(tolower(varDF$designrole))=="w"]
  
  if(length(wgtVar)==0){ #if no weight found, use the readMRC method of weight identification using variableName and Label description
    testLbls <- tolower(ff$Labels)
    ff$weights <- ifelse(4*grepl("origwt", ff$variableName, ignore.case=TRUE) + grepl("wgt", ff$variableName) + grepl("student", testLbls) + grepl("weight", testLbls) + grepl("unadjusted", testLbls) + grepl("overall", testLbls) + grepl("unpoststratified", testLbls) - 5 * grepl("replicate", testLbls) >= 4, TRUE, FALSE)
  }else{
    ff$weights <- tolower(ff$variableName) %in% tolower(wgtVar)
  }
  
  return(ff)
}

parseNAEP_XML_DataFields <- function(fieldList){
  
  n <- length(fieldList)

  fieldname <- character(n)
  naepid <- character(n)
  start <- numeric(n)
  width <- numeric(n)
  decimal <- numeric(n)
  format <- character(n)
  type <- character(n)
  block <- character(n)
  item <- character(n)
  accnum <- character(n)
  validn <- numeric(n)
  minvalue <- numeric(n)
  maxvalue <- numeric(n)
  mean <- numeric(n)
  stddev <- numeric(n)
  usage <- character(n)
  formatname <- character(n)
  nummiss <- numeric(n)
  fieldLabel <- character(n)
  preselect <- character(n)
  designrole <- character(n)
  datacounts <- vector("list", n)
  irtitems <- vector("list", n)
  classitems <- vector("list", n)
  
  #build out sequence for loop
  if(n > 0){
    n <- 1:n
  }else{
    n <- NULL
  }
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(n)){
    
    rec <- fieldList[[i]]
    
    fieldname[i] <- parseNAEP_XML_GetAttribute(rec, "fieldname")
    naepid[i] <- parseNAEP_XML_GetAttribute(rec, "naepid")
    start[i] <- parseNAEP_XML_GetAttribute(rec, "start", "numeric")
    width[i] <- parseNAEP_XML_GetAttribute(rec, "width", "numeric")
    decimal[i] <- parseNAEP_XML_GetAttribute(rec, "decimal", "numeric")
    format[i] <- parseNAEP_XML_GetAttribute(rec, "format")
    type[i] <- parseNAEP_XML_GetAttribute(rec, "type")
    block[i] <- parseNAEP_XML_GetAttribute(rec, "block")
    item[i] <- parseNAEP_XML_GetAttribute(rec, "item")
    accnum[i] <- parseNAEP_XML_GetAttribute(rec, "accnum")
    validn[i] <- parseNAEP_XML_GetAttribute(rec, "validn", "numeric")
    minvalue[i] <- parseNAEP_XML_GetAttribute(rec, "minvalue", "numeric")
    maxvalue[i] <- parseNAEP_XML_GetAttribute(rec, "maxvalue", "numeric")
    mean[i] <- parseNAEP_XML_GetAttribute(rec, "mean", "numeric")
    stddev[i] <- parseNAEP_XML_GetAttribute(rec, "stddev", "numeric")
    usage[i] <- parseNAEP_XML_GetAttribute(rec, "usage")
    formatname[i] <- parseNAEP_XML_GetAttribute(rec, "formatname")
    nummiss[i] <- parseNAEP_XML_GetAttribute(rec, "nummiss", "numeric")
    fieldLabel[i] <- parseNAEP_XML_GetAttribute(rec, "fieldlabel")
    preselect[i] <- parseNAEP_XML_GetAttribute(rec, "preselect")
    designrole[i] <- parseNAEP_XML_GetAttribute(rec, "designrole")
    
    if(!is.null(rec$DataCounts)){
      datacounts[[i]] <- rec$DataCounts
    }
    if(!is.null(rec$IRTItems)){
      irtitems[[i]] <- rec$IRTItems
    }
    if(!is.null(rec$ClassItems)){
      classitems[[i]] <- rec$ClassItems
    }
  }#end for(i in seq_along(n))
  
  decimal[is.na(decimal)] <- 0 #to match existing readMRC function output only
  
  #give names to items for better linking later
  names(datacounts) <- fieldname #for our purposes we don't need the datacounts information, can be ignored
  names(irtitems) <- fieldname
  names(classitems) <- fieldname
  
  df <- data.frame(fieldname, naepid, start, width, decimal, format, type, validn, minvalue, maxvalue, 
                   mean, stddev, usage, formatname, nummiss, fieldLabel, preselect, designrole, block, item, stringsAsFactors = FALSE)
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  #==== DATA COUNT ====
  #the data count details the frequency counts for discrete variables.
  datacountDF <- data.frame(fieldname = character(0),
                           datavalue = character(0),
                           datafreq = numeric(0),
                           stringsAsFactors = FALSE)
  
  for(i in seq_along(datacounts)){
    rec <- datacounts[[i]]
    if(is.null(rec)){ #if null no class item definition
      next
    }
    
    tempDF <- datacountDF[FALSE, ] #prep template
    for(ii in seq_along(rec)){
      tempDF[ii, "fieldname"] <- names(datacounts)[i]
      tempDF[ii, "datavalue"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "datavalue")
      tempDF[ii, "datafreq"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "datafreq", "numeric")
    }
    datacountDF <- rbind(datacountDF, tempDF)
  }

  #==== CLASS ITEMS ====
  #Process the classitems lists into a data.frame, filter out NULL entries
  #don't know exactly how many items we will have, so need to rbind here since a data field can have multiple class items associated with it
  classItemDF <- data.frame(fieldname = character(0),
                            classcode = character(0),
                            levelcode = character(0),
                            stringsAsFactors = FALSE)
  
  for(i in seq_along(classitems)){
    rec <- classitems[[i]]
    if(is.null(rec)){ #if null no class item definition
      next
    }
    
    tempDF <- classItemDF[FALSE, ] #prep template
    for(ii in seq_along(rec)){
      tempDF[ii, "fieldname"] <- names(classitems)[i]
      tempDF[ii, "classcode"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "classcode")
      tempDF[ii, "levelcode"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "levelcode")
    }
    classItemDF <- rbind(classItemDF, tempDF)
  }
  
  #==== IRT Item Parameters ====
  #grabs the IRT Item details as well as the parameter details and generates a data.frame of the information
  irtItemDF <- data.frame(fieldname = character(0),
                          scale = character(0),
                          sample = character(0),
                          numalt = character(0),
                          numcat = character(0),
                          model = character(0),
                          score = character(0),
                          numparms = character(0),
                          parametercode = character(0),
                          IRTValue = numeric(0),
                          stderror = numeric(0),
                          stringsAsFactors = FALSE)
  
  for(i in seq_along(irtitems)){
    for(j in seq_along(irtitems[[i]])){ #three levels of nesting here
      rec <- irtitems[[i]][[j]]
      
      if(is.null(rec)){ #if null no IRT Items definition
        next
      }
      
      fldName <- names(irtitems)[i]
      scale <- parseNAEP_XML_GetAttribute(rec, "scale")
      sample <- parseNAEP_XML_GetAttribute(rec, "sample")
      numalt <- parseNAEP_XML_GetAttribute(rec, "numalt")
      numcat <- parseNAEP_XML_GetAttribute(rec, "numcat")
      model <- parseNAEP_XML_GetAttribute(rec, "model")
      score <- parseNAEP_XML_GetAttribute(rec, "score")
      numparms <- parseNAEP_XML_GetAttribute(rec, "numparms")
      
      tempDF <- irtItemDF[FALSE, ] #prep template
      for(ii in seq_along(rec)){
        tempDF[ii, "fieldname"] <- fldName
        tempDF[ii, "scale"] <- scale
        tempDF[ii, "sample"] <- sample
        tempDF[ii, "numalt"] <- numalt
        tempDF[ii, "numcat"] <- numcat
        tempDF[ii, "model"] <- model
        tempDF[ii, "score"] <- score
        tempDF[ii, "numparms"] <- numparms
        tempDF[ii, "parametercode"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "parametercode")
        tempDF[ii, "IRTValue"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "IRTvalue", "numeric")
        tempDF[ii, "stderror"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "stderror", "numeric")
      }
      irtItemDF <- rbind(irtItemDF, tempDF)
    }#end for(j in seq_along(irtitems[i]))
  }#end for(i in seq_along(irtitems))
  
  retList <- list(DataFields = df, DataCounts = datacountDF, ClassItems = classItemDF, IRTItems = irtItemDF)
  return(retList)
}

#helper function to get the attribute of a list item
#if attribute does not exist in the item an 'NA' value is returned
#if dataType is NULL it will be ignored and a character will be returned (default for attributes), otherwise it will convert by setting class type
parseNAEP_XML_GetAttribute <- function(item, attributeName, dataType = NULL){
  x <- attr(item, attributeName)
  #NULL if attribute does not exist
  if(is.null(x) || is.na(x)){
    return(NA)
  }
  if(!is.null(dataType)){
    class(x) <- dataType
  }
  return(x)
}

#parse the dataformats section and stack into a formatted data.frame
parseNAEP_XML_DataFormats <- function(formatList){
  n <- length(formatList)
  
  formatname <- character(n)
  numcodes <- numeric(n)
  formatdefinition <- character(n)

  #build out sequence for loop
  if(n > 0){
    n <- 1:n
  }else{
    n <- NULL
  }
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(n)){
    rec <- formatList[[i]]
    
    formatname[i] <- parseNAEP_XML_GetAttribute(rec, "formatname")
    numcodes[i] <- parseNAEP_XML_GetAttribute(rec, "numcodes", "numeric")
    nCodes <- length(rec)
    val <- character(nCodes)
    lbl <- character(nCodes)
    nCodes <- 1:length(rec)
    
    for(ii in nCodes){
      fmt <- rec[[ii]]
      val[ii] <- parseNAEP_XML_GetAttribute(fmt, "formatvalue")
      lbl[ii] <- parseNAEP_XML_GetAttribute(fmt, "formatlabel")
      lbl[ii] <- gsub("^", "", lbl[ii], fixed = TRUE) #need to ensure carrot is not in labels for collapse scheme!!
    }
    
    #special case for the label 'multiple' to be moved to the end of the item string to retain consistency with .FR2 format 
    if(any(grepl("^multiple$", lbl, ignore.case = TRUE))){
      idx <- which(grepl("^multiple$", lbl, ignore.case = TRUE))
      #remove the item, then order it at the tail of the vector
      tmpVal <- val[-idx]
      tmpLbl <- lbl[-idx]
      val <- c(tmpVal, val[idx])
      lbl <- c(tmpLbl, lbl[idx])
    }
    
    formatdefinition[i] <- paste(val, lbl, sep = "=", collapse = "^")
  }
  
  df <- data.frame(formatname, numcodes, formatdefinition, stringsAsFactors = FALSE)
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  return(df)
}

#parse the IRT Scales section and stack into a formatted data.frame
parseNAEP_XML_IRTScales <- function(scales){
  n <- length(scales)
  
  subject <- character(n)
  scalecode <- character(n)
  scalelabel <- character(n)
  scalesample <- character(n)
  transform_a <- numeric(n)
  transform_b <- numeric(n)
  compositeweight <- numeric(n)
  
  #build out sequence for loop
  if(n > 0){
    n <- 1:n
  }else{
    n <- NULL
  }
  
  subj <- parseNAEP_XML_GetAttribute(scales, "subject")
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(n)){
    rec <- scales[[i]]
    
    subject[i] <- subj
    scalecode[i] <- parseNAEP_XML_GetAttribute(rec, "scalecode")
    scalelabel[i] <- parseNAEP_XML_GetAttribute(rec, "scalelabel")
    scalesample[i] <- parseNAEP_XML_GetAttribute(rec, "scalesample")
    transform_a[i] <- parseNAEP_XML_GetAttribute(rec, "transform-a", "numeric")
    transform_b[i] <- parseNAEP_XML_GetAttribute(rec, "transform-b", "numeric")
    compositeweight[i] <- parseNAEP_XML_GetAttribute(rec, "compositeweight", "numeric")
  }
  
  df <- data.frame(subject, scalecode, scalelabel, scalesample, transform_a, transform_b, compositeweight, stringsAsFactors = FALSE)
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  return(df)
}

#parse the IRT Scales section and stack into a formatted data.frame
parseNAEP_XML_Classifications <- function(classifications){
  
  df <- data.frame(classcode = character(0),
                   classlabel = character(0),
                   numlevels = numeric(0),
                   levelcode = character(0),
                   levellabel = character(0))
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(classifications)){
    rec <- classifications[[i]]
    tempDF <- df[FALSE, ]
    
    classcode <- parseNAEP_XML_GetAttribute(rec, "classcode")
    classlabel <- parseNAEP_XML_GetAttribute(rec, "classlabel")
    numlevels <- parseNAEP_XML_GetAttribute(rec, "numlevels", "numeric")
    
    for(ii in seq_along(rec)){
      tempDF[ii, "classcode"] <- classcode
      tempDF[ii, "classlabel"] <- classlabel
      tempDF[ii, "numlevels"] <- numlevels
      tempDF[ii, "levelcode"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "levelcode")
      tempDF[ii, "levellabel"] <- parseNAEP_XML_GetAttribute(rec[[ii]], "levellabel")
    }
    
    df <- rbind(df, tempDF)
  }
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  return(df)
}

#parse the Samples section and stack into a formatted data.frame
parseNAEP_XML_Samples <- function(samples){
  
  n <- length(samples)
  df <- data.frame(samplecode = character(n),
                   samplevar = character(n),
                   samplevalue = character(n),
                   samplelabel = character(n))
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(samples)){
    rec <- samples[[i]]
    
    df[i, "samplecode"] <- parseNAEP_XML_GetAttribute(rec, "samplecode")
    df[i, "samplevar"] <- parseNAEP_XML_GetAttribute(rec, "samplevar")
    df[i, "samplevalue"] <- parseNAEP_XML_GetAttribute(rec, "samplevalue")
    df[i, "samplelabel"] <- parseNAEP_XML_GetAttribute(rec, "samplelabel")
  }
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  return(df)
}

#parse the Samples section and stack into a formatted data.frame
parseNAEP_XML_Groups <- function(groups){
  
  n <- length(groups)
  df <- data.frame(usagecode = character(n),
                   grouplabel = character(n))
  
  #need to parse record by record since not all entries will have all attributes
  for(i in seq_along(groups)){
    rec <- groups[[i]]
    
    df[i, "usagecode"] <- parseNAEP_XML_GetAttribute(rec, "usagecode")
    df[i, "grouplabel"] <- parseNAEP_XML_GetAttribute(rec, "grouplabel")
  }
  
  charCols <- sapply(df, is.character)
  df[charCols][is.na(df[charCols])] <- "" #for character columns convert NA to blanks
  
  return(df)
}

#reformats the xml layout IRT parameter information into the format needed for mml functions
#returns a list object consisting of these elements:
#1: TestData
#2: DichotParamTab
#3: PolyParamTab
#4: AdjsutedData
#5: ScoreDict
#6: ScoreCard
parseNAEP_XML_mmlIRT_Params <- function(fileContents, scoreDict, fileFormat_pvvars){
  
  compositeRegex <- "composite"
  
  testData <- parseNAEP_XML_IRT_TestData(fileContents, fileFormat_pvvars)
  
  pvvarsNotInScales <- fileFormat_pvvars[!fileFormat_pvvars %in% testData$subtest]
  hasComposite <- grepl(compositeRegex, pvvarsNotInScales, ignore.case = TRUE)

  
  dichotParamTab <- parseNAEP_XML_IRT_DichotParam(fileContents, fileFormat_pvvars)
  polyParamTab <- parseNAEP_XML_IRT_PolyParam(fileContents, fileFormat_pvvars)
  adjustedData <- parseNAEP_XML_IRT_AdjustedData(fileContents) #returns formatted empty data.frame with matching columns
  
  #score dict required for processing score card
  scoreCard <- parseNAEP_XML_IRT_ScoreCard(fileContents, scoreDict)
  
  res <- list(TestData = testData,
              DichotParamTab = dichotParamTab,
              PolyParamTab = polyParamTab,
              AdjsutedData = adjustedData,
              ScoreDict = scoreDict,
              ScoreCard = scoreCard)
  return(res)
}

#prepares the dichotomous item data.frame for mml
parseNAEP_XML_IRT_DichotParam <- function(fileContents, fileFormat_pvvars){
  
  irtItems <- fileContents$DataFields$IRTItems
  irtItems <- irtItems[irtItems$numcat==1, ] #filter here for 3PL and 2PL items
  
  irtScales <- fileContents$IRTScales
  scaleInfo <- irtScales[ , c("subject", "scalecode", "scalelabel")]
  scaleInfo$convertedCode <- parseNAEP_XML_CodeConvert(irtScales, fileFormat_pvvars)
  
  #get our data subset
  subIRT <- unique(irtItems[ , c("fieldname", "scale", "sample", "numalt", "numcat", "model", "score")]) #numcat==1 gives us 2PL and 3PL items
  
  #split these out into separate data frames for easier merging
  matchFields <- c("fieldname", "scale", "sample")
  dataFields <- c("IRTValue", "stderror")
  aparams <- irtItems[irtItems$parametercode=="A" , c(matchFields, dataFields)]
  bparams <- irtItems[irtItems$parametercode=="B" , c(matchFields, dataFields)]
  cparams <- irtItems[irtItems$parametercode=="C" , c(matchFields, dataFields)]
  abcParams <- unique(rbind(aparams[, matchFields],
                       bparams[, matchFields],
                       cparams[, matchFields]))
  
  #slope
  abcParams <- merge(abcParams, aparams, by=matchFields, all.x = TRUE)
  abcParams$slope <- abcParams$IRTValue
  abcParams[ , dataFields] <- NULL
  #difficulty
  abcParams <- merge(abcParams, bparams, by=matchFields, all.x = TRUE)
  abcParams$difficulty <- abcParams$IRTValue
  abcParams[ , dataFields] <- NULL
  #guessing
  abcParams <- merge(abcParams, cparams, by=matchFields, all.x = TRUE)
  abcParams$guessing <- abcParams$IRTValue
  abcParams[ , dataFields] <- NULL
  abcParams$guessing[is.na(abcParams$guessing)] <- 0 #set any 2PL items to 0 here
  
  #these fields are to match existing IRT 'dichotParamTab' value and will be what gets returned
  subIRT$ItemID <- tolower(subIRT$fieldname)
  subIRT$subtest <- scaleInfo$convertedCode[match(tolower(subIRT$scale), tolower(scaleInfo$scalecode))]
  if(nrow(subIRT) > 0){ #might not have data
    subIRT$D <- 1.7
    subIRT$missingValue <- 1 / nchar(subIRT$score)
    subIRT$missingCode <- 8
  } else {
    subIRT$D <- numeric(0)
    subIRT$missingValue <- character(0)
    subIRT$missingCode <- numeric(0)
  }
  
  subIRT <- merge(subIRT, abcParams, by = matchFields, all.x = TRUE)
  
  #to match existing IRT specs
  res <- subIRT[ , c("ItemID", "subtest", "slope", "difficulty", "guessing", "D", "missingValue", "missingCode")]
  return(res)
}

parseNAEP_XML_IRT_PolyParam <- function(fileContents, fileFormat_pvvars){
  
  irtItems <- fileContents$DataFields$IRTItems
  irtItems <- irtItems[irtItems$numcat > 1, ] #filter here for poly items (having numcat > 1) 
  
  irtScales <- fileContents$IRTScales
  scaleInfo <- irtScales[ , c("subject", "scalecode", "scalelabel")]
  scaleInfo$convertedCode <- parseNAEP_XML_CodeConvert(irtScales, fileFormat_pvvars)
  
  #get our data subset
  subIRT <- unique(irtItems[ , c("fieldname", "scale", "sample", "numalt", "numcat", "model", "score")])
  
  matchFields <- c("fieldname", "scale", "sample")
  dataFields <- c("IRTValue", "stderror")
  
  aparam <- irtItems[irtItems$parametercode=="A" , c(matchFields, dataFields)]
  bparam <- irtItems[irtItems$parametercode=="B" , c(matchFields, dataFields)]
  d1 <- irtItems[irtItems$parametercode=="D01" , c(matchFields, dataFields)]
  d2 <- irtItems[irtItems$parametercode=="D02" , c(matchFields, dataFields)]
  d3 <- irtItems[irtItems$parametercode=="D03" , c(matchFields, dataFields)]
  d4 <- irtItems[irtItems$parametercode=="D04" , c(matchFields, dataFields)]
  d5 <- irtItems[irtItems$parametercode=="D05" , c(matchFields, dataFields)]
  
  allParams <- unique(rbind(aparam[, matchFields],
                            bparam[, matchFields],
                            d1[, matchFields],
                            d2[, matchFields],
                            d3[, matchFields],
                            d4[, matchFields],
                            d5[, matchFields]))
  
  #slope
  allParams <- merge(allParams, aparam, by=matchFields, all.x = TRUE)
  allParams$slope <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #item location
  allParams <- merge(allParams, bparam, by=matchFields, all.x = TRUE)
  allParams$itemLocation <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #d1
  allParams <- merge(allParams, d1, by=matchFields, all.x = TRUE)
  allParams$d1 <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #d2
  allParams <- merge(allParams, d2, by=matchFields, all.x = TRUE)
  allParams$d2 <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #d3
  allParams <- merge(allParams, d3, by=matchFields, all.x = TRUE)
  allParams$d3 <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #d4
  allParams <- merge(allParams, d4, by=matchFields, all.x = TRUE)
  allParams$d4 <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  #d5
  allParams <- merge(allParams, d5, by=matchFields, all.x = TRUE)
  allParams$d5 <- allParams$IRTValue
  allParams[ , dataFields] <- NULL
  
  #these fields are to match existing IRT 'dichotParamTab' value and will be what gets returned
  subIRT$ItemID <- tolower(subIRT$fieldname)
  subIRT$subtest <- scaleInfo$convertedCode[match(tolower(subIRT$scale), tolower(scaleInfo$scalecode))]
  if(nrow(subIRT)>0){
    subIRT$D <- 1.7
    subIRT$missingValue <- "c"
    subIRT$missingCode <- 8
  } else{
    subIRT$D <- numeric(0)
    subIRT$missingValue <- character(0)
    subIRT$missingCode <- numeric(0)
  }
  
  subIRT <- merge(subIRT, allParams, by = matchFields, all.x = TRUE)
  
  #to match existing IRT specs
  res <- subIRT[ , c("ItemID", "subtest", "slope", "itemLocation", "d1", "d2", "d3", "d4", "d5", "D", "missingValue", "missingCode")]
  return(res)
}

#retrieve the subscale test level information
parseNAEP_XML_IRT_TestData <- function(fileContents, fileFormat_pvvars){
  
  irt <- fileContents$IRTScales #this includes the 'subtest' information needed
  res <- data.frame(subtest = parseNAEP_XML_CodeConvert(irt, fileFormat_pvvars), #convert codes to what IRTparams has
                    location = irt$transform_b, #convert codes to what IRTparams has
                    scale = irt$transform_a,
                    subtestWeight = irt$compositeweight,
                    scalecode = irt$scalecode, #these are on the XML irt scales and might be useful to include here, but not needed for mml.sdf
                    scalelabel = irt$scalelabel,
                    scalesample = irt$scalesample)
  #do we need to check if composite weights equal to 1 (100%) if they are present?
  #a file can have multiple samples (e.g. digital/paper)
  return(res)
}

parseNAEP_XML_IRT_AdjustedData <- function(fileContents){
  
  df <- data.frame(NAEPid = character(0),
                   from = character(0),
                   to = character(0),
                   stringsAsFactors = FALSE)
  
  #RETURN EMTPY DATA.FRAME. WE HAVE THE SCORING KEY MASK (e.g., 0010, 0123, 0100)
  return(df)
}

parseNAEP_XML_IRT_ScoreCard <- function(fileContents, scoreDict){
  
  irtItems <- unique(fileContents$DataFields$IRTItems[, c("fieldname", "score", "numcat", "model")])
  
  dataFields <- fileContents$DataFields$DataFields
  dataFields <- subset(dataFields, (tolower(dataFields$fieldname) %in% tolower(irtItems$fieldname)))
  dataFields <- merge(dataFields, irtItems, by="fieldname", all.x = TRUE)
  
  formats <- fileContents$DataFormats
  dataFields <- merge(dataFields, formats, by="formatname", all.x = TRUE) #datafields DF will now have all needed data (labels and scoring key)
  irtItems <- NULL
  formats <- NULL
  
  dfTemplate <- data.frame(key = character(0), #the variable name
                           answer = character(0), #the value label defined
                           score = numeric(0),
                           stringsAsFactors = FALSE) #the number of points its assigned for the 'scoringKey' index
  dfResult <- dfTemplate
  
  for(i in seq_len(nrow(dataFields))){ #loop for each row in dataFields data.frame
    
    var <- tolower(dataFields$fieldname[i])
    scr <- dataFields$score[i]
    scrArr1 <- unlist(strsplit(scr, "")) #split into it a character vector
    valLbls <- dataFields$formatdefinition[i]
    isDichot <- dataFields$numcat[i]==1 #numcat==1 signifies it's a dichotomous item
    
    valTokens <- unlist(strsplit(valLbls, "^", fixed = TRUE))
    valTokens <- strsplit(valTokens, "=", fixed = TRUE)
    
    vals <- character(length(valTokens))
    lbls <- character(length(valTokens))
    
    for(ii in seq_along(valTokens)){
      vals[ii] <- valTokens[[ii]][1]
      lbls[ii] <- paste(valTokens[[ii]][-1], sep = "", collapse = "")
    }
    
    vals1 <- vals[1:nchar(scr)] #only need the items in the scoring key, the rest can be discarded
    lbls1 <- lbls[1:nchar(scr)] #only need the items in the scoring key, the rest can be discarded
    vals2 <- vals[!vals %in% vals1] #these will be omitted, missing, etc.
    lbls2 <- lbls[!lbls %in% lbls1] #these will be omitted, missing, etc.
    
    tempDF <- data.frame(key = rep(var, times = length(lbls1)), answer = lbls1, score = as.numeric(scrArr1), stringsAsFactors = FALSE)
    dfResult <- rbind(dfResult, tempDF)
    tempDF <- NULL
    
    #scored differently depending on it's question type
    if(isDichot){
      scrArr2 <- scoreDict$pointMult[match(tolower(lbls2), tolower(scoreDict$resCat))]
    } else{ #poly item
      scrArr2 <- scoreDict$pointConst[match(tolower(lbls2), tolower(scoreDict$resCat))]
    }
    
    tempDF <- data.frame(key = rep(var, times = length(lbls2)), answer = lbls2, score = as.numeric(scrArr2), stringsAsFactors = FALSE)
    tempDF <- subset(tempDF, !is.na(tempDF$score)) #only add items with a scoring value
    dfResult <- rbind(dfResult, tempDF)
    tempDF <- NULL
  }
  
  return(dfResult)
}

#converts a NAEP subject or test code into the value used with NAEPirtparams to have them be consistent and match
parseNAEP_XML_CodeConvert <- function(irtScaleDF, fileFormat_pvvars) {
  
  compositePVRegex <- "(composite|univariate)" #regex for determining composite pvvars, they will have 'composite' or 'univariate' in the title
  
  samples <- unique(irtScaleDF$scalesample) #files can have multiple samples, generally for paper/digital variations
  resDF <- irtScaleDF[FALSE , ] #empty result data.frame
  resDF$pvvar <- character(0) #add blank column for our matching pvvar
  
  if(nrow(irtScaleDF)==1 && length(fileFormat_pvvars)==1){
    return(fileFormat_pvvars)
  }

  #process each sample separately
  for (samp in samples) {
    irtSubDF <- irtScaleDF[irtScaleDF$scalesample == samp , ] #subset for specific sample
    irtSubDF$pvvar <- NA #add pvvar to match our resDF
    
    #is digital or paper based assessment sample
    isPBA <- all(grepl("paper", irtSubDF$scalelabel, ignore.case = TRUE))
    isDBA <- all(grepl("digital", irtSubDF$scalelabel, ignore.case = TRUE))
    
    #sometimes there is a separate IRT scale code for the composite/overall, other times there is not, and the entire sample is the composite
    hasComposite <- any(grepl(compositePVRegex, fileFormat_pvvars, ignore.case = TRUE))

    for(i in seq_len(nrow(irtSubDF))) {
      
      #Test 1 || check if this is a composite scale item or 'overall' item
      if (hasComposite && grepl("overall", irtSubDF$scalelabel[i], ignore.case = TRUE)) {
        irtSubDF$pvvar[i] <- fileFormat_pvvars[grepl(compositePVRegex, fileFormat_pvvars, ignore.case = TRUE)]
        next #skip to next
      }
      
      #get the first word in the scale label
      firstWrd <- tolower(unlist(regmatches(irtSubDF$scalelabel[i], regexpr("\\w+", irtSubDF$scalelabel[i])))) #regexpr for first match only
      
      #Test 2 - test first word matching sting entirely for pvvar name (e.g., physical science == physical)
      if (any(tolower(fileFormat_pvvars) == firstWrd)) {
        irtSubDF$pvvar[i] <- fileFormat_pvvars[tolower(fileFormat_pvvars) == firstWrd][1] #first TRUE match (should only be one!)
        next
      }
      
      #Test 3 - test if the first word is in the pvvar name at any point
      if (any(grepl(paste0("(", firstWrd, ")"), fileFormat_pvvars, ignore.case = TRUE))) {
        irtSubDF$pvvar[i] <- fileFormat_pvvars[grepl(paste0("(", firstWrd, ")"), fileFormat_pvvars, ignore.case = TRUE)][1] #first TRUE match (should only be one!)
        next
      }
      
      #Test 4 - grab tokens for each word, remove any 'ignore' tokens (e.g., 'and', 'for', 'from'), then test if any tokens match pvvar name
      tokens <- tolower(unlist(regmatches(irtSubDF$scalelabel[i], gregexpr("\\w+", irtSubDF$scalelabel[i])))) #gregexpr for all words
      ignoreTokens <- "^(and|&|to|from|for|long-term |long|term|trend|digital|paper)$"
      tokens <- tokens[!grepl(ignoreTokens, tokens, ignore.case = TRUE)]
      
      testpvvars <- fileFormat_pvvars[!grepl(compositePVRegex, fileFormat_pvvars, ignore.case = TRUE)] #start with all pvvars, filter out the composites
      testRes <- numeric(length = length(tokens))
      for(ii in seq_along(tokens)){
        xRes <- grepl(paste0("(", tokens[ii], ")"), testpvvars, ignore.case = TRUE)
        
        if (any(xRes)){
          testRes[ii] <- which(xRes)
        }
        #testRes[ii] <- which(grepl(paste0("(", tokens[ii], ")"), testpvvars, ignore.case = TRUE))
      }
      
      if (any(testRes > 0)) { #a match was found!
        testRes <- testRes[testRes > 0]
        irtSubDF$pvvar[i] <- testpvvars[testRes[1]]
        next
      }
      
      #Test 5 - grab first three characters of the tokens, check to see if the 3-character tokens are present in the pvvar name
      tokens3Char <- substr(tokens, 1, 3)
      
      testpvvars <- fileFormat_pvvars[!grepl(compositePVRegex, fileFormat_pvvars, ignore.case = TRUE)] #start with all pvvars, filter out the composites
      testRes <- numeric(length = length(tokens3Char))
      for(ii in seq_along(tokens3Char)){
        xRes <- grepl(paste0("(", tokens3Char[ii], ")"), testpvvars, ignore.case = TRUE)
        
        if (any(xRes)){
          testRes[ii] <- which(xRes)
        }
        #testRes[ii] <- which(grepl(paste0("(", tokens[ii], ")"), testpvvars, ignore.case = TRUE))
      }
      
      frq <- table(testRes)
      frq <- frq[names(frq)!="0"] #filter out no matches
      
      if (length(frq) > 0) { #match was found!
        testRes <- as.numeric(names(frq)[which.max(frq)]) #get the most common match
        
        irtSubDF$pvvar[i] <- testpvvars[testRes[1]]
        next
      }
      
      #could not be located, or the pvvars were not accurately located in the file throw warning for the user
      warning(paste0("Unable to locate matching pvvar variable for scale: ", paste0(irtSubDF$scalecode[i], " - ", irtSubDF$scalelabel[i])))
      irtSubDF$pvvar[i] <- ""  
    } #end for(i in seq_len(nrow(irtSubDF)))
    
    if (isPBA) {
      irtSubDF$pvvar <- paste0(irtSubDF$pvvar, "_paper")
    }
    if (isDBA) {
      irtSubDF$pvvar <- paste0(irtSubDF$pvvar, "_digital")
    }
    
    resDF <- rbind(resDF, irtSubDF)
  } #end for (samp in samples)

  #reorder to match original order, in case it didn't stack as it was originally
  origOrder <- which(resDF$scalecode==irtScaleDF$scalecode)
  res <- resDF$pvvar[origOrder]
  
  return(res)
}
