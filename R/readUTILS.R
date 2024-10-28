# this file is used for common utility function that can be re-used across different functions

# takes a tibble object (spssDF) after it has been read in from a haven 'read_sav' call and returns a fileFormat object of the tibble describing it's properties
#' @keywords internal
getSPSSFileFormat <- function(spssDF) {
  if (!inherits(spssDF, "data.frame")) stop("spssDF must be a data.frame")

  colInfo <- data.frame(names = colnames(spssDF), stringsAsFactors = FALSE)
  colInfo$format <- sapply(colInfo$names, function(z) {
    attributes(spssDF[[z]])$format.spss
  })
  colInfo$class <- sapply(colInfo$names, function(zz) { # class seems to only be populated if it's a 'labelled' field or 'date' field, we want to treat dates as characters
    attributes(spssDF[[zz]])$class
  })

  colInfo$decimal <- as.numeric(ifelse(substr(colInfo$format, 1, 1) == "F", sapply(strsplit(colInfo$format, "\\."), function(x) {
    tail(x, 1)
  }), rep(NA, nrow(colInfo))))
  colInfo$multiplier <- as.numeric(ifelse(is.na(colInfo$decimal), 1, 10^colInfo$decimal))
  colInfo$size <- gsub("[a-zA-Z]", "", sapply(strsplit(colInfo$format, "\\."), function(x) {
    head(x, 1)
  }))
  colInfo$size <- as.numeric(colInfo$size)

  colInfo$type <- substr(colInfo$format, 1, 1)
  colInfo$dataType <- "character" # set 'character' type to all fields first
  colInfo$dataType[colInfo$type == "F" & colInfo$size < 9 & colInfo$decimal == 0] <- "integer"
  colInfo$dataType[colInfo$type == "F" & colInfo$size >= 9 & colInfo$decimal == 0] <- "numeric"
  colInfo$dataType[colInfo$type == "F" & colInfo$decimal > 0] <- "numeric"

  ff <- data.frame(variableName = colInfo$names, stringsAsFactors = FALSE)

  ff$variableName <- tolower(ff$variableName)
  ff$Start <- c(1, 1 + cumsum(colInfo$size))[1:nrow(colInfo)]
  ff$End <- cumsum(colInfo$size)
  ff$Width <- colInfo$size
  ff$Decimal <- colInfo$decimal

  # get labels
  lbls <- sapply(colnames(spssDF), function(z) {
    attributes(spssDF[[z]])$label
  })
  ff$Labels <- lbls

  # get level labels
  lblv <- sapply(colnames(spssDF), function(z) {
    attr <- attributes(spssDF[[z]])$labels
    paste(attr, names(attr), sep = "=", collapse = "^")
  })

  ff$labelValues <- toupper(lblv)
  # test for any specialty *dash* (em-dash, en-dash) characters in the labels that may be confusing for users and replace them with a regular dash character
  ff$labelValues <- gsub("\u2013", "-", ff$labelValues) # replace en-dash (\u2013) with regular dash (\u002d)
  ff$labelValues <- gsub("\u2014", "-", ff$labelValues) # replace em-dash (\u2014) with regular dash (\u002d)

  ff$dataType <- colInfo$dataType

  ff$weights <- FALSE # set all weights to false initially

  # fix any column names that are named 'repeat', or add other reserved names if those are present too
  # do this after it's gathered all the required info from the SPSS metadata
  if (any(grepl("^repeat$", ff$variableName, ignore.case = TRUE))) {
    idx <- grepl("^repeat$", ff$variableName, ignore.case = TRUE)
    prefix <- ff$variableName[idx]
    suffix <- seq_along(prefix)

    ff$variableName[idx] <- paste(prefix, suffix, sep = ".")
  }
  return(ff)
}

# reads an SPSS (.sps) snytax file and prepares the fileformat from the SPSS syntax file
# expects a very specific format of SPSS file and will only be applicable for the format found with ECLS_K, BTLS, and HSTS data sets
# going foward looking into possible packages to use for parsing SPSS/SAS scripts that would be more reliable/handle other formats
#' @keywords internal
parseSPSSFileFormat <- function(inSPSSyntax) {
  dict <- list(
    "variableName" = character(0),
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
    "RecordIndex" = character(0)
  )

  # Read in spss control files
  con <- file(inSPSSyntax, open = "r")
  controlFile <- readLines(con)
  close.connection(con)

  # prep for processing
  controlFile <- gsub("[^[:print:]]", "", controlFile) # remove unprintable characters
  controlFile <- trimws(controlFile, which = "both") # remove leading or ending whitespace
  controlFile <- controlFile[controlFile != ""] # remove blank rows


  # Note: the following lines need to be in order
  # Some syntax files uses single quote instead of double quote
  # Because later code use quote as a pattern to get labelValues, it's necessary
  # to replace relevant single quotes with double quotes
  controlFile <- gsub(" \'", " \"", controlFile) # replace single quote with double quote for later use
  controlFile <- gsub("\' ", "\" ", controlFile)
  controlFile <- gsub("^\'|\'$", "\"", controlFile)
  controlFile <- gsub("\'/", "\"/", controlFile)
  controlFile <- gsub("\'\\.", "\"\\.", controlFile)

  # GET VARIABLE LIST, FWF POSITIONS, and DATA TYPE
  i <- 1
  while (!grepl("file handle (fhand){0,1}.*name=.*", controlFile[i], ignore.case = TRUE)) { # find file handle function in SPSS file
    i <- i + 1

    if (i > length(controlFile)) {
      stop(paste0("Error parsing SPSS syntax file: Unable to locate file handle location."))
    }
  }
  while (substr(controlFile[i], 1, 1) != "/") { # find start line of variables by '1-# (1 is first column in .dat file)
    i <- i + 1
  }


  # find where we want to end the variable list chunk
  j <- i + 1
  while (controlFile[j] != ".") { # find start line of variables by '1-# (1 is first column in .dat file)
    j <- j + 1
  }

  lineChunk <- controlFile[i:j]
  lineChunkRecIndex <- which(lineChunk %in% lineChunk[substr(lineChunk, 1, 1) == "/"]) # record index are defined such as '/2', '/3', etc.  we want to keep these positions as the .dat has multiple rows for one piece of data
  names(lineChunkRecIndex) <- seq_along(lineChunkRecIndex)

  lineChunk <- lineChunk[trimws(lineChunk, which = "both") != ""]
  lineChunk <- lineChunk[substr(lineChunk, 1, 1) != "."] # remove unneeded rows

  recIndex <- c()
  ri <- 0 # will change to one when first line is noted
  for (lc in lineChunk) {
    if (substr(lc, 1, 1) == "/") {
      ri <- ri + 1
    } else {
      recIndex <- c(recIndex, ri)
    }
  }
  lineChunk <- lineChunk[substr(lineChunk, 1, 1) != "/"] # remove the table indexes now that we don't need them

  xMatch <- regexpr("^\\w+", lineChunk) # get first word
  varName <- tolower(trimws(regmatches(lineChunk, xMatch), which = "both")) # use lower case variable names
  names(lineChunk) <- varName # store the variable name here for later use

  xMatch <- regexpr(" \\d+[ ]*-\\d+", lineChunk) # get digits of fwf ###-###
  pos <- trimws(regmatches(lineChunk, xMatch), which = "both")

  xMatch <- regexpr("\\d+[ ]*-", pos) # get digits before '-' char
  posStart <- trimws(gsub("-", "", regmatches(pos, xMatch)), which = "both")
  posStart <- as.integer(posStart)

  xMatch <- regexpr("-\\d+", pos) # get digits after '-' char
  posEnd <- trimws(gsub("-", "", regmatches(pos, xMatch)), which = "both")
  posEnd <- as.integer(posEnd)


  # grab extra info contained in parens()
  xMatch <- regexpr("[(].*[)]", lineChunk)
  extraInfo <- toupper(trimws(regmatches(lineChunk, xMatch), which = "both"))

  # setup default data type and decimal
  xType <- rep("numeric", times = length(varName))
  names(xType) <- varName
  xDec <- rep(0, times = length(varName))
  names(xDec) <- varName

  # change character type
  xType[names(xType) %in% names(extraInfo[extraInfo == "(A)"])] <- "character"
  xDec[names(xDec) %in% names(extraInfo[extraInfo == "(A)"])] <- NA

  # change the numeric type
  xType[names(xType) %in% names(extraInfo[grepl("\\d+", extraInfo, ignore.case = TRUE)])] <- "numeric"
  xMatch <- regexpr("\\d+", extraInfo)
  xDec[names(xDec) %in% names(extraInfo[grepl("\\d+", extraInfo, ignore.case = TRUE)])] <- as.integer(regmatches(extraInfo, xMatch))

  # update the dictionary
  dict$variableName <- varName
  dict$Start <- posStart
  dict$End <- posEnd
  dict$Width <- (dict$End - dict$Start) + 1
  dict$Decimal <- xDec
  dict$dataType <- xType
  dict$RecordIndex <- recIndex
  #########################

  # Get the Variable Label section here in chunk
  i <- j # move one row down past the 'VARIABLE LABEL' line
  while (!grepl("variable label", controlFile[i], ignore.case = TRUE)) { # get out next chunk of data (VARIABLE DATA)
    i <- i + 1
  }

  j <- i + 1
  while (!grepl("value labels", controlFile[j], ignore.case = TRUE)) { # get out next chunk of data (VARIABLE DATA)
    j <- j + 1
  }

  lineChunk <- controlFile[(i + 1):(j - 1)] # get all the variable labels as on chunk
  lineChunk <- lineChunk[length(trimws(lineChunk, which = "both")) > 0 & lineChunk != "."] # remove any junk lines

  # get variable name
  xMatch <- regexpr("^\\w+", lineChunk) # get first word
  varName <- trimws(regmatches(lineChunk, xMatch), which = "both")

  xMatch <- regexpr("\\s[\"].*[\"]", lineChunk) # find the text between the double quotes in the string
  varLabel <- trimws(regmatches(lineChunk, xMatch), which = "both")
  varLabel <- substr(varLabel, 2, nchar(varLabel) - 1) # remove the beginning and end double quotes

  # setup the labels
  dict$Labels <- rep("", length(dict$variableName))
  dict$Labels[tolower(dict$variableName) == tolower(varName)] <- varLabel

  # prep for gathering the value labels::should look into doing this in one chunk as well, but difficult with the varnames being used as the markers
  dict$labelValues <- rep("", times = length(dict$variableName)) # so we don't have NA values in the labels and it's of proper length

  i <- j + 1
  j <- i + 1 # skip the 'VALUE LABELS' row
  varLbls <- list() # prep vars for getting the labels
  varName <- ""

  while (controlFile[i] != ".") { # the '.' will signal the end of the section
    tempStr <- trimws(controlFile[i], which = "both")


    if (substr(tempStr, 1, 1) == "/" || any(grepl(tolower(tempStr), tolower(dict$variableName), fixed = TRUE))) { # indicates beginning of definition, use 'fixed=TRUE' here in case of special regex chars

      # found new var, write the labels to the list if we have them
      if (sum(nchar(varName)) > 0 && sum(nchar(varLblList[["Value"]])) > 0) {
        keep <- grepl("[0-9A-z]", varLblList[["Value"]], ignore.case = TRUE)

        varLblList[["Value"]] <- varLblList[["Value"]][keep]
        varLblList[["Label"]] <- varLblList[["Label"]][keep]

        if (length(varLblList[["Value"]]) > 0) {
          dict$labelValues[tolower(dict$variableName) == tolower(varName)] <- paste(varLblList[["Value"]], varLblList[["Label"]], sep = "=", collapse = "^") # create the proper labelValue type of string '1=One^2=Two'
        }
      }

      # get the new variable name without the "/" char
      varName <- tolower(trimws(gsub("/", "", tempStr, fixed = TRUE), which = "both")) # ensure it's lower case to match

      # do any prep for parsing labels
      varLblList <- list()
    } else {
      if (substr(tempStr, 1, 1) == "\"") { # check if first character is a double-quote as the value will be in double-quotes in addition to the varlabel
        xMatch <- regexpr("^[\"].+?[\"]\\s", tempStr) # find first quoted item with space after.  use ? to indicate lazy quantifier here, or can cause issue if double quotes in string
      } else {
        xMatch <- regexpr("^[^\"]*", tempStr) # find all characters before first double-quote char
      }

      varValue <- trimws(regmatches(tempStr, xMatch), which = "both")

      # check if value in quotes::remove if so
      if (substr(varValue, 1, 1) == "\"" && substr(varValue, nchar(varValue), nchar(varValue)) == "\"") {
        varValue <- substr(varValue, 2, nchar(varValue) - 1)
      }

      # test if a numeric range such as '1 - 20' or '1 - 10' is specified
      # these ranges will be converted to NA for removal
      # as they seem to be indicating a valid range of values instead of specific variable labels
      varValue[grepl("//d* - //d*", varValue, ignore.case = TRUE)] <- NA

      if (!is.na(varValue)) {
        # get the text label
        xMatch <- regexpr("\\s[\"].*[\"]$", tempStr) # find the text between the double quotes at the end of the string
        varLabel <- trimws(regmatches(tempStr, xMatch), which = "both")
        varLabel <- substr(varLabel, 2, nchar(varLabel) - 1) # remove the beginning and end double quotes

        # fix any label char values that don't display correctly.
        # The actual character is a 'replacement char' (\uFFFD) but it's raw is converted to three seperate chars: (\u00EF) (\u00BF) (\u00BD)
        varLabel <- gsub("\u00EF\u00BF\u00BD", "'", varLabel, ignore.case = TRUE)
        varLabel <- gsub("\u00E2", "'", varLabel, ignore.case = TRUE) # converts an latin small 'a' with circumflex (u00E2) to apostraphe (u0027)

        # get a list of the value and lables
        varLblList[["Value"]] <- c(varLblList[["Value"]], varValue)
        varLblList[["Label"]] <- c(varLblList[["Label"]], varLabel)
      }
    }

    i <- i + 1
  }

  # add the last variable labels if applicable
  if (nchar(varName) > 0 && length(varLblList[["Value"]]) > 0) {
    dict$labelValues[dict$variableName == varName] <- paste(varLblList[["Value"]], varLblList[["Label"]], sep = "=", collapse = "^") # create the proper labelValue type of string '1=One^2=Two'
  }

  # need to populate the Type, pvWt and weights dict sublists so we can convert the list to a data.frame
  dict$Type <- rep("", times = length(dict$variableName))
  dict$pvWt <- rep("", times = length(dict$variableName))
  dict$weights <- rep(FALSE, times = length(dict$variableName))

  # need to update the position start/end variables as the positions are thrown off by having SPSS tables defined::tables will be removed
  dict$Start <- c(1, 1 + cumsum(dict$Width))[seq_along(dict$Width)]
  dict$End <- cumsum(dict$Width)

  return(data.frame(dict, stringsAsFactors = FALSE))
}

# This function will evaluate file format valueLabels where valueLabels are defined
# for continuous variables that are not omittedLevels, but shouldn't be there and removes them from the returned fileFormat
#' @keywords internal
valueLabelCleanupFF <- function(fileFormat, omittedLevels, valLblsToCheck) {
  newFF <- fileFormat

  if (is.null(newFF) || nrow(newFF) == 0) {
    return(newFF)
  }

  for (i in 1:nrow(newFF)) {
    loopLV <- newFF$labelValues[[i]]

    if (is.null(loopLV) || nchar(loopLV) == 0) {
      next
    }

    x <- strsplit(loopLV, "^", fixed = TRUE)
    y <- sapply(x, function(z) {
      strsplit(z, "=", fixed = TRUE)
    })
    key <- sapply(y, function(z) {
      z[1]
    })
    lbl <- sapply(y, function(z) {
      paste(z[-1], sep = "", collapse = "=")
    })

    if (all(lbl %in% c(omittedLevels, valLblsToCheck))) {
      idxToRemove <- which(lbl %in% valLblsToCheck, arr.ind = TRUE) # we only want to omit the special case labels

      if (length(idxToRemove) > 0) { # ensure it wasn't just all defined omittedLevels
        key <- key[-idxToRemove]
        lbl <- lbl[-idxToRemove]

        newFF$labelValues[i] <- paste(key, lbl, sep = "=", collapse = "^")
      }
    }
  }

  return(newFF)
}

# This function will evaluate the variable names in the included fileformat that should be categorical; but for some reason are not
# It constructs the addtional value labels and modifies the fileFormat 'labelsValue' field to reflect the categories for the supplied variableNames.
# data param should be a data.frame with matching variableName columns to analyze for constructing the additional value labels
valueLabel_MakeCategorical <- function(fileFormat, variableNames, data) {
  subFF <- subset(fileFormat, fileFormat$variableName %in% variableNames)

  if (is.null(subFF) || nrow(subFF) == 0) {
    return(fileFormat)
  }

  for (i in 1:nrow(subFF)) {
    loopLV <- subFF$labelValues[[i]]

    if (is.null(loopLV) || nchar(loopLV) == 0) {
      next
    }

    x <- strsplit(loopLV, "^", fixed = TRUE)
    y <- sapply(x, function(z) {
      strsplit(z, "=", fixed = TRUE)
    })
    key <- sapply(y, function(z) {
      z[1]
    })
    lbl <- sapply(y, function(z) {
      paste(z[-1], sep = "", collapse = "=")
    })

    uniqueVals <- unique(data[ , subFF$variableName[[i]]])[[1]]
    uniqueVals <- uniqueVals[order(uniqueVals)]

    if (length(uniqueVals) > 0 && !is.null(uniqueVals)) {
      for (ii in seq_along(uniqueVals)) {
        if (!(uniqueVals[[ii]] %in% key) && !(uniqueVals[[ii]] %in% lbl)) {
          key <- c(key, uniqueVals[ii])
          lbl <- c(lbl, uniqueVals[ii])
        }
      }

      # apply this back to the fileFormat for returning
      fileFormat$labelValues[fileFormat$variableName == subFF$variableName[[i]]] <- paste(key, lbl, sep = "=", collapse = "^")
    }
  }

  return(fileFormat)
}

# the .csv files for the RUD data are accompanied by two .txt files
# the *_metadata.txt and the *_layout.txt
# this takes in those two files and returns a formatted fileFormat object
#' @keywords internal
getMetaFormatDictionary <- function(metaDataFile, formatDataFile) {
  if (!file.exists(metaDataFile)) {
    stop(paste0("No metadata file found at path ", metaDataFile, "."))
  }

  if (!file.exists(formatDataFile)) {
    stop(paste0("No format layout file found at path ", formatDataFile, "."))
  }

  # prepare return dictionary
  dict <- list(
    "variableName" = character(0),
    "Start" = integer(0),
    "End" = integer(0),
    "Width" = integer(0),
    "Decimal" = integer(0),
    "Labels" = character(0),
    "labelValues" = character(0),
    "Type" = character(0),
    "pvWt" = character(0),
    "dataType" = character(0),
    "weights" = character(0)
  )

  linesFormat <- readLines(formatDataFile)
  linesMeta <- readLines(metaDataFile)

  # omit first line as it just states its metadata
  linesMeta <- linesMeta[-1]

  colsFormat <- strsplit(linesFormat, "|", fixed = TRUE) # creates a list object
  colsFormat <- data.frame(t(sapply(colsFormat, c)), stringsAsFactors = FALSE) # convert the list to data.frame
  colnames(colsFormat) <- c("varname", "format", "recIndex")
  colsFormat$varname <- tolower(colsFormat$varname)

  colsMeta <- strsplit(linesMeta, "|", fixed = TRUE)
  colsMeta <- data.frame(t(sapply(colsMeta, c)), stringsAsFactors = FALSE)
  colnames(colsMeta) <- c("flag1", "flag2", "varname", "label", "value")

  # subset the variable labels from the description labels
  colLbls <- subset(colsMeta, flag1 == 0)
  colValLbls <- subset(colsMeta, flag1 == 1)

  colsFormat$sortOrd <- 1:nrow(colsFormat)
  colsFormat <- merge(colsFormat, colLbls, by = "varname", all.x = TRUE, all.y = FALSE)
  colsFormat <- colsFormat[order(colsFormat$sortOrd), ]

  dict$variableName <- colsFormat$varname
  dict$Start <- seq_along(1:nrow(colsFormat)) # keep a sort order of sorts
  dict$End <- rep(NA, nrow(colsFormat))
  dict$Width <- rep(NA, nrow(colsFormat))
  dict$Decimal <- rep(NA, nrow(colsFormat))
  dict$Labels <- colsFormat$label
  dict$labelValues <- rep("", nrow(colsFormat))
  dict$Type <- rep("", nrow(colsFormat))
  dict$pvWt <- rep("", nrow(colsFormat))
  dict$dataType <- rep("", nrow(colsFormat))
  dict$weights <- rep("", nrow(colsFormat))

  dict <- data.frame(dict, stringsAsFactors = FALSE)

  for (v in dict$variableName) {
    lblSubset <- colValLbls[colValLbls$varname == v, ]

    keys <- lblSubset$value
    lbls <- lblSubset$label

    dict[dict$variableName == v, "labelValues"] <- paste(keys, lbls, collapse = "^", sep = "=")
  }

  # now work with the layout file to gather the datatype and the precision
  lines <- readLines(formatDataFile)
  cols <- strsplit(lines, "|", fixed = TRUE) # creates a list object
  cols <- data.frame(t(sapply(cols, c)), stringsAsFactors = FALSE) # convert the list to data.frame

  numericVars <- colsFormat$varname[grepl("^F", colsFormat$format)] # get the names of all the numeric variables
  charVars <- colsFormat$varname[!grepl("^F", colsFormat$format)]

  dict$dataType[tolower(dict$variableName) %in% tolower(numericVars)] <- "numeric"
  dict$dataType[tolower(dict$variableName) %in% tolower(charVars)] <- "character"

  # pull out the decimal places and widths
  dict$Decimal <- as.numeric(ifelse(substr(colsFormat$format, 1, 1) == "F" & grepl(".", colsFormat$format, fixed = TRUE), sapply(strsplit(colsFormat$format, "\\."), function(x) {
    tail(x, 1)
  }), rep(NA, nrow(dict))))
  dict$Width <- gsub("[a-zA-Z]", "", sapply(strsplit(colsFormat$format, "\\."), function(x) {
    head(x, 1)
  }))

  # lastly, convert numeric values to integer when they really should be an integer
  dict$dataType[dict$dataType == "numeric" & is.na(dict$Decimal) & dict$Width < 8] <- "integer"
  dict$Decimal[dict$dataType == "integer"] <- 0

  return(dict)
}

# parses the NCES 'Layout*.txt' file to a FWF .dat data file.
#' @keywords internal
parseTEXTFileFormat_NCES <- function(inTxtFile) {
  dict <- list(
    "variableName" = character(0),
    "Start" = integer(0),
    "End" = integer(0),
    "Width" = integer(0),
    "Decimal" = integer(0),
    "Labels" = character(0),
    "labelValues" = character(0),
    "Type" = character(0),
    "pvWt" = character(0),
    "dataType" = character(0),
    "weights" = character(0)
  )

  # Read in spss control files
  con <- file(inTxtFile)
  controlFile <- readLines(con)
  close.connection(con)

  # prep for processing
  controlFile <- gsub("[^[:print:]]", "", controlFile) # remove unprintable characters
  controlFile <- trimws(controlFile, which = "both") # remove leading or ending whitespace
  controlFile <- controlFile[controlFile != ""] # remove blank rows


  # Dataset Name
  iPos <- which(controlFile == "/* ASCII Dataset File Name */")[1]
  if (length(iPos) == 0) {
    stop(paste0("Unable to locate ASCII Dataset File Name in file: ", dQuote(inTxtFile), ))
  }
  datasetName <- trimws(controlFile[iPos + 1], which = "both")

  # Record Length
  iPos <- which(controlFile == "/* Total Record Length */")[1]
  if (length(iPos) == 0) {
    stop(paste0("Unable to locate ASCII Total Record Length in file: ", dQuote(inTxtFile), ))
  }
  maxRecordLen <- as.numeric(trimws(controlFile[iPos + 1], which = "both"))

  # Variable Names and FWF Positions, and Variable Descriptions
  iPos <- which(controlFile == "/* Variable Names, Locations, and Descriptions */")[1]
  endPos <- which(controlFile == "/* Variable Value Labels */")[1]

  lineChunk <- controlFile[(iPos + 1):(endPos - 1)]

  varMatch <- regexpr("^\\w+\\s{0,}\\d+-\\d+", lineChunk) # get first word in line and include the position chars as sometimes they are right next to each other with no space
  varName <- tolower(regmatches(lineChunk, varMatch)) # use lower case for the names
  varName <- trimws(gsub("\\d+-\\d+", "", varName), which = "both") # remove the position indicator  ###-###
  names(varName) <- varName # store the variable name here for later use

  posMatch <- regexpr("\\d+-\\d+", lineChunk) # finds the digits and the dash with at least two spaces on either side
  pos <- trimws(regmatches(lineChunk, posMatch), which = "both")

  posStartMatch <- regexpr("\\d+-", pos) # get digits before '-' char
  posStart <- trimws(gsub("-", "", regmatches(pos, posStartMatch)), which = "both")
  posStart <- as.integer(posStart)

  posEndMatch <- regexpr("-\\d+", pos) # get digits after '-' char
  posEnd <- trimws(gsub("-", "", regmatches(pos, posEndMatch)), which = "both")
  posEnd <- as.integer(posEnd)

  # remove the first occurance of the var name as well as the position indicators
  descMatch <- regexpr("\\d+-\\d+.*$", lineChunk)
  varDesc <- regmatches(lineChunk, descMatch) # grab matches, but still need to remove position
  varDesc <- gsub("^\\d+-\\d+", "", varDesc) # remove position indicator

  # trim
  varDesc <- trimws(varDesc, which = "both")

  # in special cases the variable name is adjacent to the start-end positions
  # we need to test for these and clean them up otherwise it will hard crash LaF and R
  # Example: F3ICREDDBLMAJ_1130-132 -->  The true var name is F3ICREDDBLMAJ_1 and the start is position 130
  # outOfRangeIndex <- which(posStart > maxRecordLen, arr.ind = TRUE)
  outOfRangeIndex <- which(posStart > posEnd, arr.ind = TRUE)

  for (ri in outOfRangeIndex) {
    xChr <- substr(posStart[ri], 1, 1)
    varName[ri] <- paste0(varName[ri], xChr)
    names(varName)[ri] <- varName[ri] # update name to be consistent
    posStart[ri] <- as.numeric(substr(posStart[ri], 2, nchar(posStart[ri])))
  }

  # check for any variable name duplication here
  # duplicated names can occur when the variable name is too long for the file layout specification
  # we need to assume that the variable is numbered: '1', '2', '3', etc.
  if (anyDuplicated(varName) > 0) {
    dupeNames <- unique(varName[duplicated(varName)]) # gather the specific duplicated variable names
    for (dN in dupeNames) {
      dupeIndex <- which(varName %in% dN, arr.ind = TRUE) # find which index the dupes are located in the overall vector

      # number the duplicates(1, 2, 3, etc.) in the order they exist in the original vector
      for (i in seq_along(dupeIndex)) {
        varName[dupeIndex[i]] <- paste0(dN, i)
      }
    }
  }

  # for some reason even between the fwf datasets the ASCII layout files have slight variations between their column specs
  # no need to offset if the end positions equal the
  if (posEnd[1] == posStart[2]) {
    fwfWidthOffset <- 0
  } else {
    fwfWidthOffset <- 1
  }

  # update the dictionary
  dict$variableName <- varName
  dict$Start <- posStart
  dict$End <- posEnd
  dict$Width <- (dict$End - dict$Start) + fwfWidthOffset
  dict$Decimal <- rep("", length(varName))
  dict$Labels <- varDesc
  dict$labelValues <- rep("", length(varName))
  dict$Type <- rep("", times = length(dict$variableName))
  dict$pvWt <- rep("", times = length(dict$variableName))
  dict$dataType <- rep("character", times = length(dict$variableName)) # default all to character for now
  dict$weights <- rep(FALSE, times = length(dict$variableName))
  #########################

  # Get the Variable Label section here in chunk
  iPos <- which(controlFile == "/* Variable Value Labels */")[1]

  lineChunk <- controlFile[(iPos + 1):length(controlFile)] # get all the variable labels as on chunk

  # some variable names were trimmed in the layout specs
  # identify which variables have no labels and compare
  noLabelVars <- lineChunk[!grepl("[0-9].*[=].*", lineChunk, ignore.case = TRUE)]
  noLabelVars <- noLabelVars[!(tolower(noLabelVars) %in% tolower(dict$variableName))] # omit any vars we already can match on to limit our pool

  for (v in noLabelVars[!is.na(noLabelVars)]) {
    if (any(substr(tolower(v), 1, 15) == tolower(dict$variableName))) { # NCES text file layouts only have space for 15 characters, rest of the chars are cutoff
      dict$variableName[tolower(dict$variableName) == substr(tolower(v), 1, 15)] <- paste0(tolower(v))
    }
  }


  # get variable row indexes
  subChunk <- which(tolower(lineChunk) %in% tolower(dict$variableName), arr.ind = TRUE)

  if (length(subChunk) > 0) {
    for (varIndex in seq_along(subChunk)) {
      tempVarName <- tolower(lineChunk[subChunk[varIndex]])

      nextChunkIndex <- subChunk[varIndex + 1]
      if (is.na(nextChunkIndex)) {
        nextChunkIndex <- length(lineChunk)
      } else {
        nextChunkIndex <- nextChunkIndex - 1 # we want lines between vars
      }

      lblLines <- lineChunk[(subChunk[(varIndex)] + 1):nextChunkIndex]
      splitPos <- regexpr("=", lblLines)

      # get everything before the first '=' symbol
      lblVals <- trimws(substr(lblLines, 1, (splitPos - 1)), which = "both")

      lblTxt <- trimws(substring(lblLines, (splitPos + 1)), which = "both")
      lblTxt <- substr(lblTxt, 2, (nchar(lblTxt) - 1)) # remove leading/ending quotes

      valsForCleanup <- grepl(" - ", lblVals, fixed = TRUE) # these define a sort of value range but are not true value labels

      valsToAdd <- c()
      lblsToAdd <- c()

      lblVals <- c(valsToAdd, lblVals[!valsForCleanup])
      lblTxt <- c(lblsToAdd, lblTxt[!valsForCleanup])

      # fix any label char values that don't display correctly.
      # The actual character is a 'replacement char' (\uFFFD) but it's raw is converted to three seperate chars: (\u00EF) (\u00BF) (\u00BD)
      lblTxt <- gsub("\u00EF\u00BF\u00BD", "'", lblTxt, ignore.case = TRUE)
      lblTxt <- gsub("\u00E2", "'", lblTxt, ignore.case = TRUE) # converts an latin small 'a' with circumflex (u00E2) to apostraphe (u0027)

      dict$labelValues[dict$variableName == tempVarName] <- paste(lblVals, lblTxt, sep = "=", collapse = "^")
    }
  } # end if(length(subChunk)>0)

  # ensure rows are sorted before returning so the start/width positon is correct
  finalFormat <- data.frame(dict, stringsAsFactors = FALSE)
  finalFormat <- finalFormat[order(finalFormat$Start), ]

  # check for any gaps in the start/end positions of the defined fileformat, LaF cannot support missing gaps as it relies on width value
  finalFormat <- validateFWF_FileFormat(finalFormat)

  return(finalFormat)
}

# checks for any missing numeric gaps between the start and end postions and inserts 'xGAP' fields for LaF to operate properly on FWF file
#' @keywords internal
validateFWF_FileFormat <- function(fileFormat) {
  startPos <- min(fileFormat$Start)
  endPos <- max(fileFormat$End)

  if (startPos != 1) {
    stop("Improper file format starting position found.  All fixed-with-format files should start with a numerical value of '1'")
  }

  posIndex <- data.frame(index = startPos:endPos)
  posIndex$chkFlag <- FALSE # default all check flags to false initially

  # ensure file format is ordered correctly
  fileFormat <- fileFormat[order(fileFormat$Start), ]

  for (xVar in fileFormat$variableName) {
    xStart <- fileFormat[fileFormat$variableName == xVar, "Start"]
    xEnd <- fileFormat[fileFormat$variableName == xVar, "End"]

    for (idx in xStart:xEnd) {
      posIndex[posIndex$index == idx, "chkFlag"] <- TRUE # has a value
    }
  }

  posIndex <- posIndex[posIndex$chkFlag == FALSE, ]

  if (nrow(posIndex) > 0) {
    for (idx in posIndex$index) {
      rnames <- row.names(fileFormat)
      ri <- nrow(fileFormat) + 1

      fileFormat[ri, "variableName"] <- paste0("x___fileformatgap", idx)
      fileFormat[ri, "Start"] <- idx
      fileFormat[ri, "End"] <- idx
      fileFormat[ri, "Width"] <- 1
      fileFormat[ri, "Decimal"] <- NA
      fileFormat[ri, "Labels"] <- paste0("Gap in File Format Definition. Column Position: ", idx)
      fileFormat[ri, "labelValues"] <- ""
      fileFormat[ri, "Type"] <- ""
      fileFormat[ri, "pvWt"] <- ""
      fileFormat[ri, "dataType"] <- "character"
      fileFormat[ri, "weights"] <- FALSE

      # ensure row.names are properly applied
      row.names(fileFormat) <- c(rnames, paste0("x___fileformatgap", idx))
    }

    # reorder before returning
    fileFormat <- fileFormat[order(fileFormat$Start), ]
  }

  return(fileFormat)
}

# writes a dataframe to the savePath using the fileFormat specifications
# returns the fileFormat object as it might involve updates if the actual data is larger than specified in the fileFormat
#' @keywords internal
writeDF_FWF <- function(df, fileFormat, savePath, verbose) {
  if (is.null(df)) {
    stop(paste0(sQuote("df"), " cannot be Null."))
  }

  if (nrow(df) == 0) {
    stop(paste0(sQuote("df"), " must contain at least 1 row."))
  }

  # this ensures we get the full entire decimal value, otherwise it might round or do something odd
  userOp <- options(digits = 15)
  on.exit(options(userOp), add = TRUE)

  # for newer R, memory.limit() makes a warning and is inf
  if (suppressWarnings(memory.limit() < 128000)) {
    memory.limit(128000)
  }
  omat <- matrix(nrow = nrow(df), ncol = nrow(fileFormat))

  # ensure all fields have a dataType at this point for inspection
  fileFormat$dataType[is.na(fileFormat$dataType)] <- "character"

  # ======
  for (coli in 1:nrow(fileFormat)) {
    if (verbose) {
      if (((coli %% 2500) == 0) || coli == 1) {
        if (coli == 1) {
          cat(paste0("Caching data to file ", sQuote(savePath), ".\n"))
        }
        cat(paste0("Processing column ", coli, " to ", min((coli + 2500) - ifelse(coli == 1, 2, 1), ncol(omat)), " of ", ncol(omat), "\n"))
      }
    }

    # ensure we have no out of bounds values per the file format specification::this also converts any scientific notation numbers to their full character count
    colData <- df[[coli]]

    # test the type and precision is what it should be
    if (is.numeric(colData) && any(grepl(".", colData, fixed = TRUE), na.rm = TRUE)) {
      fileFormat$dataType[coli] <- "numeric"

      decTestLen <- num.decimals(colData) # get the decimal precision (not base function, included in readUTILS.R)
      decTestLen[is.na(decTestLen)] <- 0 # convert NAs to 0 for testing

      # nsmall param for format has max limit of 20 decimal places!!
      if (any(decTestLen > 20)) {
        decTestLen[decTestLen > 20] <- 20
      }

      charTest <- nchar(format(colData, trim = TRUE, scientific = FALSE, nsmall = max(decTestLen)), keepNA = TRUE)
      charTest[is.na(colData)] <- 0 # this sets the NaN and NA values to nchar of 0

      # update all the format and decimal values for these numerics regardless of what the SPSS formatting says
      fileFormat$Width[coli] <- max(charTest)
      fileFormat$Decimal[coli] <- max(decTestLen)
      fileFormat$dataType[coli] <- "numeric"
    } else { # test there is enough width for character strings too

      charTest <- nchar(format(colData, trim = TRUE, scientific = FALSE), keepNA = TRUE)
      charTest[is.na(colData)] <- 0 # this sets the NaN and NA values to nchar of 0

      fileFormat$Width[coli] <- max(charTest)

      if (fileFormat$dataType[coli] == "numeric" && fileFormat$Width[coli] < 9) {
        fileFormat$dataType[coli] <- "integer"
        fileFormat$Decimal[coli] <- 0 # it's not a decimal regardless of what
      } else if (fileFormat$dataType[coli] == "numeric" && fileFormat$Width[coli] >= 9) { # contains no decimals
        fileFormat$Decimal[coli] <- 0
      }
    } # end if(is.numeric(df[[coli]]) && any(grepl(".", df[[coli]], fixed = TRUE), na.rm = TRUE))

    # prepare the data for adding to the omat matrix
    dataType <- fileFormat$dataType[coli]
    width <- fileFormat$Width[coli]
    decimal <- fileFormat$Decimal[coli]

    if (tolower(dataType) %in% c("numeric") && decimal > 0) {
      colData[!is.na(colData)] <- format(colData[!is.na(colData)], scientific = FALSE, width = width, nsmall = decimal, justify = "right")
      colData[is.na(colData)] <- format("", scientific = FALSE, width = width, justify = "right")
    } else {
      colData[!is.na(colData)] <- format(colData[!is.na(colData)], scientific = FALSE, width = width, nsmall = 0, justify = "right")
      colData[is.na(colData)] <- format("", scientific = FALSE, width = width, justify = "right")

      # '\\' in text is read as 1 space, so add 1 to the width
      colData[grepl("\\\\", colData)] <- format(colData[grepl("\\\\", colData)], scientific = FALSE, width = width + 1, justify = "right")
    }

    omat[ , coli] <- colData
  } # end for(coli in 1:nrow(fileFormat))

  # write the file out to the cache location
  outFile <- tryCatch(
    {
      file(savePath, "w")
    },
    error = function(e) {
      stop(paste0(
        "Unable to write to cache file.\n Please ensure you have write permissions to the save path: ", sQuote(savePath), "\n",
        "Error message: ", e
      ))
    }
  )

  for (i in 1:nrow(omat)) {
    if (verbose) {
      if ((i %% 2500) == 0 || i == 1) {
        cat(paste0("Writing data row ", i, " to ", min((i + 2500) - ifelse(i == 1, 2, 1), nrow(omat)), " of ", nrow(omat), "\n"))
      }
    }


    writeStr <- paste0(omat[i, ], collapse = "")
    writeLines(writeStr, outFile)
  }

  close(outFile)

  # recalibrate the Start/End Positions before returning the fileFormat
  fileFormat$Start <- c(1, 1 + cumsum(fileFormat$Width))[1:nrow(fileFormat)]
  fileFormat$End <- cumsum(fileFormat$Width)

  return(fileFormat)
}

# calculates the number of decimal places behind the decimal point for a numeric
#' @keywords internal
num.decimals <- function(x) {
  if (!inherits(x, "numeric")) {
    eout("Invalid Datatype for num.decimals function.")
  }

  # as.character here doesn't grab the full decimal values, need to incorporate format as well
  x <- trimws(format(x, digits = 15, scientific = FALSE))

  x <- sub("0+$", "", x)
  x <- sub("^.+[.]", "", x)
  return(nchar(x))
}

# parses the .SAS syntax file into a fileFormat object for use with the HS&B Study and NLS-72 study ONLY as currently tested and validated.
#' @keywords internal
parseSAS_FileFormat_HSB <- function(sasFile) {
  # prepare return dictionary
  dict <- list(
    "variableName" = character(0),
    "Start" = integer(0),
    "End" = integer(0),
    "Width" = integer(0),
    "Decimal" = integer(0),
    "Labels" = character(0),
    "labelValues" = character(0),
    "Type" = character(0),
    "pvWt" = character(0),
    "dataType" = character(0),
    "weights" = character(0)
  )

  # get lines
  lines <- readLines(sasFile, encoding = "UTF-8")
  lines <- trimws(lines, which = "both")

  # find start position of the 'INPUT'
  startIdx <- which(lines[] == "INPUT", arr.ind = TRUE) + 1
  endIdx <- startIdx + 1

  # find the ending terminator of ';' to signal the INPUT end
  while (TRUE) {
    currentLine <- lines[endIdx]
    if (currentLine == ";") {
      break
    } else {
      endIdx <- endIdx + 1
    }
  }

  # process lines.  values in order from left to right
  # ordinal pos | @start | variable name | datatype | FWF start-end positions
  # sample lines:
  # /* 4576 */     @13406 FI123L          2.0          /* 13406 - 13407 */
  # /* 4577 */     @13408 FI123M          2.0          /* 13408 - 13409 */
  # /* 4578 */     @13410 FI123N          2.0          /* 13410 - 13411 */
  inputLines <- lines[startIdx:(endIdx - 1)]
  inputLinesSplit <- strsplit(inputLines, "*/", fixed = TRUE) # split by */ first

  ordPos <- sapply(inputLinesSplit, function(x) {
    x[1]
  })
  ordPos <- trimws(gsub("/*", "", ordPos, fixed = TRUE), which = "both")
  ordPos <- as.numeric(ordPos)

  # take the second half from the first split, then split it again on '/*'
  inputLinesSplit <- sapply(inputLinesSplit, function(x) {
    x[2]
  })
  inputLinesSplit <- strsplit(inputLinesSplit, "/*", fixed = TRUE)

  # grab the second part after split, then split it again on the '-' to get the start and end numeric positions
  fwfPos <- vapply(inputLinesSplit, function(x) {
    x[2]
  }, FUN.VALUE=character(1))
  fwfPos <- strsplit(fwfPos, "-", fixed = TRUE)

  startPos <- as.numeric(trimws(sapply(fwfPos, function(x) {
    x[1]
  }), which = "both"))
  endPos <- as.numeric(trimws(sapply(fwfPos, function(x) {
    x[2]
  }), which = "both"))

  # take the first split section from the first split, then split it on spaces (removing empty spacing) to leave the
  # start position, variable name, and datatype fields in the list
  varInfo <- trimws(sapply(inputLinesSplit, function(x) {
    x[1]
  }), which = "both")
  varInfoSplit <- strsplit(varInfo, " ", fixed = TRUE)
  varInfoSplit <- lapply(varInfoSplit, function(x) {
    x[x != ""]
  }) # remove any blank/empty spacing elements from strsplit

  varName <- tolower(sapply(varInfoSplit, function(x) {
    x[2]
  }))
  dataType <- sapply(varInfoSplit, function(x) {
    x[3]
  })


  # grab the numeric precision and scale
  precision <- suppressWarnings(sapply(strsplit(dataType, ".", fixed = TRUE), function(x) {
    as.numeric(tail(x, 1))
  }))
  scale <- suppressWarnings(sapply(strsplit(dataType, ".", fixed = TRUE), function(x) {
    as.numeric(head(x, 1))
  }))

  dataType[substr(dataType, 1, 5) == "$CHAR"] <- "character"

  # convert dataType into LaF dataType descriptors
  dataType[precision > 0 | scale > 8] <- "numeric" # if precision greater than 8 or has decimal then it's numeric
  dataType[!(dataType %in% c("character", "numeric"))] <- "integer"

  dict$variableName <- varName
  dict$Start <- startPos
  dict$End <- endPos
  dict$Width <- (endPos - startPos) + 1 # be sure the width is defined by the /* 00001 - 00006 */ lines
  dict$Decimal <- precision
  dict$Labels <- rep("", times = length(dict$variableName))
  dict$labelValues <- rep("", times = length(dict$variableName))
  dict$Type <- rep("", times = length(dict$variableName))
  dict$pvWt <- rep("", times = length(dict$variableName))
  dict$dataType <- dataType
  dict$weights <- rep(FALSE, times = length(dict$variableName))

  ff <- data.frame(dict, stringsAsFactors = FALSE)

  # Get the variable labels from the 'LABEL' section
  startIdx <- which(lines[] == "LABEL", arr.ind = TRUE) + 1
  endIdx <- startIdx

  # find the ending terminator of ';' to signal the INPUT end
  while (TRUE) {
    currentLine <- lines[endIdx]
    if (currentLine == ";") {
      break
    } else {
      endIdx <- endIdx + 1
    }
  }

  lblLines <- lines[startIdx:(endIdx - 1)]
  lblSplit <- strsplit(lblLines, "=", fixed = TRUE)
  lblVar <- tolower(sapply(lblSplit, function(x) {
    x[1]
  }))
  lblDesc <- sapply(lblSplit, function(x) {
    paste(x[-1], sep = "", collapse = "=")
  }) # in case there is a '=' symbol use paste here
  lblDesc <- substr(lblDesc, 2, nchar(lblDesc) - 1) # remove single quotes surrounding the text description

  for (lv in lblVar) {
    ff$Labels[ff$variableName == lv] <- lblDesc[which(ff$variableName == lv, arr.ind = TRUE)]
  }

  # gather the value labels.  there is no set 'page' of these as it's done as a proc command
  startIdx <- which(lines[] == "PROC FORMAT;", arr.ind = TRUE) + 1
  endIdx <- startIdx

  # find the ending terminator of ';' to signal the INPUT end
  while (TRUE) {
    currentLine <- lines[endIdx]
    if (currentLine == "RUN;") {
      break
    } else {
      endIdx <- endIdx + 1
    }
  }

  lblLines <- lines[startIdx:(endIdx - 1)]
  currentVar <- ""
  valLblList <- list()

  for (testLine in lblLines) {
    if (grepl("^VALUE ", testLine, ignore.case = TRUE)) {
      if (nchar(currentVar) > 0 && sum(nchar(valLblList[["Value"]])) > 0) {
        # add the val/label pair to the fileFormat as we found a new variable definition

        # need to check that the currentVar is found in the vector if variableNames in the fileFormat as SAS has quirky formatting definitions
        # that don't always match up to the true variable name
        if (!currentVar %in% ff$variableName) {
          currentVar <- ff$variableName[which(currentVar == substr(ff$variableName, 1, nchar(currentVar)), arr.ind = TRUE)[1]]
        }

        ff$labelValues[tolower(ff$variableName) == tolower(currentVar)] <- paste(valLblList[["Value"]], valLblList[["Label"]], sep = "=", collapse = "^")
        valLblList <- list()
      }

      testLine <- trimws(sub("^VALUE", "", testLine), which = "both") # remove the VALUE from the line

      xMatch <- regexpr("^(\\w+|[\\$]\\w+) ", testLine, ignore.case = TRUE) # note second matching group::some variables start with $
      currentVar <- trimws(regmatches(testLine, xMatch), which = "both")

      testLine <- sub(currentVar, "", testLine, fixed = TRUE) # remove the variable from the line since we got it
      testLine <- trimws(testLine, which = "both")
      currentVar <- tolower(currentVar)

      # for some strange reason the SAS syntax has an 'F' character appended to the variable name, this removes that
      currentVar <- substr(currentVar, 1, nchar(currentVar) - 1)

      # the $ denotes a character field, but the variable name doesn't include the $
      if (substr(currentVar, 1, 1) == "$") {
        currentVar <- substr(currentVar, 2, nchar(currentVar))
      }

      splitVals <- strsplit(testLine, "=", fixed = TRUE)
      val <- sapply(splitVals, function(x) {
        x[1]
      })
      desc <- sapply(splitVals, function(x) {
        paste(x[-1], sep = "", collapse = "=")
      })


      # check if value is wrapped in single quotes and remove
      if (grepl("^'.*'$", val)) {
        val <- substr(val, 2, nchar(val) - 1)
      }

      # do any cleanup for ^ and single quotes
      desc <- gsub("^", "'", desc, fixed = TRUE)

      if (grepl(";$", desc)) { # last item ends with ; character
        desc <- substr(desc, 2, nchar(desc) - 2)
      } else {
        desc <- substr(desc, 2, nchar(desc) - 1)
      }

      # add the val and desc to the list only if it's valid
      if (isValidSASValueLable_HSB(val, desc)) {
        if (grepl(",", val, fixed = TRUE)) { # multiple values specified for a specific label
          val <- unlist(strsplit(val, ",", fixed = TRUE))
          desc <- rep(desc, times = length(val))
        }
        valLblList[["Value"]] <- c(valLblList[["Value"]], val)
        valLblList[["Label"]] <- c(valLblList[["Label"]], desc)
      }
    } else { # not a new variable but another line for the same variable with a new value/label pair

      testLine <- trimws(testLine, which = "both") # remove the variable from the line since we got it

      splitVals <- strsplit(testLine, "=", fixed = TRUE)
      val <- sapply(splitVals, function(x) {
        x[1]
      })
      desc <- sapply(splitVals, function(x) {
        paste(x[-1], sep = "", collapse = "=")
      })

      # check if value is wrapped in single quotes and remove
      if (grepl("^'.*'$", val)) {
        val <- substr(val, 2, nchar(val) - 1)
      }

      # do any cleanup for ^ and single quotes
      desc <- gsub("^", "'", desc, fixed = TRUE)

      if (grepl(";$", desc)) { # last item ends with ; character
        desc <- substr(desc, 2, nchar(desc) - 2)
      } else {
        desc <- substr(desc, 2, nchar(desc) - 1)
      }

      if (isValidSASValueLable_HSB(val, desc)) {
        if (grepl(",", val, fixed = TRUE)) { # multiple values specified for a specific label
          val <- unlist(strsplit(val, ",", fixed = TRUE))
          desc <- rep(desc, times = length(val))
        }
        valLblList[["Value"]] <- c(valLblList[["Value"]], val)
        valLblList[["Label"]] <- c(valLblList[["Label"]], desc)
      }
    }
  } # end for(testLine in lblLines)

  # one last check for the last value/label item
  if (nchar(currentVar) > 0 && sum(nchar(valLblList[["Value"]])) > 0) {
    # add the val/label pair to the fileFormat as we found a new variable definition

    # need to check that the currentVar is found in the vector if variableNames in the fileFormat as SAS has quirky formatting definitions
    # that don't always match up to the true variable name
    if (!currentVar %in% ff$variableName) {
      currentVar <- ff$variableName[which(currentVar == substr(ff$variableName, 1, nchar(currentVar)), arr.ind = TRUE)[1]]
    }

    ff$labelValues[tolower(ff$variableName) == tolower(currentVar)] <- paste(valLblList[["Value"]], valLblList[["Label"]], sep = "=", collapse = "^")
    valLblList <- list()
  }

  return(ff)
}

# determine which value/label pairs are actually a valid SAS label; used with parseSAS_FileFormat_HSB
isValidSASValueLable_HSB <- function(value, label) {
  # NA's can cause issue in validation checks below
  value[is.na(value)] <- ""
  label[is.na(label)] <- ""

  # ensure we have an actual value
  if (value == ";" && nchar(label) > 0) {
    return(FALSE)
  }

  if (nchar(trimws(value, which = "both")) == 0 && nchar(trimws(label, which = "both")) == 0) {
    return(FALSE)
  }

  # omit any continuous range specifications as it's not needed
  if (grepl("^(\\d|-|.)+-(\\d|-|.)+$", value, ignore.case = TRUE)) { #-3.75-15.00
    return(FALSE)
  }

  return(TRUE)
}

#' @keywords internal
lightUnclassCols <- function(tbl) {
  # remove non-standard classes from data frame columns
  #
  # needed for haven read in
  for (i in seq_along(colnames(tbl))) {
    if (inherits(tbl[[i]], "haven_labelled")) {
      if (requireNamespace("haven")) {
        tbl[[i]] <- haven::as_factor(tbl[[i]])
      }
    } else {
      if (inherits(tbl[[i]], "lfactor")) {
        # do nothing, leave it as an lfactor
      } else {
        if (length(class(tbl[[i]])) == 1 & (inherits(tbl[[i]], "factor"))) {
          # do nothing, leave it as a single class factor
        } else {
          tbl[[i]] <- unclass(tbl[[i]])
        }
      }
    }
  }
  return(tbl)
}

#' remove non-standard classes from data frame columns
#'
#' removes the haven defined column classes from a returned tibble
#' needed for issues revolving around `user_na=TRUE` argument of haven 'read_sav' method parameter
#' @keywords internal
UnclassCols <- function(tbl) {
  if (ncol(tbl) > 0) {
    for (i in 1:ncol(tbl)) {
      tbl[[i]] <- unclass(tbl[[i]])
    }
  }

  return(tbl)
}


# returns an LaF to the fwf file using the fileformat specs
# fwfFilePath = file path to the fixed-width data file
# fwfFileFormat = data.frame of the fileFormat object that has fixed-width file definitions
#' @keywords internal
getFWFLaFConnection <- function(fwfFilePath, fwfFileFormat) {
  laf <- LaF::laf_open_fwf(fwfFilePath, column_types = fwfFileFormat$dataType, column_widths = fwfFileFormat$Width, column_names = tolower(fwfFileFormat$variableName))
}

# internal function to get a unique data.frame of all variable value labels of a fileFormat object (value & label)
#' @keywords internal
valueLabel_getUniqueDF <- function(fileFormat) {
  fullDF <- data.frame(key = character(0), lbl = character(0))

  for (i in 1:nrow(fileFormat)) {
    loopLV <- fileFormat$labelValues[[i]]

    if (is.null(loopLV) || nchar(loopLV) == 0) {
      next
    }

    x <- unlist(strsplit(loopLV, "^", fixed = TRUE))
    y <- sapply(x, function(z) {
      strsplit(z, "=", fixed = TRUE)
    })
    key <- sapply(y, function(z) {
      z[1]
    })
    lbl <- sapply(y, function(z) {
      paste(z[-1], sep = "", collapse = "=")
    })

    tmpDF <- data.frame(key, lbl)

    fullDF <- unique(rbind(fullDF, tmpDF))
  }

  return(fullDF)
}
