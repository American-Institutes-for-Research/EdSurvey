#' NOT CURRENTLY EXPORTED!  In Future this could potentially be made to a separate R package
#' THIS parseScript_SPSS function should be used 100% moving forward.
#' Old/Previous SPSS script parsers should be slowly transitioned to utilize this function when possible to maximize code use.

#' @title Parse SPSS Syntax Script for Fixed-Width Data Files
#'
#' @description Parses an SPSS Syntax Script (.sps) file to return information relating to fixed-width data files.
#'
#' @param spsFilePath a character value of the file path to the SPSS script to parse.
#' @param verbose a logic value to indicate if user wishes to print parsing activity to console. Default value is \code{FALSE}.
#' @param outputFormat a named argument to indicate which output format the resulting object should be.  See details for information on each format.
#'                     Currently, \code{data.frame} format is only supported.
#' @param encoding a character value to indicate the encoding specification that is used by \code{readLines} base function for the \code{spsFilePath} parameter.
#'                    Only adjust this parameter if the original file encoding of the file is known, is not producing correct string values, or other errors occur.
#'                    See \code{?readLines} help for details about it's use for file encoding, and additional details.
#'
#' @details The SPSS syntax script parser is focused on gathering details for use with fixed-width data files.  This function scans for the following SPSS commands:
#'    \itemize{
#'             \item{FILE HANDLE}
#'             \item{DATA LIST}
#'             \item{VARIABLE LABEL}
#'             \item{VALUE LABEL}
#'             \item{MISSING VALUE}
#'    }
#'
#' The \code{outputFormat} specified will determine the result object returned.  This function currently supports the following formats.
#'
#' \itemize{
#'      \item{data.frame}
#'           \itemize{
#'                \item{variableName - The variable name as defined in the script}
#'                \item{Start - The start number index of the variable defined for the fixed-width format layout}
#'                \item{End - The end number index of the variable defined for the fixed-width format layout}
#'                \item{Width - The length of how many columns the variable uses in the fixed-width format layout}
#'                \item{Attributes - Any SPSS attributes that are defined in the DATA LIST command.  This is typically only for field formatting.}
#'                \item{RecordNumber - Some fixed-width data files are considered "multi-line" where one record of data can span multiple rows in the file.
#'                The RecordNumber indicates which line the variable is assigned.}
#'                \item{Labels - The descriptive label associated with the variable name to give more detail or context.}
#'                \item{labelValues - For categorical variables a stored value will typically be assigned a longer label/definition. This string identifies these mappings.
#'                The '^' symbol is used to delimit each individual label value. Then additionally, the '=' is used to split the value from the left side of the '=' symbol,
#'                and the remaining right-hand side of '=' is the text label for that value.}
#'                \item{dataType - A best-guess of the data type (either 'numeric' or 'character') without actually examining the data-file.}
#'                \item{missingValues - If a MISSING VALUE clause is included in the script this will list the values that are considered 'Missing'.
#'                If multiple values specified, they will be delimited by a ';' (semi-colon) symbol.}
#'           }
#' }
#'
#' @return returns an object containing information specified by the \code{outputFormat} argument.
#'
#' @author Tom Fink

parseScript_SPSS <- function(spsFilePath, verbose = FALSE, outputFormat = c("data.frame"), encoding = getOption("encoding")) {
  if (!file.exists(spsFilePath)) {
    stop(paste0("Unable to locate ", dQuote("spssFP"), " file at location: ", dQuote(spsFilePath)))
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop(paste0("The ", sQuote("verbose"), " argument must be a logical vector of length 1."))
  }
  outputFormat <- match.arg(outputFormat)

  fCon <- file(spsFilePath, open = "rt", blocking = FALSE, encoding = encoding) # blocking = FALSE here so it reads entire file instead of first break
  on.exit(close(fCon))

  spsText <- tryCatch(readLines(fCon),
    error = function(e) {
      stop(paste0("Unable to read data file, readLines call failed: ", e))
    }
  )

  spsText <- paste(spsText, sep = "", collapse = "\n") # collapse as one single string, easier to parse going forward

  # break the text into its command parts for further processing. certain comments are removed here
  cmdText <- parseSPSS_COMMANDS(spsText)

  # define the command regex to locate them properly that we want
  fileHandleRE <- "^\\s*FILE HANDLE"
  dataListRE <- "^\\s*DATA LIST"
  variableLblRE <- "^\\s*VARIABLE LABEL(S){0,1}"
  missingValRE <- "^\\s*MISSING VALUE(S){0,1}"
  valueLblRE <- "^\\s*VALUE LABEL(S){0,1}"

  # create flags
  hasFileHandle <- any(grepl(fileHandleRE, cmdText, ignore.case = TRUE))
  hasDataList <- any(grepl(dataListRE, cmdText, ignore.case = TRUE))
  hasVariableLabels <- any(grepl(variableLblRE, cmdText, ignore.case = TRUE))
  hasMissingValues <- any(grepl(missingValRE, cmdText, ignore.case = TRUE))
  hasValueLabel <- any(grepl(valueLblRE, cmdText, ignore.case = TRUE))

  # procure return list of items
  retList <- list()

  if (hasFileHandle) {
    if (verbose) {
      cat(paste0("Parsing Script File Handle...\n"))
    }

    fileHandleText <- cmdText[grepl(fileHandleRE, cmdText, ignore.case = TRUE)]
    fileHandleData <- vector("list", length = length(fileHandleText))

    for (i in seq_along(fileHandleText)) {
      fileHandleData[[i]] <- parseSPSS_FILEHANDLE(fileHandleText[i])
    }

    retList$FileHandle <- fileHandleData
  } # end if(hasFileHandle)

  allVars <- character(0) # store all of the variable names here for functions downstream

  if (hasDataList) {
    if (verbose) {
      cat(paste0("Parsing Data List...\n"))
    }

    dataListText <- cmdText[grepl(dataListRE, cmdText, ignore.case = TRUE)]

    if (length(dataListText) > 1) {
      stop(paste0("Unable to handle multiple DATA LIST calls in SPSS script.  Please revise the script and retry."))
    }
    dataListData <- vector("list", length = length(dataListText))

    for (i in seq_along(dataListText)) {
      dataListData[[i]] <- parseSPSS_DATALIST(dataListText)
      allVars <- c(allVars, dataListData[[i]]$DataList$VariableName)
    }

    retList$DataList <- dataListData
  }

  if (hasVariableLabels) {
    if (hasDataList == FALSE || length(allVars) == 0) {
      stop("DATA LIST Command required in script for parsing variable labels.")
    }
    if (verbose) {
      cat(paste0("Parsing Variable Labels...\n"))
    }

    variableLabelText <- cmdText[grepl(variableLblRE, cmdText, ignore.case = TRUE)]
    variableLabelData <- vector("list", length = length(variableLabelText))

    for (i in seq_along(variableLabelText)) {
      variableLabelData[[i]] <- parseSPSS_VARIABLELABEL(variableLabelText[i], allVars, verbose)
    }

    retList$VariableLabels <- variableLabelData
  }

  if (hasMissingValues) {
    if (hasDataList == FALSE || length(allVars) == 0) {
      stop("DATA LIST Command required in script for parsing missing values.")
    }
    if (verbose) {
      cat(paste0("Parsing Missing Values...\n"))
    }
    missingValueText <- cmdText[grepl(missingValRE, cmdText, ignore.case = TRUE)]
    missingValueData <- vector("list", length = length(missingValueText))

    for (i in seq_along(missingValueText)) {
      missingValueData[[i]] <- parseSPSS_MISSINGVALUES(missingValueText[i], allVars)
    }

    retList$MissingValues <- missingValueData
  }

  if (hasValueLabel) {
    if (hasDataList == FALSE || length(allVars) == 0) {
      stop("DATA LIST Command required in script for parsing value labels.")
    }
    if (verbose) {
      cat(paste0("Parsing Value Labels...\n"))
    }
    valueLabelText <- cmdText[grepl(valueLblRE, cmdText, ignore.case = TRUE)]
    valueLabelData <- vector("list", length = length(valueLabelText))

    for (i in seq_along(valueLabelText)) {
      valueLabelData[[i]] <- parseSPSS_VALUELABEL(valueLabelText[i], allVars)
    }

    retList$ValueLabels <- valueLabelData
  }

  outDF <- formatSPSS_Output(retList, outputFormat = outputFormat)
  return(outDF)
}

parseSPSS_RemoveComments <- function(spsTxt) {
  # get the comments, as we want to remove them to make parsing easier
  # comments take many forms, typically they begin with a '*' character, and end in a '.' character.

  rVal <- "|~{?}~|"
  txt <- spsTxt # working copy of text

  # remove any '*' characters that are inside double or single quotes
  dblQ_match <- gregexpr("([\"]).+?([\"])", txt)
  dblQ_txt <- regmatches(txt, dblQ_match)
  dblQ_txt <- lapply(dblQ_txt, function(t) {
    gsub("*", rVal, x = t, fixed = TRUE)
  })
  regmatches(txt, dblQ_match) <- dblQ_txt # apply change back

  sngQ_match <- gregexpr("([\']).+?([\'])", txt)
  sngQ_txt <- regmatches(txt, sngQ_match)
  sngQ_txt <- lapply(sngQ_txt, function(t) {
    gsub("*", rVal, x = t, fixed = TRUE)
  })
  regmatches(txt, sngQ_match) <- sngQ_txt # apply change back

  # regular comments start line with (*) and end in a period, this is most typical
  # also can accept '\*' and ends in '.'
  commentRE1 <- "(^|\\s*|\\/)[*](.|\\n)+?[.]\\s*?(\\n|$)"
  m <- gregexpr(commentRE1, txt, perl = TRUE)
  regmatches(txt, m) <- ""

  # Block comments start with (/*) and end with a terminator (*/)
  commentRE2 <- "(\\/[*])(.|\\n)+?([*]\\/)"
  m <- gregexpr(commentRE2, txt, perl = TRUE, ignore.case = TRUE)
  regmatches(txt, m) <- ""

  # The COMMENT command starts the comment and ends with a period (.)
  commentRE3 <- "(^|\\s)(COMMENT)(.|\\n)+?([.])"
  m <- gregexpr(commentRE3, txt, perl = TRUE, ignore.case = TRUE)
  regmatches(txt, m) <- ""

  txt <- trimws(txt)

  txt <- gsub(rVal, "*", txt, fixed = TRUE)
  return(txt)
}

parseSPSS_FILEHANDLE <- function(spsTxt) {
  txt <- gsub("(FILE HANDLE)", "", spsTxt, ignore.case = TRUE)
  txt <- sub("[.]\\s*\\n$", "", txt)

  hnRE <- regexpr("^\\s*(\\w*){1}", txt) # get first word (alphanumeric and _ characters)
  hnTxt <- regmatches(txt, hnRE)

  handleName <- trimws(gsub("/", "", hnTxt, fixed = TRUE))

  retList <- list(FILEHANDLE = handleName)

  txt <- sub(hnTxt, "", txt)
  subcommands <- c("NAME", "MODE", "RECFORM", "LRECL", "ENCODING")

  for (sc in subcommands) {
    re <- paste0("(", sc, ")", ".*?(\\s|$)")
    reM <- regexpr(re, txt, ignore.case = TRUE)

    if (reM > 0) { # match found for subcommand
      scTxt <- regmatches(txt, reM)
      scTxt <- trimws(scTxt)
      scTxt <- sub(sc, "", scTxt, ignore.case = TRUE) # remove the subcommand text
      scTxt <- sub("=", "", scTxt, fixed = TRUE) # remove the first '=' assignment character

      # is the command value quoted?
      if (grepl("^[\"'].*[\"']$", scTxt)) {
        scTxt <- sub("^[\"']", "", scTxt) # remove opening double or single quote character
        scTxt <- sub("[\"']$", "", scTxt) # remove ending double or single quote character
      }

      retList[sc] <- trimws(scTxt) # add param to the return list
    } # end if(reM > 0)
  } # end for(sc in subcommands)

  return(retList)
}

parseSPSS_DATALIST <- function(spsTxt) {
  # splitting here
  tokens <- spss_CustomSplitter(spsTxt, splitChar = "/")
  cmdToken <- tokens[1]
  dataTokens <- tokens[-1]

  IsMultiLine <- length(tokens) > 2 # TRUE/FALSE

  # remove the 'DATA LIST' command
  cmdToken <- sub("^\\s*DATA LIST", "", cmdToken, ignore.case = TRUE)

  fileM <- regexpr("FILE\\s*[=]\\s*\\w+", cmdToken, ignore.case = TRUE)
  fileTxt <- regmatches(cmdToken, fileM)
  fileTxt <- sub("FILE", "", fileTxt, ignore.case = TRUE)
  fileTxt <- sub("=", "", fileTxt, fixed = TRUE)
  fileHandle <- trimws(fileTxt) # the file handle associated with this data list command
  regmatches(cmdToken, fileM) <- "" # remove our file = xxxx from the argument, leaves remaining commands
  cmdToken <- trimws(cmdToken)

  # FIXED is the DEFAULT format subcommand, so not necessary be in string
  # check if either FREE or LIST subcommands specified as we don't handle them
  badM <- regexpr("\\s*(FREE|LIST)\\s*", cmdToken, ignore.case = TRUE)
  if (badM > 0) {
    stop("Unable to parse a DATA LIST command using FREE or LIST subcommands.")
  }

  recN <- length(dataTokens)

  if (recN > 1) { # split the string by the table records for processing
    for (i in seq_along(dataTokens)) {
      # remove line breaks and the ending '.' terminator
      chunkTxt <- sub("^\\d+", "", dataTokens[i])
      chunkTxt <- gsub("\n", "", chunkTxt)
      chunkTxt <- sub("[.]\\s*?[\n]$", "", chunkTxt)

      tmpItem <- parseSPSS_DATALIST_Items(chunkTxt)
      tmpItem$RecordNumber <- i

      if (i == 1) {
        items <- tmpItem
      } else {
        items <- base::rbind(items, tmpItem)
      }
    }
  } else { # single record only.  may or may not have record number as part of the listing

    # remove line breaks and the ending '.' terminator
    dataTokens <- gsub("\n", "", dataTokens)
    dataTokens <- sub("[.]\\s*?[\n]$", "", dataTokens)

    items <- parseSPSS_DATALIST_Items(dataTokens)
  }

  # define output object
  outputList <- list(
    FileHandle = fileHandle,
    RecordNumber = recN,
    DataList = items
  )

  # ensure all names are valid and not duplicated
  items$VariableName <- make.names(items$VariableName)

  return(outputList)
}

parseSPSS_DATALIST_Items <- function(txt) {
  # tokens begin with a word, then digits for starting position
  # usually it has a '-' character and another digit to signify ending position, but not always
  # lastly, there may be additional attributes enclosed in parenthesis to signify alphanumeric (A)
  # or numeric formatting precision (6)
  tokensM <- gregexpr("(\\w|[_])+\\s*\\d+(\\s*[-]\\s*\\d+){0,1}\\s*([(]\\w+[)]){0,1}", txt)
  tokens <- regmatches(txt, tokensM)[[1]]

  # varname is the first word of the token
  varsM <- regexpr("(\\w|[_])+", tokens)
  vars <- trimws(regmatches(tokens, varsM))
  regmatches(tokens, varsM) <- "" # remove the variables now that we have them isolated

  # attributes are enclosed in parens e.g., (A), (6)
  attribM <- regexpr("([(]\\w+[)])", tokens)
  attrib <- regmatches(tokens, attribM)
  regmatches(tokens, attribM) <- "" # remove the attributes from tokens

  # cleanup the attributes as not all variables have them defined
  attribClean <- rep("", times = length(vars))
  attribClean[attribM > 0] <- trimws(attrib) # maintains ordering

  startM <- regexpr("\\d+", tokens)
  startPos <- as.numeric(regmatches(tokens, startM))
  regmatches(tokens, startM) <- ""

  tokens <- sub("-", "", tokens, fixed = TRUE) # remove the '-' from the string now to isolate the end position
  endPos <- as.numeric(trimws(tokens))
  endPos[is.na(endPos)] <- startPos[is.na(endPos)] # if there is no ending position defined, use the start position (length of 1)

  width <- (endPos - startPos) + 1

  # build output object
  df <- data.frame(
    VariableName = vars,
    StartPosition = startPos,
    EndPosition = endPos,
    Width = width,
    Attributes = attribClean
  )
  # fix FWF spacing to ensure there are no gaps in the definition
  df <- parseSPSS_fixSpacingFWF(df[order(startPos), ])

  return(df)
}

# the spsTxt is the spss script block to be parsed.
# the varNames are the variable names defined that will be used for matching
parseSPSS_VARIABLELABEL <- function(spsTxt, varNames, verbose = FALSE) {
  # prep: remove the command text from the string
  spsTxt <- sub("^\\s*VARIABLE LABEL(S){0,1}", "", spsTxt, ignore.case = TRUE)
  spsTxt <- sub("[.]\\s*(\n|$)", "", spsTxt)

  retDF <- data.frame(sortOrder = seq_along(varNames), varNames = tolower(varNames), stringsAsFactors = FALSE)

  # SCRIPT EXAMPLE
  # OLDSAL "EMPLOYEE'S GROSS SALARY PRIOR"
  # + " TO 1988"
  # + "blah blah blah"
  # country	" Country three-digit ISO code"
  # schoolid	" School ID (unique)"
  # stidstd	" Student ID"
  # subnatio	" Language community ID"
  tokensM <- gregexpr("(\\w|[+])+\\s*((\"(?:\\\\.|[^\"\\\\])*?\")|(\'(?:\'.|[^\'])*?\'))(\\s|\\n|$)", spsTxt, perl = TRUE)
  tokens <- trimws(unlist(regmatches(spsTxt, tokensM)))

  # grab the varname
  wordValM <- regexpr("(\\w|_|[+])+\\s*", tokens)
  wordVal <- trimws(unlist(regmatches(tokens, wordValM)))

  appendTokens <- c() # zero-length vector here if no appending necessary

  if (any(substr(wordVal, 1, 1) == "+")) {
    idx <- which(wordVal == "+", arr.ind = TRUE)

    appendTokens <- tokens[idx]
    appendWord <- rep("", times = length(appendTokens))
    appendIdx <- rep(0, times = length(appendTokens))

    for (i in rev(seq_along(idx))) {
      ii <- idx[i] - 1

      while (wordVal[ii] == "+") {
        ii <- ii - 1
      }
      appendWord[i] <- wordVal[ii]
      appendIdx[i] <- ii
    }

    # cleanup the appending tokens
    # remove the '+'/append character from the token text
    appendTokens <- trimws(sub("+", "", appendTokens, fixed = TRUE))
    appendTokens <- substr(appendTokens, 2, nchar(appendTokens) - 1)

    # apply the appending tokens
    for (i in seq_along(appendIdx)) {
      iT <- tokens[appendIdx[i]]
      aT <- appendTokens[i]

      # create the new token string with the appended text
      iT <- paste0(substr(iT, 1, nchar(iT) - 1), aT, substr(iT, nchar(iT), nchar(iT)))

      tokens[appendIdx[i]] <- iT # apply the new label back to the variable
    }

    # remove the appending tokens from the words and tokens
    tokens <- tokens[-idx]
    wordVal <- wordVal[-idx]

    # the match needs to then be recalculated as it can't be simply dropped for whatever reason
    wordValM <- regexpr("(\\w|_|[+])+\\s*", tokens)
  }

  # clear out the word from the token
  regmatches(tokens, wordValM) <- ""
  tokens <- trimws(tokens)

  # strip off the beginning/ending quote charaters (either double or single quotes)
  tokens <- substr(tokens, 2, nchar(tokens) - 1)

  # cleanup any text escapes
  tokens <- gsub("''", "'", tokens, fixed = TRUE) # a ('') is escaped to just a single quote (')
  tokens <- gsub("\\\"", "\"", tokens, fixed = TRUE) # a (\") is escaped to just a double quote (")
  tokens <- gsub("\\n", "", tokens, fixed = TRUE) # a (\n) should be removed from the token, we don't want the wrapping

  # create a data.frame to help merging later
  tokenDF <- data.frame(varNames = tolower(wordVal), varLabel = trimws(tokens), stringsAsFactors = FALSE)

  # check for duplicates in vars and reconcile them
  if (anyDuplicated(tokenDF$varNames)) {
    idx <- which(duplicated(tokenDF$varNames), arr.ind = TRUE)
    dupeVars <- unique(tokenDF$varNames[idx])

    warning(paste0(
      "Duplicate Variable Labels Defined For Variable(s): ", paste(sQuote(dupeVars), collapse = ","),
      "\n", "The last defined variable label will be used."
    ))

    for (dv in dupeVars) {
      rowIdx <- which(tokenDF$varNames == dv, arr.ind = TRUE)
      rowIdx <- rowIdx[rowIdx != max(rowIdx)]

      tokenDF <- tokenDF[-rowIdx, ]
    }
  }

  retDF <- merge(retDF, tokenDF, all.x = TRUE, all.y = FALSE, by = "varNames")
  retDF <- retDF[order(retDF$sortOrder), ] # re-order based on the original sort order
  retDF$sortOrder <- NULL # drop the sorting

  row.names(retDF) <- 1:nrow(retDF) # renumber the row.names
  return(retDF)
}

parseSPSS_VALUELABEL <- function(spsTxt, varNames) {
  retDF <- data.frame(
    variableName = tolower(varNames),
    valType = rep("", times = length(varNames)),
    valLblDesc = rep("", times = length(varNames))
  )

  spsTxt <- sub("^\\s*VALUE LABEL(S){0,1}", "", spsTxt, ignore.case = TRUE)

  tokens <- spss_CustomSplitter(spsTxt, splitChar = "/")
  tokens <- trimws(tokens)
  tokens <- tokens[nchar(tokens) > 0] # drop any blank tokens

  # define the regular expression for grabbing both the quoted value, and the quoted value label
  quotedDef_Regex <- "([\'\"]).+?([\'\"])\\s+[\'\"].+[\'\"](\\s|\\n|$)(\\s*[+]\\s*[\'\"].+[\'\"](\\s|\\n|$)){0,5}"

  # get the index to the quoted assignment definition, if not quoted, then it's numeric
  quotedTokenIdx <- which(grepl(quotedDef_Regex, tokens, ignore.case = TRUE, perl = TRUE), arr.ind = TRUE)
  quotedTokens <- tokens[quotedTokenIdx]
  # numeric tokens will have a number assignment that's not within quotes for a label value
  if (length(quotedTokenIdx) == 0) {
    numericTokens <- tokens
  } else {
    numericTokens <- tokens[-quotedTokenIdx]
  }

  # parse the quoted tokens first
  if (length(quotedTokens) > 0) {
    quotedM <- gregexpr(quotedDef_Regex, quotedTokens, ignore.case = TRUE, perl = TRUE)
    tokenTemp <- regmatches(quotedTokens, quotedM) # this is now the quoted value along with the quoted label

    regmatches(quotedTokens, quotedM) <- "" # remove the assignment from the token::leaves only the variables left
    quotedTokens <- trimws(quotedTokens)
    quotedM <- gregexpr("[A-z]\\w*", quotedTokens, ignore.case = TRUE, perl = TRUE)
    qVars <- regmatches(quotedTokens, quotedM) # list of jagged vectors containing the associated variable names

    # check if any of the variables has the 'TO' keyword
    for (i in seq_along(tokenTemp)) {
      z <- tokenTemp[[i]]

      vars <- tolower(qVars[[i]])
      m <- gregexpr("^[\'\"].*?[\'\"]", z, ignore.case = TRUE, perl = TRUE)
      valLbl <- regmatches(z, m)
      valLbl <- substr(valLbl, 2, nchar(valLbl) - 1)
      regmatches(z, m) <- ""
      z <- trimws(z)

      lblLbl <- substr(z, 2, nchar(z) - 1)
      lblLbl <- gsub("[\'\"]\\s*[+]\\s*[\'\"]", "", lblLbl) # for any line concatenations we want to remove and leave full string

      # ensure our collapse character isn't present in the text!
      lblLbl <- gsub("^", "", lblLbl, fixed = TRUE)

      lblFormatted <- paste(valLbl, lblLbl, sep = "=", collapse = "^")

      # Check if 'TO' keyword is used.  vars will be a length of 3
      # e.g., "V1 to V3" OR Var1 TO Var10
      if (any(grepl("^to$", vars[2], ignore.case = TRUE)) && length(vars) == 3) {
        p1 <- which(grepl(paste0("^", vars[1], "$"), retDF$variableName, ignore.case = TRUE), arr.ind = TRUE)
        p2 <- which(grepl(paste0("^", vars[3], "$"), retDF$variableName, ignore.case = TRUE), arr.ind = TRUE)
        vars <- retDF$variableName[p1:p2]
      }

      retDF[retDF$variableName %in% vars, "valType"] <- "character"
      retDF[retDF$variableName %in% vars, "valLblDesc"] <- lblFormatted
    }
  } # end if (length(quotedTokens) > 0)

  # then process the numeric tokens
  # allows for up to 5 concatenations to the label!!
  if (length(numericTokens) > 0) {
    numericDef_Regex <- "\\s+(\\d*|[.]\\d*)\\s*([-]){0,1}\\s*(\\d+|[.]\\d+)\\s+[\'\"].+[\'\"](\\s|\\n|$)(\\s*[+]\\s*[\'\"].+[\'\"](\\s|\\n|$)){0,5}"

    numericM <- gregexpr(numericDef_Regex, numericTokens, ignore.case = TRUE, perl = TRUE)
    tokenTemp <- regmatches(numericTokens, numericM)

    regmatches(numericTokens, numericM) <- "" # remove the assignment from the token::leaves only the variables left
    numericTokens <- trimws(numericTokens)
    quotedM <- gregexpr("[A-z]\\w*", numericTokens, ignore.case = TRUE, perl = TRUE)
    qVars <- regmatches(numericTokens, quotedM) # list of jagged vectors containing the associated variable names

    # check if any of the variables has the 'TO' keyword
    for (i in seq_along(tokenTemp)) {
      z <- tokenTemp[[i]]
      z <- trimws(z) # matches generally have a leading space here due to the regex specification!

      vars <- tolower(qVars[[i]])
      m <- gregexpr("^(\\d*|[.]\\d*)\\s*([-]){0,1}\\s*(\\d+|[.]\\d+)", z, ignore.case = TRUE, perl = TRUE)
      valLbl <- trimws(regmatches(z, m)) # trimws unlists here!
      regmatches(z, m) <- ""
      z <- trimws(z)

      lblLbl <- substr(z, 2, nchar(z) - 1)
      lblLbl <- gsub("[\'\"]\\s*[+]\\s*[\'\"]", "", lblLbl) # for any line concatenations we want to remove and leave full string

      # ensure our collapse character isn't present in the text!
      lblLbl <- gsub("^", "", lblLbl, fixed = TRUE)

      # check for any numeric ranges, exclude them if found (e.g. 0 - 100; 1-4)
      include <- sapply(valLbl, function(x) {
        !grepl("\\d+\\s*[-]\\s*\\d+", x, ignore.case = TRUE)
      })

      # ranges are excluded here
      lblFormatted <- paste(valLbl[include], lblLbl[include], sep = "=", collapse = "^")

      # Check if 'TO' keyword is used.  vars will be a length of 3
      # e.g., "V1 to V3" OR Var1 TO Var10
      if (any(grepl("^to$", vars[2], ignore.case = TRUE)) && length(vars) == 3) {
        p1 <- which(grepl(paste0("^", vars[1], "$"), retDF$variableName, ignore.case = TRUE), arr.ind = TRUE)
        p2 <- which(grepl(paste0("^", vars[3], "$"), retDF$variableName, ignore.case = TRUE), arr.ind = TRUE)
        vars <- retDF$variableName[p1:p2]
      }

      retDF[retDF$variableName %in% vars, "valType"] <- "numeric"
      retDF[retDF$variableName %in% vars, "valLblDesc"] <- lblFormatted
    }
  } # end if (length(numericTokens) > 0)

  return(retDF)
}

# custom splitting on the '/' character, escapes text within double and single quotes
# also checks for escape tokens within the quoted text if double or single quotes are used
# the nextCharRegEx is a regular expression that will check the n+1 character of the splitChar specified.
# this is useful if the splitChar needs additional validation
spss_CustomSplitter <- function(inTxt, splitChar = "/", nextCharRegEx = NULL) {
  tokenStart <- 1
  allTokens <- c()
  inDblQuotes <- FALSE
  inSglQuotes <- FALSE
  prevX <- ""
  checkNextChar <- FALSE

  if (is.character(nextCharRegEx)) {
    checkNextChar <- TRUE
  } else {
    nextCharTestVal <- TRUE # default to always true if no regex supplied
  }
  charArr <- strsplit(inTxt, "")[[1]]
  endPos <- length(charArr)

  checkChars <- which(charArr %in% c(splitChar, "\"", "'"), arr.ind = TRUE)

  for (i in seq_along(checkChars)) {
    x <- charArr[checkChars[i]]

    if (checkNextChar) {
      nextX <- ifelse(i == endPos, "\n", charArr[checkChars[i] + 1])
      nextCharTestVal <- grepl(nextCharRegEx, nextX, ignore.case = TRUE)
    }

    if (x == "\"" && inSglQuotes == FALSE && charArr[checkChars[i] - 1] != "\\") {
      inDblQuotes <- !inDblQuotes
    }
    if (x == "'" && inDblQuotes == FALSE && charArr[checkChars[i] - 1] != "'") {
      inSglQuotes <- !inSglQuotes
    }
    if (x == splitChar && !inDblQuotes && !inSglQuotes && nextCharTestVal) {
      allTokens <- c(allTokens, substr(inTxt, tokenStart, checkChars[i] - 1))
      tokenStart <- checkChars[i] + 1
    }
    if (i == length(checkChars) && !inDblQuotes && !inSglQuotes) { # end of the text, be sure to add it
      allTokens <- c(allTokens, substr(inTxt, tokenStart, endPos))
      tokenStart <- checkChars[i] + 1
    }
  }

  return(allTokens)
}

# parse the missing values command from the script to know which values are considered 'missings' for analysis
parseSPSS_MISSINGVALUES <- function(spsTxt, varNames) {
  spsTxt <- sub("^\\s*MISSING VALUE(S){0,1}", "", spsTxt, ignore.case = TRUE)

  retDF <- data.frame(
    variableName = varNames,
    missingValues = rep("", times = length(varNames)),
    valueType = rep("", times = length(varNames)), stringsAsFactors = FALSE
  )

  tokens <- spss_CustomSplitter(spsTxt, splitChar = "/")
  tokens <- trimws(tokens)
  tokens <- gsub("\n", "", tokens, fixed = TRUE)
  tokens <- paste0(tokens, collapse = "\n")

  m <- gregexpr("(\\w+\\s*){1,}[(](\\s|\\w|[ ,]|[\"'])+[)]", tokens)
  valDef <- regmatches(tokens, m)
  valDef <- trimws(valDef[[1]]) # from list to vector

  for (i in seq_along(valDef)) {
    iVals <- valDef[[i]]
    m <- gregexpr("[(].*[)]", iVals) # get the part in the parenthesis

    missVals <- regmatches(iVals, m)[[1]] # will only have one value
    assignVars <- trimws(gsub(missVals, "", iVals, fixed = TRUE))
    missVals <- gsub("([(]|[)])", "", missVals)

    isQuoted <- grepl("[\"']", missVals)
    missVals <- gsub("(\"|')", "", missVals) # remove the quotes from the string

    # change the value separator from a ',' to ';' character
    missVals <- gsub(",", ";", missVals, fixed = TRUE)

    # split out all tokens of the variables
    m <- gregexpr("\\w+", assignVars)
    assignVars <- trimws(regmatches(assignVars, m)[[1]])

    idxTO <- which(grepl("^to$", assignVars, ignore.case = TRUE), arr.ind = TRUE) # need to expand the variables that have the 'TO' range keyword

    if (length(idxTO) > 0) {
      appendVars <- c()
      # expand the variables to include the variables defined between the vars
      for (ii in seq_along(idxTO)) {
        idxVal <- idxTO[ii]
        aVar <- assignVars[idxVal - 1]
        bVar <- assignVars[idxVal + 1]

        aVarRow <- which(grepl(paste0("^", aVar, "$"), retDF$variableName, ignore.case = TRUE))
        bVarRow <- which(grepl(paste0("^", bVar, "$"), retDF$variableName, ignore.case = TRUE))

        appendVars <- c(appendVars, retDF$variableName[aVarRow:bVarRow])
      }

      assignVars <- assignVars[-idxTO] # drop the 'TO' variables
      assignVars <- c(assignVars, appendVars) # add the 'range' variables
    }

    retDF$missingValues[tolower(retDF$variableName) %in% tolower(assignVars)] <- missVals
    retDF$valueType[tolower(retDF$variableName) %in% tolower(assignVars)] <- ifelse(isQuoted, "character", "numeric")
  }

  return(retDF)
}

# split the script up into its command parts by the ending '.', then clean up the text a bit
parseSPSS_COMMANDS <- function(spsTxt) {
  items <- spss_CustomSplitter(spsTxt, splitChar = ".", nextCharRegEx = "(\\s|\\n)")
  items <- trimws(items) # trim any spacing before or after
  items <- items[nchar(items) > 0]

  # drop any items beginning with a '*', or uses the COMMENT command, as those are comments
  items <- items[!grepl("^([*]|COMMENT)", items, ignore.case = TRUE)]

  return(items)
}

# create a formatted output object
formatSPSS_Output <- function(resList, outputFormat) {
  if (outputFormat == "data.frame") {
    return(formatSPSS_Output_dataframe(resList))
  }
}

formatSPSS_Output_dataframe <- function(resList) {
  df <- resList$DataList[[1]]$DataList # at this time only one 'DATA LIST' command is supported, use as the basis for the data.frame

  # rename to match expected column names for EdSurvey
  names(df)[grepl("variablename", names(df), ignore.case = TRUE)] <- "variableName"
  names(df)[grepl("startposition", names(df), ignore.case = TRUE)] <- "Start"
  names(df)[grepl("endposition", names(df), ignore.case = TRUE)] <- "End"
  names(df)[grepl("width", names(df), ignore.case = TRUE)] <- "Width"
  names(df)[grepl("attributes", names(df), ignore.case = TRUE)] <- "Attributes"

  df$Labels <- rep("", times = nrow(df))
  for (x in resList$VariableLabels) {
    m <- match(tolower(x$varNames), tolower(df$variableName))
    df$Labels[m] <- x$varLabel[m]
  }

  df$labelValues <- rep("", times = nrow(df))
  for (x in resList$ValueLabels) {
    m <- match(tolower(x$variableName), tolower(df$variableName))
    df$labelValues[m] <- x$valLblDesc[m]
    df$dataType[m] <- x$valType[m]
  }


  for (x in resList$MissingValues) {
    m <- match(tolower(x$variableName), tolower(df$variableName))
    df$missingValues[m] <- x$missingValues[m]
  }

  df <- parseSPSS_validateDataType(df)
  return(df)
}

# validates the data.frame's 'dataType' column to ensure it has a value, and is consistent with the attributes from the data list
parseSPSS_validateDataType <- function(df) {
  # if no dataType defined and has a numeric format attribute consider it numeric
  idx <- which(nchar(df$dataType) == 0 & grepl("\\d", df$Attributes, ignore.case = TRUE), arr.ind = TRUE)
  df[idx, "dataType"] <- "numeric"

  # otherwise if no data type, it will be classified as character
  idx <- which(nchar(df$dataType) == 0, arr.ind = TRUE)
  df[idx, "dataType"] <- "character"

  return(df)
}

# takes a built data.frame from the parseSPSS_DATALIST_Items function and ensures there are no gaps in the FWF spacing
# if gaps exist then it fills them with filler columns for proper spacing
parseSPSS_fixSpacingFWF <- function(df) {
  # get the expected widths based on the end positions, returns a vector
  df$TestWidth <- c(df$End[1], df$End[-1] - df$End[-nrow(df)])

  # determine what numbers are missing from our FWF sequence
  issues <- (1:nrow(df))[df$Width != df$TestWidth]

  # shortcut out if no problems
  if (length(issues) == 0) {
    df$TestWidth <- NULL # remove temporary column
    return(df)
  }

  # reverse issue order so that order stay correct
  for (issue in rev(issues)) {
    # issue is a row index with a problem
    # make a new row
    newrow <- df[issue, ]
    # set the width to fill the gap
    newrow$Width <- newrow$TestWidth - newrow$Width # fill the gap

    newrow$VariableName <- "zzz_Filler"
    newrow$Attributes <- "(A)" # make sure it can be read in

    df[issue:nrow(df) + 1, ] <- df[issue:nrow(df), ]
    df[issue, ] <- newrow
  }

  # recalibrate the start/end positions after placing in the filler columns
  df$StartPosition <- c(1, 1 + cumsum(df$Width))[1:nrow(df)]
  df$EndPosition <- cumsum(df$Width)

  df$TestWidth <- NULL # remove out temporary column

  # ensure unique names
  df$VariableName <- make.names(df$VariableName, unique = TRUE)

  return(df)
}
