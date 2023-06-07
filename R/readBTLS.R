#' @title Connect to BTLS Data
#'
#' @description Opens a connection to the Beginning Teacher Longitudinal Study (BTLS) waves 1 through 5 data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param dat_FilePath a character value to the full path of the BTLS fixed-width (.dat) data file
#' @param spss_FilePath a character value to the full path of the SPSS syntax file to process the \code{dat_FilePath}
#' @param verbose a logical value that will determine if you want verbose output while the \code{readBTLS} function is running to indicate processing progress
#'                (the default value is \code{TRUE})
#'
#' @details Reads the \code{spss_FilePath} file to parse the \code{dat_FilePath} to an \code{edsurvey.data.frame}.
#'          There is no cached data because the \code{dat_FilePath} format already is in fixed-width format.
#'
#' @return
#'  an \code{edsurvey.data.frame} for the BTLS waves 1 to 5 longitudinal dataset.
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readBTLS.R
#' @export
readBTLS <- function(dat_FilePath,
                     spss_FilePath,
                     verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  dat_FilePath <- suppressWarnings(normalizePath(unique(dat_FilePath), winslash = "/"))
  spss_FilePath <- suppressWarnings(normalizePath(unique(spss_FilePath), winslash = "/"))

  if (!file.exists(dat_FilePath)) {
    stop(paste0("Cannot find specified data file ", sQuote("dat_FilePath"), " in path ", sQuote(file.path(dat_FilePath)), "."))
  }
  if (!file.exists(spss_FilePath)) {
    stop(paste0("Cannot find specified data file ", sQuote("spss_FilePath"), " in path ", sQuote(file.path(spss_FilePath)), "."))
  }

  if (verbose) {
    cat(paste0("Parsing SPSS syntax file.\n"))
  }
  fileFormat <- parseSPSSFileFormat(spss_FilePath) # the returned fileformat only has character or numeric types.  need to analyze each data column to determine integer/numerics as it's important for analsis

  lafObj <- laf_open_fwf(dat_FilePath, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)

  for (coli in 1:nrow(fileFormat)) {
    if (verbose && ((coli %% 500) == 0) || coli == 1 || coli == nrow(fileFormat)) {
      cat(paste0("Processing column ", coli, " of ", ncol(lafObj), "\n"))
    }
    # ensure we have no out of bounds values per the file format specification::this also converts any scientific notation numbers to their full character count
    colData <- lafObj[, coli]
    colData <- colData[, 1, drop = TRUE]

    # test the type and precision is what it should be
    if (is.numeric(colData)) {
      if (any(grepl(".", colData, fixed = TRUE), na.rm = TRUE)) {
        decTestLen <- num.decimals(colData) # get the decimal precision
        decTestLen[is.na(decTestLen)] <- 0 # convert NAs to 0 for testing

        fileFormat$dataType[coli] <- "numeric"
        fileFormat$Decimal[coli] <- max(decTestLen)
      } else { # treat it as integer
        if (fileFormat$Width[coli] < 9) {
          fileFormat$dataType[coli] <- "integer"
          fileFormat$Decimal[coli] <- 0
        }
      }
    }
  }

  # close and reopen with the updated dataTypes to ensure those stick to the LaF object moving forward
  close(lafObj)
  lafObj <- laf_open_fwf(dat_FilePath, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)

  fileFormat <- identifyBTLSWeights(fileFormat)
  weights <- buildBTLSWeightList(fileFormat)

  if (!is.null(weights)) {
    attributes(weights)$default <- "" # no default weight
  }

  pvs <- list() # no plausible values or achievement levels

  omittedLevels <- c(
    "Deceased", "Nonrespondent", "suppressed due to limited number of respondents", "Respondent, valid skip",
    "Valid Skip", "Valid skip", "Missing", "Respondent, missing data",
    "(Missing)", NA
  )

  edsurvey.data.frame(
    userConditions = list(),
    defaultConditions = NULL,
    dataList = buildBTLSDataList(lafObj, fileFormat),
    weights = weights,
    pvvars = pvs,
    subject = "",
    year = "2007-2012",
    assessmentCode = "Longitudinal",
    dataType = "Longitudinal Data",
    gradeLevel = "",
    achievementLevels = NULL, # no achievement levels
    omittedLevels = omittedLevels,
    survey = "BTLS",
    country = "USA",
    psuVar = NULL, # No PSU or Stratum for Taylor in Data
    stratumVar = NULL,
    jkSumMultiplier = 0.0113636363636364, # 1/88 replicates for BRR replication
    validateFactorLabels = TRUE, # the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
    reqDecimalConversion = FALSE
  ) # decimal conversion is not needed
}


# identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBTLSWeights <- function(fileFormat) {
  varNames <- fileFormat$variableName

  # identify weight variables
  wgtVars <- grep("^(w1|w2|w3|w4|w5).*(wt|wgt)$", fileFormat$variableName, ignore.case = TRUE, value = TRUE)

  # TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars

  return(fileFormat)
}

# prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBTLSWeightList <- function(fileFormat) {
  wgtVars <- fileFormat[fileFormat$weights == TRUE, "variableName"]

  # no wgts found
  if (length(wgtVars) == 0) {
    return(NULL)
  }

  weights <- list()
  wgtLookupDF <- data.frame(
    wgt = wgtVars,
    repVar = rep("", times = length(wgtVars)),
    stringsAsFactors = FALSE
  )

  # make repVar assignments here as there is no standard convention
  wgtLookupDF[wgtLookupDF$wgt == "w1tfnlwgt", "repVar"] <- "w1trepwt"
  wgtLookupDF[wgtLookupDF$wgt == "w2afwt", "repVar"] <- "w2arwt"
  wgtLookupDF[wgtLookupDF$wgt == "w2rafwt", "repVar"] <- "w2rarwt"
  wgtLookupDF[wgtLookupDF$wgt == "w3afwt", "repVar"] <- "w3arwt"
  wgtLookupDF[wgtLookupDF$wgt == "w3lwgt", "repVar"] <- "w3lrwgt"
  wgtLookupDF[wgtLookupDF$wgt == "w3rafwt", "repVar"] <- "w3rarwt"
  wgtLookupDF[wgtLookupDF$wgt == "w3rlwgt", "repVar"] <- "w3rlrwgt"
  wgtLookupDF[wgtLookupDF$wgt == "w4afwt", "repVar"] <- "w4arwt"
  wgtLookupDF[wgtLookupDF$wgt == "w4lwgt", "repVar"] <- "w4lrwgt"
  wgtLookupDF[wgtLookupDF$wgt == "w4rlwgt", "repVar"] <- "w4rlrwgt"
  wgtLookupDF[wgtLookupDF$wgt == "w4rafwt", "repVar"] <- "w4rarwt"
  wgtLookupDF[wgtLookupDF$wgt == "w5afwt", "repVar"] <- "w5arwt"
  wgtLookupDF[wgtLookupDF$wgt == "w5lwgt", "repVar"] <- "w5lrwgt"
  wgtLookupDF[wgtLookupDF$wgt == "w5rlwgt", "repVar"] <- "w5rlrwgt"

  for (i in 1:length(wgtVars)) {
    tempVar <- wgtVars[i] # full variable name of the weight var

    repVar <- wgtLookupDF$repVar[wgtLookupDF$wgt == tempVar]

    wgtPattern <- paste0("^", repVar, "\\d+$")
    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- sub(repVar, "", ujkz, ignore.case = TRUE) # strip away and leave just the numeric variable name ending as a string

    if (length(ujkz) > 0) {
      tmpWgt <- list()

      tmpWgt[[1]] <- list(jkbase = repVar, jksuffixes = as.character(ujkz))

      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights, tmpWgt)
    }
  }

  return(weights)
}

buildBTLSDataList <- function(lafObj, fileFormat) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(
    lafObject = lafObj,
    fileFormat = fileFormat,
    levelLabel = "Data",
    forceMerge = TRUE,
    parentMergeLevels = NULL,
    parentMergeVars = NULL,
    mergeVars = NULL,
    ignoreVars = NULL,
    isDimLevel = TRUE
  )

  return(dataList)
}
