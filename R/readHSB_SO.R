#' @title Connect to HS&B Study Sophomore Data
#'
#' @description Opens a connection to a High School & Beyond 1980--1992 Sophomore cohort data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param HSO8092_PRI_FilePath a character value to the main study-derived
#'                             analytical data file (HSO8092_REV.PRI).
#'                             Located within the \code{REVISED_ASCII} folder.
#' @param HSO8092_SASSyntax_Path a character value to the SAS syntax file for
#'                               parsing the \code{HSO8092_REV.PRI} data file.
#'                               Located within the \code{SAS_EXTRACT_LOGIC} folder.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read
#'                    function by using existing read-in data already processed.
#' @param verbose a logical value that will determine if you want verbose
#'                output while the \code{readHSB_Sophomore} function is running
#'                to indicate processing progress.
#'                The default value is \code{TRUE}.
#'
#' @details
#' Reads in the specified \code{HSO8092_SASSyntax_Path} file to parse
#' the \code{HSO8092_PRI_FilePath} file.
#' A cached data file and metadata file will be saved in the same
#' directory and filename as the \code{HSO8092_PRI_FilePath} file,
#' having new file extensions of .txt and .meta, respectively.
#'
#' Please note the original source \code{repcode} variable has been split
#' into two variables named \code{repcode_str} for the stratum value
#' and \code{repcode_psu} for the primary sampling unit (PSU) value in the resulting cache data.
#'
#' @return
#' an \code{edsurvey.data.frame} for the HS&B Sophomore 1980--1992 longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readHSB_SO.R
#'
readHSB_Sophomore <- function(HSO8092_PRI_FilePath,
                              HSO8092_SASSyntax_Path,
                              forceReread = FALSE,
                              verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  HSO8092_PRI_FilePath <- suppressWarnings(normalizePath(unique(HSO8092_PRI_FilePath), winslash = "/"))
  HSO8092_SASSyntax_Path <- suppressWarnings(normalizePath(unique(HSO8092_SASSyntax_Path), winslash = "/"))

  if (!file.exists(HSO8092_PRI_FilePath)) {
    stop(paste0("Cannot find specified data file ", sQuote("HSO8092_PRI_FilePath"), " in path ", sQuote(file.path(HSO8092_PRI_FilePath)), "."))
  }
  if (!file.exists(HSO8092_SASSyntax_Path)) {
    stop(paste0("Cannot find specified data file ", sQuote("HSO8092_SASSyntax_Path"), " in path ", sQuote(file.path(HSO8092_SASSyntax_Path)), "."))
  }

  # prep the data cache items
  cacheFilename <- gsub("\\.pri$", ".txt", HSO8092_PRI_FilePath, ignore.case = TRUE)
  metaFilename <- gsub("\\.pri$", ".meta", HSO8092_PRI_FilePath, ignore.case = TRUE)

  runProcessing <- TRUE # set default value
  # check and validate any cached files to see if they should be used
  if (file.exists(cacheFilename)) {
    if (file.exists(metaFilename)) {
      cacheRDS <- readRDS(metaFilename) # get the cache info from the .meta file

      if (!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "HS&B")) {
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }

  # define omittedLevels for the clean step below
  omittedLevels <- c(
    "{DON'T KNOW}", "{DONT'T KNOW}",
    "{LEGITIMATE SKIP}", "{MISSING, LEGIT SKIP}",
    "{MISSING}", "{MULTIPLE PUNCH}", "{OUT OF RANGE}",
    "{REFUAL}", "{REFUSAL}", "{UNC VERBATIM}",
    "{UNC. VERBATIM}", "{UNCODABLE VERBATIM}",
    "{INSTRUMENT NONRESP}", "{MULTIPLE RESPONSE}",
    "NOT IN SUBGROUP", "{Missing}", "MISSING DATE",
    "NO BA EARNED",
    "(Missing)", NA
  )

  # force reprocess if called for
  if (forceReread == TRUE) {
    runProcessing <- TRUE
  }

  if (runProcessing == TRUE) {
    if (verbose) {
      cat(paste0("Processing SAS syntax file.\n"))
    }

    fileFormat <- parseSAS_FileFormat_HSB(HSO8092_SASSyntax_Path) # get the file format from the master.txt file

    # special fixes for the fileFormat label values due to inconsistancies with the SAS script formattings
    fileFormat$labelValues[fileFormat$variableName == "decflag2"] <- "0=NOT DECEASED^1=DECD IN 1ST FOLLOWUP^2=DECD IN 2ND FOLLOWUP^3=DECD IN 3RD FOLLOWUP^4=DECD IN 4TH FOLLOWUP"
    fileFormat$labelValues[fileFormat$variableName == "pfaminc2"] <- "1=Less than 8,000^2=8,000-14,999^3=15,000-19,999^4=20,000-24,999^5=25,000-29,999^6=30,000-39,999^7=40,000-49,999^8=50,000-more^-1={Missing}"

    # the REPCODE field contains both the stratum and PSU in one field, redefine the fileformat to split it into two vars for analysis
    rowIdx <- which(fileFormat$variableName == "repcode", arr.ind = TRUE)

    tempFF <- fileFormat[1:(rowIdx - 1), ]
    repcodeFF <- fileFormat[rowIdx, ]
    tempFF2 <- fileFormat[(rowIdx + 1):nrow(fileFormat), ]
    repcodeFF2 <- repcodeFF # duplicate copy of the repCodeFF

    repcodeFF$variableName <- "repcode_str"
    repcodeFF$End <- repcodeFF$End - 1
    repcodeFF$Width <- repcodeFF$Width - 1
    repcodeFF$Labels <- "REPLICATED SAMPLE CODE - stratum"

    repcodeFF2$variableName <- "repcode_psu"
    repcodeFF2$Start <- repcodeFF2$Start + 2
    repcodeFF2$Width <- repcodeFF2$Width - 2
    repcodeFF2$Labels <- "REPLICATED SAMPLE CODE - psu"

    # rebuild the parts after the repcode variable was dissected into it's parts
    fileFormat <- rbind.data.frame(tempFF, repcodeFF, repcodeFF2, tempFF2, stringsAsFactors = FALSE)
    rownames(fileFormat) <- 1:nrow(fileFormat)

    # recalibrate the start/end positions so they are correct
    fileFormat$Start <- c(1, 1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)]
    fileFormat$End <- cumsum(fileFormat$Width)

    lafObj <- laf_open_fwf(HSO8092_PRI_FilePath, fileFormat$dataType, fileFormat$Width, fileFormat$variableName)

    fileFormat <- identifyHSB_SOWeights(fileFormat)
    fileFormat <- valueLabelCleanupFF(fileFormat, omittedLevels, c("{$ ZERO}", "{NON-PARTICIPANT}"))
    fileFormat <- writeCacheWithRepWgt_HSB(lafObj, fileFormat, "repcode_str", "repcode_psu", cacheFilename, verbose)

    # write cache file and .meta
    cacheFile <- list(
      ver = ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
      cacheFileVer = 1,
      ts = Sys.time(),
      fileFormat = fileFormat
    )

    saveRDS(cacheFile, metaFilename)
  } # end if(runProcessing==TRUE)

  lafObj <- laf_open_fwf(cacheFilename, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)

  weights <- buildHSB_SOWeightList(fileFormat)
  if (!is.null(weights)) {
    attributes(weights)$default <- "" # set default weight
  }

  pvs <- list() # no plausible values or achievement levels

  edsurvey.data.frame(
    userConditions = list(),
    defaultConditions = NULL,
    dataList = buildHSB_SODataList(lafObj, fileFormat),
    weights = weights,
    pvvars = pvs,
    subject = "",
    year = "1980-1992",
    assessmentCode = "Longitudinal",
    dataType = "Longitudinal Data",
    gradeLevel = "Sophomore",
    achievementLevels = NULL, # no achievement levels
    omittedLevels = omittedLevels,
    survey = "HS&B",
    country = "USA",
    psuVar = "repcode_psu",
    stratumVar = "repcode_str",
    jkSumMultiplier = 1 / 184,
    validateFactorLabels = TRUE, # the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
    reqDecimalConversion = FALSE
  ) # decimal conversion is not needed
}


# identified the HS&B Senior weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyHSB_SOWeights <- function(fileFormat) {
  varNames <- fileFormat$variableName

  # identify weight variables
  wgtVars <- grep("wt(2|3|4){0,1}$", varNames, ignore.case = TRUE, value = TRUE)

  # TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars

  return(fileFormat)
}

# prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildHSB_SOWeightList <- function(fileFormat) {
  wgtVars <- fileFormat[fileFormat$weights == TRUE, "variableName"]

  # no wgts found
  if (length(wgtVars) == 0) {
    return(NULL)
  }

  weights <- list()

  for (i in 1:length(wgtVars)) {
    tempVar <- wgtVars[i] # full variable name of the weight var
    baseWgtVar <- paste0(tempVar, "_jk2_")
    wgtPattern <- paste0("^", baseWgtVar, "\\d+$")

    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- sub(baseWgtVar, "", ujkz, ignore.case = TRUE) # strip away and leave just the numeric variable name ending as a string

    if (length(ujkz) > 0) {
      tmpWgt <- list()

      tmpWgt[[1]] <- list(jkbase = baseWgtVar, jksuffixes = as.character(ujkz))

      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights, tmpWgt)
    }
  }

  return(weights)
}

buildHSB_SODataList <- function(lafObj, fileFormat) {
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
