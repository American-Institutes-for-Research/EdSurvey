#' @title Connect to High School Longitudinal Study 2009 (HSLS:2009) Data
#'
#' @description Opens a connection to an HSLS data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value to the full directory path(s) to the HSLS
#'             extracted SPSS (.sav) set of data files
#' @param filename a character value of the name of the SPSS (.sav) datafile to be read
#' @param wgtFilename a character value of the name of the associated BRR
#'                    weight SPSS (.sav) data file in the specificed \code{path}
#'                    to be read.
#'                    This argument is only applicable for the restricted-use
#'                    student level data, which contains a separate data-file
#'                    containing the weight replicate information.
#'                    For data files with no balanced
#'                    repeated replication (BRR) weight file associated,
#'                    specify a value of \code{NULL} or \code{NA}.
#' @param forceReread a logic value to force a rereading of all processed data.
#'                    The default value of \code{FALSE} speeds up the
#'                    \code{readHSLS} function by using existing read-in data
#'                    already processed.
#' @param verbose a logical value set to \code{TRUE} for verbose output that
#'                indicates progress
#' @details
#' Reads in the unzipped files downloaded from the HSLS longitudinal dataset.
#' @note
#' The SPSS (.sav) format is preferred over the fixed-width-format (.dat)
#' ASCII file format at this time relating to value label issues identified
#' with the ASCII layout specifications.
#'
#' @return an \code{edsurvey.data.frame} for the HSLS longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example man/examples/readHSLS.R
#' @importFrom haven read_sav
#' @export
readHSLS <- function(path = getwd(),
                     filename = "hsls_16_student_v1_0.sav",
                     wgtFilename = NA,
                     forceReread = FALSE,
                     verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl = TRUE, ignore.case = TRUE), dirname(path), path)

  # setup file list to work with
  fileList <- list(dataFile = unlist(file.path(path, filename))[1])

  # validate files::get the filecount to see if we have any missing or excess files
  validateData <- sapply(fileList$dataFile, file.exists)

  if (!all(validateData == TRUE)) {
    missingVars <- names(validateData == TRUE)
    if (length(missingVars) > 0) {
      stop(paste0("Cannot find specified data file ", dQuote(missingVars), " in path ", dQuote(path), "."))
    }
  }

  # do checks for the wgtFilename argument
  if (is.null(wgtFilename)) {
    wgtFilename <- NA
  }

  # test and validate weighting files if they are specified
  if (!is.na(wgtFilename)) {
    fileList$dataFileWgt <- unlist(file.path(path, wgtFilename))[1]

    # validate files::get the filecount to see if we have any missing or excess files
    validateData <- sapply(fileList$dataFileWgt, file.exists)
    layoutData <- sapply(fileList$layoutFileWgt, file.exists)

    if (!all(validateData == TRUE)) {
      missingVars <- names(validateData == TRUE)
      if (length(missingVars) > 0) {
        stop(paste0("Cannot find specified weight data file ", dQuote(missingVars), " in path ", dQuote(path), "."))
      }
    }
    if (!all(layoutData == TRUE)) {
      missingVars <- names(layoutData == TRUE)

      if (length(missingVars) > 0) {
        stop(paste0("Cannot find specified weight layout file ", dQuote(missingVars), " in path ", dQuote(path), "."))
      }
    }
  } # end if(!is.na(wgtFilename))

  cacheInfo <- list(
    cacheFilepath = gsub("\\.sav$", "\\.txt", fileList$dataFile, ignore.case = TRUE),
    cacheFilepathWgt = gsub("\\.sav$", "\\.txt", fileList$dataFileWgt, ignore.case = TRUE),
    cacheMetaFilepath = gsub("\\.sav$", "\\.meta", fileList$dataFile, ignore.case = TRUE)
  )

  processArgs <- list(
    files = fileList,
    cacheFileInfo = cacheInfo,
    forceReread = forceReread,
    verbose = verbose
  )

  retryProc <- tryCatch(
    {
      processedData <- do.call("processHSLS_SPSS", processArgs, quote = TRUE)
      FALSE
    },
    error = function(e) {
      TRUE # flag to retry
    },
    warning = function(w) {
      TRUE # flag to retry
    }
  )

  if (retryProc) {
    processArgs[["forceReread"]] <- TRUE # try it again reprocessing the data
    eout("Cache corrupt, attempting to reread the data.")
    processedData <- tryCatch(do.call("processHSLS_SPSS", processArgs, quote = TRUE),
      error = function(e) {
        stop(paste0(
          "Unable to process HSLS data. Possible file corruption with source data.",
          " Error message: ", e
        ))
      }
    )
  }

  weights <- buildHSLSWeightList(processedData$fileFormat$data, processedData$fileFormat$wgt)

  # fix up these specific variables to be categorical
  processedData$fileFormat$data <- valueLabel_MakeCategorical(processedData$fileFormat$data, c("x1aqdate", "x1cqdate"), processedData$data$data)
  processedData$fileFormat$wgt <- valueLabel_MakeCategorical(processedData$fileFormat$wgt, c("x1aqdate", "x1cqdate"), processedData$data$wgt)

  if (!is.null(weights)) {
    attr(weights, "default") <- ""
  }

  pvs <- list() # no plausible values or achievement levels
  omittedLevels <- c(
    "Data suppressed", "DATA SUPPRESSED",
    "Undeclared/undecided", "UNDECLARED/UNDECIDED",
    "Missing", "MISSING",
    "Don't know", "DON'T KNOW",
    "Unit non-response", "UNIT NON-RESPONSE",
    "Unit non-response/component not applicable", "UNIT NON-RESPONSE/COMPONENT NOT APPLICABLE",
    "Carry through missing", "CARRY THROUGH MISSING",
    "Item legitimate skip/NA", "ITEM LEGITIMATE SKIP/NA",
    "Item not administered: abbreviated interview", "ITEM NOT ADMINISTERED: ABBREVIATED INTERVIEW",
    "Component not applicable", "COMPONENT NOT APPLICABLE",
    "Did not attempt credit", "DID NOT ATTEMPT CREDIT",
    "Did not attempt", "DID NOT ATTEMPT",
    NA,
    "(Missing)"
  )

  # cleaup any '0=ZERO' labels as those are a problem for analysis
  processedData$fileFormat$data <- valueLabelCleanupFF(processedData$fileFormat$data, omittedLevels, c("Zero", "zero", "ZERO")) # cleanup

  if (!is.null(processedData$fileFormat$wgt)) {
    processedData$fileFormat$wgt <- valueLabelCleanupFF(processedData$fileFormat$wgt, omittedLevels, c("Zero", "zero", "ZERO")) # cleanup
  }

  # return the edsurvey.data.frame
  edsurvey.data.frame(
    userConditions = list(),
    defaultConditions = NULL,
    dataList = buildHSLSDataList(processedData$data$data, processedData$fileFormat$data, processedData$data$wgt, processedData$fileFormat$wgt),
    weights = weights,
    pvvars = pvs,
    subject = "",
    year = "2009",
    assessmentCode = "Longitudinal",
    dataType = "Longitudinal Data",
    gradeLevel = "High School",
    achievementLevels = NULL, # no achievement levels
    omittedLevels = omittedLevels,
    survey = "HSLS",
    country = "USA",
    psuVar = "psu", # these are suppressed in the public dataset for some reason
    stratumVar = "strat_id", # these are suppressed in the public dataset for some reason
    jkSumMultiplier = 0.005, # For BRR weight replication: (1/200) Strata
    validateFactorLabels = TRUE, # the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
    reqDecimalConversion = FALSE
  ) # decimal conversion is not needed
}


# helper function for processing the SPSS file format version of the HSLS
processHSLS_SPSS <- function(files,
                             cacheFileInfo,
                             forceReread,
                             verbose) {
  runProcessing <- TRUE # set default value
  hasWeightFile <- !is.null(files$dataFileWgt) # create wgt file flag
  fileFormatWgt <- NULL

  # check and validate any cached files to see if they should be used
  if (file.exists(cacheFileInfo$cacheFilepath)) {
    if (file.exists(cacheFileInfo$cacheMetaFilepath)) {
      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)

      if (!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "HSLS")) {
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat$data

        if (hasWeightFile) {
          fileFormatWgt <- cacheRDS$fileFormat$wgt

          if (is.na(fileFormatWgt) || is.null(fileFormatWgt)) { # user previously ran the file without specifying weights and we must re-cache with weights
            runProcessing <- TRUE
          }
        }
      }
    }
  }

  # force reprocess if called for
  if (forceReread) {
    runProcessing <- TRUE
  }

  if (runProcessing) {
    # first delete the existing cache file if it exists in case the processing errors then it won't pickup the cache file
    if (file.exists(cacheFileInfo$cacheFilepath)) {
      file.remove(cacheFileInfo$cacheFilepath)
    }

    if (file.exists(cacheFileInfo$cacheMetaFilepath)) {
      file.remove(cacheFileInfo$cacheMetaFilepath)
    }

    # get fileFormat from the SPSS file here
    if (verbose) {
      eout(paste0("Reading SPSS file data."))
    }

    spssDF <- read_sav(files$dataFile, user_na = TRUE)
    spssDF <- UnclassCols(spssDF)

    # get the fileFormat from the SPSS attributes
    if (verbose) {
      eout(paste0("Gathering SPSS data format."))
    }
    fileFormat <- getSPSSFileFormat(spssDF)
    fileFormat <- writeDF_FWF(spssDF, fileFormat, cacheFileInfo$cacheFilepath, verbose)
    fileFormat <- identifyHSLSWeights(fileFormat)
    spssDF <- NULL # clear memory

    # build the file format of the weight file if exists
    fileFormatWgt <- NULL
    if (hasWeightFile) {
      spssDFWgt <- read_sav(files$dataFileWgt, user_na = TRUE)
      spssDFWgt <- UnclassCols(spssDFWgt)

      fileFormatWgt <- getSPSSFileFormat(spssDFWgt)
      fileFormatWgt <- writeDF_FWF(spssDFWgt, fileFormatWgt, cacheFileInfo$cacheFilepathWgt, verbose)
      fileFormatWgt <- identifyHSLSWeights(fileFormatWgt)
    }

    # write cache file and .meta
    cacheFile <- list(
      ver = ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
      cacheFileVer = 2,
      ts = Sys.time(),
      fileFormat = list(data = fileFormat, wgt = fileFormatWgt)
    )

    saveRDS(cacheFile, cacheFileInfo$cacheMetaFilepath)
  } else { # end if(runProcessing)
    if (verbose) {
      eout(paste0("Found cached data for file ", dQuote(files$dataFile), "."))
    }
  } # end if(runProcessing==TRUE)

  # return LAF as open to edsurvey.data.frame constructor where it needs it open to first build, then it handles closing within there
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)

  # ensure LaF throws no errors by reading first line
  tryCatch(dataLAF[1, ],
    error = function(e) {
      close(dataLAF)
      stop(e)
    }
  )

  dataLAFWgt <- NULL
  if (!is.null(fileFormatWgt)) {
    dataLAFWgt <- laf_open_fwf(cacheFileInfo$cacheFilepathWgt, column_types = fileFormatWgt$dataType, column_widths = fileFormatWgt$Width, column_names = fileFormatWgt$variableName)

    # ensure LaF throws no errors by reading first line
    tryCatch(dataLAFWgt[1, ],
      error = function(e) {
        close(dataLAFWgt)
        stop(e)
      }
    )
  }

  # do caching and testing
  return(list(
    data = list(data = dataLAF, wgt = dataLAFWgt),
    fileFormat = list(data = fileFormat, wgt = fileFormatWgt)
  ))
}

# identified the weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyHSLSWeights <- function(fileFormat) {
  varNames <- fileFormat$variableName

  # unable to grepl as it picked up too many replicate weights
  wgtVars <- varNames[grepl("^w(1|2|3|4).*[^0-9]$", varNames, ignore.case = TRUE)]
  wgtVars <- c(wgtVars, varNames[grepl("^w4w1stup1$", varNames, ignore.case = TRUE)])
  wgtVars <- c(wgtVars, varNames[grepl("^w4w1stup1p2$", varNames, ignore.case = TRUE)])

  # TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- tolower(fileFormat$variableName) %in% tolower(wgtVars)

  return(fileFormat)
}

# prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildHSLSWeightList <- function(fileFormat1, fileFormat2) {
  wgtVars <- unique(c(fileFormat1[fileFormat1$weights == TRUE, "variableName"], fileFormat2[fileFormat2$weights == TRUE, "variableName"]))

  # no wgts found
  if (length(wgtVars) == 0) {
    return(NULL)
  }

  weights <- list()

  for (i in 1:length(wgtVars)) {
    tempVar <- wgtVars[i]
    # testJKprefix <- substr(tempVar, 1, nchar(tempVar)-1) #strip the ending '0' from the variable::all the replicates will have the same name but numbered 1-n

    if (!is.null(fileFormat2)) {
      ujkz <- unique(c(
        tolower(grep(paste0("^", "(", tempVar, ")", "[0-9]"), fileFormat1$variableName, value = TRUE, ignore.case = TRUE)),
        tolower(grep(paste0("^", "(", tempVar, ")", "[0-9]"), fileFormat2$variableName, value = TRUE, ignore.case = TRUE))
      ))
    } else {
      ujkz <- unique(tolower(grep(paste0("^", "(", tempVar, ")", "[0-9]"), fileFormat1$variableName, value = TRUE, ignore.case = TRUE)))
    }

    ujkz <- gsub(tolower(tempVar), "", ujkz, fixed = TRUE) # remove jk to leave the numeric values

    if (length(ujkz) > 0) {
      tmpWgt <- list()
      tmpWgt[[1]] <- list(jkbase = tolower(tempVar), jksuffixes = as.character(ujkz))
      names(tmpWgt)[[1]] <- tolower(tempVar)
      weights <- c(weights, tmpWgt)
    }
  }

  return(weights)
}

buildHSLSDataList <- function(dataLaF, fileFormat, wgtLaf, wgtFF) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(
    lafObject = dataLaF,
    fileFormat = fileFormat,
    levelLabel = "Data",
    forceMerge = TRUE,
    parentMergeLevels = NULL,
    parentMergeVars = NULL,
    mergeVars = NULL,
    ignoreVars = NULL,
    isDimLevel = TRUE
  )

  if (!is.null(wgtFF)) {
    dataList[["Weight"]] <- dataListItem(
      lafObject = wgtLaf,
      fileFormat = wgtFF,
      levelLabel = "Weight",
      forceMerge = FALSE,
      parentMergeLevels = c("Data"),
      parentMergeVars = c("stu_id"),
      mergeVars = c("stu_id"),
      ignoreVars = wgtFF$variableName[wgtFF$variableName %in% fileFormat$variableName],
      isDimLevel = FALSE
    )
  }

  return(dataList)
}
