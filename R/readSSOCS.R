#' @title Connect to School Survey on Crime and Safety Data
#'
#' @description Opens a connection to a School Survey on Crime and Safety (SSOCS) data file and
#'              returns an \code{edsurvey.data.frame}, or an \code{edsurvey.data.frame.list} if multiple files specified,
#'              with information about the file(s) and data.
#'
#' @param sasDataFiles a character vector to the full SAS (*.sas7bdat) data file path(s) you wish to read.
#'                     If multiple paths are specified as a vector, it will return an \code{edsurvey.data.frame.list}.
#' @param years an integer vector of the year associated with the index position of the \code{sasDataFile} data file vector.
#'              The year is required to correctly determine required metadata about the file.
#'              Valid year values are as follows: 2000 (1999--2000), 2004 (2003--2004), 2006 (2005--2006), 2008 (2007--2008),
#'              2010 (2009--2010), 2016 (2015--2016), 2018 (2017--2018).
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the \code{readSSOCS} function by using existing read-in data already processed.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the SSOCS Data Products website in SAS format.  Other sources of SSOCS data, such as
#' restricted-use data or other websites, may require additional conversion steps to generate the required SAS format.
#'
#' @return An \code{edsurvey.data.frame} if one data file is specified or an \code{edsurvey.data.frame.list} if multiple files are specified in the \code{sasDataFiles} parameter.
#'
#' @note For the \code{readSSOCS} function, value label information is stored and retrieved automatically within the \code{EdSurvey} package (based on the year parameter),
#'       as the SAS files contain only raw data values.
#'
#' @seealso \code{\link{downloadSSOCS}}, and \code{\link{getData}}
#' @author Tom Fink
#' @importFrom haven read_sas
#' @example man/examples/readSSOCS.R
#' @export
readSSOCS <- function(sasDataFiles,
                      years,
                      forceReread = FALSE,
                      verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  userOp2 <- options(scipen = 999)
  on.exit(options(userOp2), add = TRUE)

  # validate the folder path(s) the user specified to make sure they exist
  sasDataFiles <- suppressWarnings(normalizePath(unique(sasDataFiles), winslash = "/"))

  if (!all(file.exists(sasDataFiles))) {
    stop(paste0("Unable to locate ", dQuote("sasDataFiles"), " file(s): ", pasteItems(dQuote(sasDataFiles[!file.exists(sasDataFiles)]))))
  }

  # input validation check
  if (length(sasDataFiles) != length(years)) {
    stop(eout(paste0("Vector length of ", dQuote("sasDataFiles"), " must equal vector length of ", dQuote("years"), ".")))
  }

  # ensure the sasDataFiles are .sas7bdat type
  validExt <- grepl("\\.sas7bdat$", sasDataFiles, ignore.case = TRUE)
  if (any(!validExt)) {
    stop(eout(paste0("The ", dQuote("sasDataFiles"), " file(s) are required to be *sas7bdat SAS binary data files. Invalid data file type: ", pasteItems(sQuote(sasDataFiles[!validExt])), ".")))
  }

  validYears <- c(2000, 2004, 2006, 2008, 2010, 2016, 2018)
  if (!all(years %in% validYears) || length(years) == 0) {
    stop(eout(paste0("Error validating ", dQuote("years"), " argument. Valid years are: ", paste(validYears, sep = " ", collapse = ", "), ".")))
  }

  fileLookupInfo <- readRDS(system.file("extdata", "SSOCS_FileLookup.rds", package = "EdSurvey"))
  valueLabelInfo <- readRDS(system.file("extdata", "SSOCS_ValueLabels.rds", package = "EdSurvey"))

  returnList <- vector(mode = "list", length = length(sasDataFiles))

  for (iFile in seq_along(sasDataFiles)) {
    iYr <- years[iFile]
    fileInfoSub <- subset(fileLookupInfo, Year == iYr)
    valueLabelSub <- subset(valueLabelInfo, Year == iYr)

    if (nrow(fileInfoSub) == 0) {
      warning(paste0(
        "Unable to locate background information to correctly build survey information for analysis for the provided file: ",
        dQuote(sasDataFiles[iFile]), ". Use the ", dQuote("downloadSSOCS"), " function (using the default filenames) to ensure files are compatible."
      ))
    } else if (nrow(fileInfoSub) > 1) {
      warning(paste0("Excess file information found for file: ", dQuote(sasDataFiles[iFile]), ". Using the first found record."))
      fileInfoSub <- fileInfoSub[1, ]
    }

    # uses double pipes (||) as delimiter in case of multiple weight specifications
    wgtVars <- fileInfoSub$WeightVars[1]
    wgtVars <- unlist(strsplit(wgtVars, "||", fixed = TRUE))

    wgtRepRegex <- fileInfoSub$WeightRepRegex[1]
    wgtRepRegex <- unlist(strsplit(wgtRepRegex, "||", fixed = TRUE))


    procArgs <- list(
      filePath = sasDataFiles[iFile],
      wgtVars = wgtVars,
      valueLabels = valueLabelSub,
      forceReread = forceReread,
      verbose = verbose
    )

    retryProc <- tryCatch(
      {
        processedData <- do.call("processSSOCS", procArgs, quote = TRUE)
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
      procArgs[["forceReread"]] <- TRUE # try it again reprocessing the data
      eout("Cache corrupt, attempting to reread the data.")
      processedData <- tryCatch(do.call("processSSOCS", procArgs, quote = TRUE),
        error = function(e) {
          stop(paste0(
            "Unable to process SSOCS data. Possible file corruption with source data.",
            " Error message: ", e
          ))
        }
      )
    }

    wgtList <- buildSSOCSWeightList(processedData$dataListFF$data, wgtVars, wgtRepRegex)
    # set a default weight
    if (length(wgtList) > 0) {
      attr(wgtList, "default") <- names(wgtList)[1]
    }

    omitLvls <- unlist(strsplit(fileInfoSub$OmittedLevels, "||", fixed = TRUE))
    omitLvls <- c(omitLvls, NA, "NA", "(Missing)")

    returnList[[iFile]] <- edsurvey.data.frame(
      userConditions = list(),
      defaultConditions = NULL,
      dataList = buildSSOCSDataList(processedData$dataList$data, processedData$dataListFF$data),
      weights = wgtList,
      pvvars = NULL,
      subject = fileInfoSub$Description,
      year = fileInfoSub$Year,
      assessmentCode = "Cross Sectional",
      dataType = "Survey Data",
      gradeLevel = "",
      achievementLevels = NULL, # no achievement levels
      omittedLevels = omitLvls,
      survey = "SSOCS",
      country = "USA",
      psuVar = tolower(fileInfoSub$PSU_Var),
      stratumVar = tolower(fileInfoSub$Stratum_Var),
      jkSumMultiplier = fileInfoSub$Jackknife_Multiplier,
      validateFactorLabels = TRUE,
      reqDecimalConversion = FALSE
    )
  }

  if (length(returnList) > 1) {
    return(edsurvey.data.frame.list(returnList))
  } else {
    return(returnList[[1]])
  }
}

processSSOCS <- function(filePath, wgtVars, valueLabels, forceReread, verbose) {
  wrkDir <- dirname(filePath)

  baseNoExt <- gsub("[.]sas7bdat$", "", basename(filePath), ignore.case = TRUE)

  metaCacheFP <- list.files(wrkDir,
    pattern = paste0("^", baseNoExt, "\\.meta$"), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(wrkDir,
    pattern = paste0("^", baseNoExt, "\\.txt$"), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) == 0 || forceReread == TRUE) {
    runProcessing <- TRUE # determine if we need to process data or not
  } else {
    cacheFile <- readRDS(metaCacheFP[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "SSOCS")) {
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      dataLAF <- getFWFLaFConnection(txtCacheFWF, cacheFile$dataListFF$data)

      dataList <- list(data = dataLAF)
      dataListFF <- list(data = cacheFile$dataListFF$data)

      runProcessing <- FALSE
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  # need to to the processing of the data file, gather the file format, write the cache file and return the proper info
  if (runProcessing) {
    # remove the cache first in case processing fails, so we don't end up in bad user state
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    data <- read_sas(filePath)
    ff <- getSSOCS_FileFormat(data, valueLabels, verbose)

    fwfPath <- file.path(wrkDir, paste0(baseNoExt, ".txt"))
    ff <- writeDF_FWF(data, ff, fwfPath, verbose)

    # identify the weight variables and change it's logical to TRUE
    ff$weights <- toupper(ff$variableName) %in% toupper(wgtVars)

    # gather the LaF connections to return
    dataLaF <- getFWFLaFConnection(fwfPath, ff)

    dataList <- list(data = dataLaF)
    dataListFF <- list(data = ff)

    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 1,
      ts = Sys.time(),
      dataListFF = dataListFF
    )

    metaPath <- file.path(wrkDir, paste0(baseNoExt, ".meta"))
    saveRDS(cacheFile, metaPath)
  } else {
    if (verbose == TRUE) {
      cat(paste0("Found cached data for file ", dQuote(basename(filePath)), ".\n"))
    }
  } # end if(runProcessing)

  return(list(
    dataList = dataList,
    dataListFF = dataListFF
  ))
}

# identified the SSOCS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifySSOCSWeights <- function(fileFormat) {
  varNames <- fileFormat$variableName

  # weight vars all begin with wta
  wgtVars <- grep("^wta", varNames, value = TRUE, ignore.case = TRUE)

  # TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars

  return(fileFormat)
}

# using the column names of the supplied sas7bdat file this will return
getSSOCS_FileFormat <- function(data, valueLabels, verbose) {
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

  # convert to data.frame from tibble for processing (easier to work with)
  data <- UnclassCols(data)

  # get list of the variable names to match to later
  valLblLookup <- strsplit(toupper(valueLabels$variables), "||", fixed = TRUE)

  # grab the variable labels here before converting tibble to data.frame
  varLbls <- sapply(data, function(z) {
    a <- attributes(z)
    l <- a$label

    if (is.null(l) || is.na(l)) {
      l <- ""
    } else {
      l
    }
  })

  varNames <- colnames(data)

  # we are going to leverage the 'writeDF_FWF' function here to determine the FWF start/end/width and datatype specs, as it's all checked there!
  # ensure this captures all the fields, value labels will applied later
  dict$variableName <- varNames
  dict$Start <- rep(NA, times = length(varNames))
  dict$End <- rep(NA, times = length(varNames))
  dict$Width <- rep(NA, times = length(varNames))
  dict$Decimal <- rep(NA, times = length(varNames))
  dict$Labels <- unlist(varLbls)
  dict$labelValues <- rep("", times = length(varNames))
  dict$Type <- rep("", times = length(varNames))
  dict$pvWt <- rep("", times = length(varNames))
  dict$dataType <- rep("", times = length(varNames))
  dict$weights <- rep(FALSE, times = length(varNames))

  # apply the value labels as they are defined in the lookup table
  for (i in seq_along(varNames)) {
    d <- data[ , i, drop = TRUE]

    # attempt to determine it's data type, will be validated in the writeDF_FWF function as well
    if (is.numeric(d)) {
      dc <- as.character(d)
      if (any(grepl("[.]", dc))) { # test if decimal is present
        dict$dataType[i] <- "numeric"
      } else {
        dict$dataType[i] <- "integer"
      }
    } else {
      dict$dataType[i] <- "character"
    }

    # check if variable has value labels associated with it
    for (iVL in seq_along(valLblLookup)) {
      if (toupper(varNames[i]) %in% valLblLookup[iVL][[1]]) {
        dict$labelValues[i] <- valueLabels$ValueLabels[iVL]
        break
      }
    }
  } # end for(i in seq_along(varNames))

  ff <- as.data.frame(dict, stringsAsFactors = FALSE)

  return(ff)
}

# prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
# the wgtVars and the associated wgtRepRegex should be in the same vector index position to correctly identify the replicate variables by name
buildSSOCSWeightList <- function(fileFormat, wgtVars, wgtRepRegex) {
  # no wgts found
  if (length(wgtVars) == 0 || is.null(wgtVars)) {
    return(NULL)
  }

  wgtList <- vector(mode = "list", length = length(wgtVars)) # build list to return

  for (i in seq_along(wgtVars)) {
    repVars <- grep(wgtRepRegex[i], fileFormat$variableName, ignore.case = TRUE, value = TRUE)

    if (length(repVars) > 0 && !is.null(repVars)) {
      base <- unique(gsub("\\d{1,2}$", "", repVars))
      reps <- unique(gsub(base, "", repVars))

      tempWgt <- list(jkbase = tolower(base), jksuffixes = as.character(reps))

      wgtList[[i]] <- tempWgt
      tempWgt <- NULL
    }
  }

  names(wgtList) <- wgtVars
  return(wgtList)
}

buildSSOCSDataList <- function(dataLaF, fileFormat) {
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

  return(dataList)
}
