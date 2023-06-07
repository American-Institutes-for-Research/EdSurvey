#' @title Connect to PIRLS Data
#'
#' @description Opens a connection to a PIRLS data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the PIRLS extracted
#'             SPSS (.sav) set of data
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}
#'                  or other online sources. Consult the \emph{PIRLS User Guide}
#'                  to help determine what countries
#'                  are included within a specific testing year of PIRLS.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the \code{readPIRLS} function by
#'                    using existing read-in data already processed.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the PIRLS international
#' database(s) using the \href{https://www.iea.nl/data-tools/repository}{IEA Study Data Repository}.
#' Data files require the SPSS data file (.sav) format using the default
#' filenames.
#'
#' @details
#' A PIRLS \code{edsurvey.data.frame} includes three distinct data levels:
#' \itemize{
#'   \item student
#'   \item school
#'   \item teacher
#'  }
#'
#' When the \code{getData} function is called using a PIRLS \code{edsurvey.data.frame},
#' the requested data variables are inspected, and it handles any necessary data merges automatically.
#' The \code{school} data always will be returned merged to the \code{student}
#' data, even if only \code{school} variables are requested.
#' If \code{teacher} variables are requested by the \code{getData} call, it
#' will cause \code{teacher} data to be merged.
#' Many \code{students} can be linked to many \code{teachers}, which varies widely between countries.
#'
#' Please note that calling the \code{dim} function for a PIRLS
#' \code{edsurvey.data.frame} will result in
#' the row count as if the \code{teacher} dataset was merged.
#' This row count will be considered the \code{full data N} of the
#' \code{edsurvey.data.frame}, even if no \code{teacher} data were
#' included in an analysis.
#' The column count returned by \code{dim} will be the count of unique
#' column variables across all three data levels.
#'
#' @return
#' an \code{edsurvey.data.frame} for a single specified country or an
#' \code{edsurvey.data.frame.list} if multiple countries specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, \code{\link{getData}}, and \code{\link{downloadPIRLS}}
#' @author Tom Fink
#'
#' @example man/examples/readPIRLS.R
#'
#' @importFrom haven read_sav
#' @import tibble
#' @export
readPIRLS <- function(path,
                      countries,
                      forceReread = FALSE,
                      verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl = TRUE, ignore.case = TRUE), dirname(path), path)

  if (!all(dir.exists(path))) {
    stop(paste0("The argument ", sQuote("path"), " cannot be located ", pasteItems(dQuote(path[!dir.exists(path)])), "."))
  }
  if (!is.logical(forceReread)) {
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if (!is.logical(verbose)) {
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }

  # prepwork
  countries <- tolower(unique(countries))
  includeLiteracy <- TRUE # include literacy as default

  gradeLvl <- 4 # PIRLS is only 4th grade data
  gradeL <- "a"

  if (unlist(countries)[1] == "*") { # user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path,
      pattern = paste0(
        "^", gradeL, ".....(",
        paste(getPIRLSYearCodes(), collapse = "|"), ")\\.sav$"
      ), full.names = FALSE, ignore.case = TRUE
    ), 4, 6)))
  }

  # gather datafile listing::be sure we only pickup PIRLS years based on the yearcodes
  filenames <- list.files(path,
    pattern = paste0(
      "^", gradeL, "..", "(", paste(countries, collapse = "|"), ")(",
      paste(getPIRLSYearCodes(), collapse = "|"), ")", "\\.sav$"
    ), full.names = TRUE, ignore.case = TRUE
  )
  if (length(filenames) == 0) {
    stop(paste0(
      "Could not find any PIRLS datafiles for countries ", paste(countries, collapse = ", "),
      " in the following folder(s) ", pasteItems(dQuote(path)), "."
    ))
  }

  fSubPart <- tolower(substring(basename(filenames), 1, 8)) # includes a (4th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames), 7, 8))))

  # ensure we grab the year code of the base year if only the literacy code is found for our loop, country only has literacy data
  yrNum <- unique(convertPIRLSYearCode(fileYrs))
  fileYrs <- getPIRLSYearCodes()[names(getPIRLSYearCodes()) %in% yrNum]

  procCountryData <- list()
  iProcCountry <- 0 # index counter for adding countries to the list

  for (yrCode in fileYrs[!(substring(fileYrs, 1, 1) == "l")]) { # loop through all the year codes first::excluding the literacy year codes
    for (cntry in countries) {
      PIRLSfiles <- list() # empty list

      PIRLSfiles <- c(
        "acg", # school background
        "asa", # student achievement
        "asg", # student background
        "ash", # student home background (Special file::might not always be present)
        "asr", # within-country scoring reliability
        "ast", # student-teacher linkage
        "atg"
      ) # teacher background


      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnamesLiteracy <- NULL
      yrCodeLiteracy <- unique(fileYrs[substring(fileYrs, 1, 1) == "l"]) # returns all literacy codes
      yrCodeLiteracy <- yrCodeLiteracy[convertPIRLSYearCode(yrCodeLiteracy) == convertPIRLSYearCode(yrCode)]

      fnames <- sapply(PIRLSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f, cntry, yrCode))] # added check here to only grab files for our specific grade level, country, and year
      }, simplify = FALSE)

      fnamesLiteracy <- sapply(PIRLSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f, cntry, yrCodeLiteracy))] # added check here to only grab files for our specific PIRLS grade level, country, and year
      }, simplify = FALSE)

      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g)) == 0
      }, simplify = TRUE)

      hasMissingLiteracy <- sapply(fnamesLiteracy, function(g) {
        sum(nchar(g)) == 0
      }, simplify = TRUE)

      hasExcess <- sapply(fnames, function(h) {
        length(h) > 1
      }, simplify = TRUE)

      hasExcessLiteracy <- sapply(fnamesLiteracy, function(h) {
        length(h) > 1
      }, simplify = TRUE)

      hasData <- sum(nchar(unlist(fnames))) > 0
      hasDataLiteracy <- sum(nchar(unlist(fnamesLiteracy))) > 0

      # includeLiteracy is TRUE as default, but retaining this as future reference
      if (includeLiteracy == FALSE) {
        hasDataLiteracy <- FALSE
        fnamesLiteracy <- NULL
        yrCodeLiteracy <- NULL
      }

      # test for any missing files::also check for any duplicate or multiple files
      if (sum(hasMissing) > 0 && hasData == TRUE && hasDataLiteracy == FALSE) {
        stop(paste0("Missing PIRLS datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasMissing]), "."))
      }
      if (sum(hasExcess) > 0 && hasData == TRUE && hasDataLiteracy == FALSE) {
        stop(paste0("Excess/duplicate PIRLS datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasExcess]), "."))
      }
      if (sum(hasMissingLiteracy) > 0 && hasData == FALSE && hasDataLiteracy == TRUE) {
        stop(paste0("Missing PIRLS literacy datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasMissingLiteracy]), "."))
      }
      if (sum(hasExcessLiteracy) > 0 && hasData == FALSE && hasDataLiteracy == TRUE) {
        stop(paste0("Excess/duplicate PIRLS literacy datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasExcessLiteracy]), "."))
      }
      if ((sum(hasMissing) > 0 || sum(hasMissingLiteracy) > 0) && hasData == TRUE && hasDataLiteracy == TRUE) {
        stop(paste0("Missing PIRLS datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasMissing]), " literacy files ", pasteItems(PIRLSfiles[hasMissingLiteracy]), "."))
      }
      if ((sum(hasExcess) > 0 || sum(hasExcessLiteracy) > 0) && hasData == TRUE && hasDataLiteracy == TRUE) {
        stop(paste0("Excess/duplicate PIRLS datafile(s) for country (", cntry, ") ", pasteItems(PIRLSfiles[hasExcess]), " literacy files ", pasteItems(PIRLSfiles[hasExcessLiteracy]), "."))
      }

      # test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (hasData == FALSE && hasDataLiteracy == FALSE) {
        next
      }

      iProcCountry <- iProcCountry + 1 # update the processed country index value after we confirm that there is data to process
      processedData <- list()

      if (hasData == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnames))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnames,
          fileYrs = yrCode,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processPIRLS", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processPIRLS", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process PIRLS data for country code ", dQuote(cntry),
                " having year code ", dQuote(yrCode), " at folder path(s) ", pasteItems(dQuote(path)),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }
      } # end if(hasData==TRUE)
      if (hasDataLiteracy == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnamesLiteracy))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnamesLiteracy,
          fileYrs = yrCodeLiteracy,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processPIRLS", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processPIRLS", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process PIRLS literacy data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }
      } # end if(hasDataLiteracy==TRUE)
      if (hasData == TRUE && hasDataLiteracy == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(c(dirname(unlist(fnames)), dirname(unlist(fnamesLiteracy)))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnames,
          fnamesLiteracy = fnamesLiteracy,
          fileYrs = paste0(yrCode, yrCodeLiteracy),
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processPIRLSAndLiteracy", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processPIRLSAndLiteracy", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process PIRLS and literacy data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }
      } # end if(hasData==TRUE && hasDataLiteracy==TRUE)

      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL
      processedData$data <- processedData$dataList$student
      processedData$dataSch <- processedData$dataList$school
      processedData$dataTch <- processedData$dataList$teacher

      testJKprefix <- c("JK", "JK.TCHWGT") # have any jk prefix values here that are applicable for this dataset
      weights <- NULL # default value

      for (i in 1:length(testJKprefix)) {
        ujkz <- unique(tolower(grep(paste0("^", "(", testJKprefix[i], ")", "[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
        ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) # remove jk to leave the numeric values

        if (length(ujkz) > 0) {
          if (testJKprefix[i] == "JK") {
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase = "jk", jksuffixes = as.character(ujkz))
            names(tmpWgt)[[1]] <- "totwgt"
            weights <- c(weights, tmpWgt)
          }
          if (testJKprefix[i] == "JK.TCHWGT") {
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase = "jk.tchwgt", jksuffixes = as.character(ujkz))
            names(tmpWgt)[[1]] <- "tchwgt"
            weights <- c(weights, tmpWgt)
          }
        }
      }
      attr(weights, "default") <- "totwgt"

      processedData$weights <- weights
      processedData$pvvars <- buildPVVARS_PIRLS(processedData$dataListFF$student, defaultPV = "rrea")
      processedData$subject <- c("Reading")
      processedData$year <- convertPIRLSYearCode(yrCode)
      processedData$assessmentCode <- "International"
      processedData$dataType <- "Student Data"
      processedData$gradeLevel <- "Grade 4"

      # achievment level cutpoints as defined by PIRLS documentation
      processedData$achievementLevels <- c("625", "550", "475", "400")
      names(processedData$achievementLevels) <- c("Advanced International Benchmark", "High International Benchmark", "Intermediate International Benchmark", "Low International Benchmark")

      processedData$omittedLevels <- c("Multiple", NA, "N/A", "NA", "OMITTED", "OMITTED OR INVALID", "NOT REACHED", "INVALID RESPONSE", "NOT APPLICABLE", "LOGICALLY NOT APPLICABLE", "MISSING", "(Missing)")

      processedData$fileFormat <- processedData$dataListFF$student
      processedData$fileFormatSchool <- processedData$dataListFF$school
      processedData$fileFormatTeacher <- processedData$dataListFF$teacher
      processedData$survey <- "PIRLS"
      processedData$country <- getPIRLSCountryName(cntry)

      procCountryData[[iProcCountry]] <- edsurvey.data.frame(
        userConditions = processedData$userConditions,
        defaultConditions = processedData$defaultConditions,
        dataList = buildPIRLS_dataList(
          processedData$data,
          processedData$fileFormat,
          processedData$dataSch,
          processedData$fileFormatSchool,
          processedData$dataTch,
          processedData$fileFormatTeacher
        ),
        weights = processedData$weights,
        pvvars = processedData$pvvars,
        subject = processedData$subject,
        year = processedData$year,
        assessmentCode = processedData$assessmentCode,
        dataType = processedData$dataType,
        gradeLevel = processedData$gradeLevel,
        achievementLevels = processedData$achievementLevels,
        omittedLevels = processedData$omittedLevels,
        survey = processedData$survey,
        country = processedData$country,
        psuVar = "jkrep",
        stratumVar = "jkzone",
        jkSumMultiplier = 0.5, # defined by the method of JK weight replication used (JK2)
        reqDecimalConversion = FALSE,
        dim0 = processedData$dim0
      )
    } # end country loop
  } # end for(fileYr in fileYrs)


  if (iProcCountry > 1) {
    return(edsurvey.data.frame.list(procCountryData)) # return full list.  let edsurvey.data.frame.list constructor build covs
  } else {
    # just one country
    return(procCountryData[[1]])
  }
}

# @param yrCode a character value used in the PIRLS filenaming structure to identify the specific year (e.g. m1, m2, m6)
# @return a numeric 4 digit year value
convertPIRLSYearCode <- function(yrCode) {
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% "r1"] <- 2001
  yrTest[yrTest %in% "r2"] <- 2006
  yrTest[yrTest %in% "r3"] <- 2011
  yrTest[yrTest %in% c("r4", "l2")] <- 2016

  return(yrTest)
}

getPIRLSYearCodes <- function() {
  # retrieve the TIMMS years based on their filenaming structure

  yrVals <- c("r1", "r2", "r3", "r4", "l2")
  names(yrVals) <- c(2001, 2006, 2011, 2016, 2016)

  return(yrVals)
}

# builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_PIRLS <- function(fileFormat, defaultPV = "rrea") {
  pvFields <- subset(fileFormat, nchar(fileFormat$Type) > 0) # type is identified in writeTibbleToFWFReturnFileFormat function
  constructs <- unique(pvFields$Type)

  # drop the international benchmark contructs as they are not true plausible values, only discrete numerics
  constructs <- constructs[!grepl("^(r|l)ibm$", constructs, ignore.case = TRUE)]

  pvvars <- vector("list", length(constructs))
  names(pvvars) <- constructs

  for (i in names(pvvars)) {
    varList <- tolower(sort(pvFields$variableName[pvFields$Type == i]))
    pvvars[[i]] <- list(varnames = varList)
  }

  # test if defaultPV in the list and make it default::otherwise set it to the first pvvar in the list
  if (defaultPV %in% names(pvvars)) {
    attr(pvvars, "default") <- defaultPV
  } else {
    attr(pvvars, "default") <- names(pvvars)[1]
  }

  return(pvvars)
}


# @param dataFolderPath a character value of the initial folder path provided to the 'readPIRLS' call to find the .sav SPSS files
# @param countryCode a character value of the 3-digit country code we want to process
# @param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
processPIRLS <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^a", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^a..", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < 3 || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PIRLS")) { # cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "asg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "acg"], cacheFile$dataListFF$school)
      teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "atg"], cacheFile$dataListFF$teacher)

      dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF

      dim0 <- cacheFile$dim0

      runProcessing <- FALSE
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  if (runProcessing == TRUE) {
    if (verbose == TRUE) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    # SCHOOL LEVEL===================================================
    acg <- unlist(fnames["acg"])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["acg"])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(acg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP)
    # ===============================================================

    # STUDENT LEVEL==================================================
    asa <- unlist(fnames["asa"])[1]
    asg <- unlist(fnames["asg"])[1]
    ash <- unlist(fnames["ash"])[1]
    asr <- unlist(fnames["asr"])[1]

    stuDF1 <- read_sav(asa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case = TRUE, value = TRUE)

    stuDF2 <- read_sav(asg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)
    colnames(stuDF2) <- toupper(colnames(stuDF2))
    ids2 <- grep("^ID", names(stuDF2), ignore.case = TRUE, value = TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER"))] # IDPUNCH should be omitted for merging

    mm <- mergeTibble(stuDF1,
      stuDF2,
      by = ids12,
      all.x = FALSE,
      all.y = FALSE,
      suffixes = c("", ".junk")
    )
    mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuDF1) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }
    if (nrow(stuDF2) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }
    if (min(is.na(ash)) == 0) {
      stuDF3 <- read_sav(ash, user_na = TRUE)
      stuDF3 <- UnclassCols(stuDF3)
      colnames(stuDF3) <- toupper(colnames(stuDF3))
      ids3 <- grep("^ID", names(stuDF3), ignore.case = TRUE, value = TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDPUNCH", "IDGRADER"))] # IDPUNCH should be omitted for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF3,
        by = idsmm3,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nrow(stuDF1) != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
    } else {
      idsmm3 <- ids12
    }
    if (min(is.na(asr)) == 0) {
      stuDF4 <- read_sav(asr, user_na = TRUE)
      stuDF4 <- UnclassCols(stuDF4)
      colnames(stuDF4) <- toupper(colnames(stuDF4))
      ids4 <- grep("^ID", names(stuDF4), ignore.case = TRUE, value = TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      idsmm4 <- idsmm4[!(idsmm4 %in% c("IDPUNCH", "IDGRADER"))] # IDPUNCH should be omitted for merging

      # test here for duplicate rows::special case for PIRLS 2001 for 'HKG' datafile having multiple data rows
      # anyDuplicated will return the row index of the first duplicate found. if no duplicates found, then it returns '0'
      if (anyDuplicated(stuDF4) > 0) {
        stuDF4 <- dropTibbleDupes(stuDF4)
      }

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF4,
        by = idsmm4,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
    }

    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames["asg"])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)
    # ===============================================================

    # Student-Teacher Linkage and Teacher Background=================
    ast <- unlist(fnames["ast"])[1]
    atg <- unlist(fnames["atg"])[1]

    stuTeachDF <- read_sav(ast, user_na = TRUE)
    stuTeachDF <- UnclassCols(stuTeachDF)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))

    teachDF <- read_sav(atg, user_na = TRUE)
    teachDF <- UnclassCols(teachDF)
    colnames(teachDF) <- toupper(colnames(teachDF))

    ids1 <- grep("^ID", names(stuTeachDF), ignore.case = TRUE, value = TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case = TRUE, value = TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER", "IDCLASS"))] # IDPUNCH should be omitted for merging

    mm <- mergeTibble(stuTeachDF,
      teachDF,
      by = ids12,
      all.x = TRUE,
      all.y = FALSE,
      suffixes = c("", ".junk")
    )
    mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("atg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }

    teachFP <- gsub(".sav$", "\\.txt", unlist(fnames["atg"])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    # ===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)

    # build data list and file format object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    # ===============================================================

    # calculate the dim0 to store in the .meta file for fast retreival
    nrow0 <- nrow(mm)
    ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, ffTeach$variableName)))
    dim0 <- c(nrow0, ncol0)

    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 4,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0("a", countryCode, yearCode, ".meta")))
  } else { # used the cache files
    if (verbose == TRUE) {
      cat(paste0("Found cached data for country code ", dQuote(countryCode), ".\n"))
    }
  } # end if(runProcessing==TRUE)

  return(list(
    dataList = dataList,
    dataListFF = dataListFF,
    dim0 = dim0
  ))
}


# @param dataFolderPath a character value of the initial folder path provided to the 'readPIRLS' call to find the .sav SPSS files
# @param countryCode a character value of the 3-digit country code we want to process
# @param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
processPIRLSAndLiteracy <- function(dataFolderPath, countryCode, fnames, fnamesLiteracy, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^a", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^a..", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < 3 || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PIRLS")) { # cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "asg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "acg"], cacheFile$dataListFF$school)
      teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == "atg"], cacheFile$dataListFF$teacher)

      dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF

      dim0 <- cacheFile$dim0

      runProcessing <- FALSE
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  if (runProcessing == TRUE) {
    if (verbose == TRUE) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    # SCHOOL LEVEL===================================================
    acg <- unlist(fnames["acg"])[1]
    acgL <- unlist(fnamesLiteracy["acg"])[1]

    schoolDF1 <- read_sav(acg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)

    schoolDF1Lit <- read_sav(acgL, user_na = TRUE)
    schoolDF1Lit <- UnclassCols(schoolDF1Lit)

    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    colnames(schoolDF1Lit) <- toupper(colnames(schoolDF1Lit))

    schoolDFx <- rBindTibble(schoolDF1, schoolDF1Lit)

    if (anyDuplicated(schoolDFx) > 0) {
      schoolDFx <- dropTibbleDupes(schoolDFx) # remove any duplicate school entries as we only need unique rows here
    }

    if (nrow(schoolDF1) != nrow(schoolDFx)) {
      # test that the school level is consistent between 4th grade and literacy fields.
      # not always the case where they are identical, but it's due to very small discrepencies in decimal percision.
    } else { # merge was good
      schoolDF1 <- schoolDFx
      schoolDFx <- NULL
    }

    schoolDF1Lit <- NULL

    colnames(schoolDF1) <- toupper(colnames(schoolDF1)) # ensure upper case intact

    schoolFP <- file.path(dataFolderPath, paste0("acg", countryCode, yearCode, ".txt"))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP)

    schoolDF1 <- NULL
    # ===============================================================

    # STUDENT LEVEL==================================================
    asa <- unlist(fnames["asa"])[1]
    asg <- unlist(fnames["asg"])[1]
    ash <- unlist(fnames["ash"])[1]
    asr <- unlist(fnames["asr"])[1]

    # for literacy files
    asaL <- unlist(fnamesLiteracy["asa"])[1]
    asgL <- unlist(fnamesLiteracy["asg"])[1]
    ashL <- unlist(fnamesLiteracy["ash"])[1]
    asrL <- unlist(fnamesLiteracy["asr"])[1]

    # ASA file prep===========
    stuDF1 <- read_sav(asa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)

    stuDF1Lit <- read_sav(asaL, user_na = TRUE)
    stuDF1Lit <- UnclassCols(stuDF1Lit)

    colnames(stuDF1) <- toupper(colnames(stuDF1))
    colnames(stuDF1Lit) <- toupper(colnames(stuDF1Lit))

    stuDFx <- rBindTibble(stuDF1, stuDF1Lit)

    if (anyDuplicated(stuDFx) > 0) {
      stuDFx <- dropTibbleDupes(stuDFx)
    }

    stuDF1 <- stuDFx
    stuDF1Lit <- NULL
    stuDFx <- NULL
    # ========================


    # ASG file prep==========
    stuDF2 <- read_sav(asg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)

    stuDF2Lit <- read_sav(asgL, user_na = TRUE)
    stuDF2Lit <- UnclassCols(stuDF2Lit)

    colnames(stuDF2) <- toupper(colnames(stuDF2))
    colnames(stuDF2Lit) <- toupper(colnames(stuDF2Lit))

    stuDFx <- rBindTibble(stuDF2, stuDF2Lit)

    if (anyDuplicated(stuDFx) > 0) {
      stuDFx <- dropTibbleDupes(stuDFx)
    }

    stuDF2 <- stuDFx
    stuDF2Lit <- NULL
    stuDFx <- NULL
    # ========================

    # ASA to ASG file merge===
    ids1 <- grep("^ID", names(stuDF1), ignore.case = TRUE, value = TRUE)
    ids2 <- grep("^ID", names(stuDF2), ignore.case = TRUE, value = TRUE)

    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER"))] # omit these ID vars for merging

    mm <- mergeTibble(stuDF1,
      stuDF2,
      by = ids12,
      all.x = FALSE,
      all.y = FALSE,
      suffixes = c("", ".junk")
    )
    mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuDF1) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }
    if (nrow(stuDF2) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }
    # =======================

    # ASH file prep and merge====
    if (min(is.na(ash)) == 0 || min(is.na(ashL)) == 0) {
      stuDF3 <- NULL
      stuDF3Lit <- NULL

      if (min(is.na(ash)) == 0) {
        stuDF3 <- read_sav(ash, user_na = TRUE)
        stuDF3 <- UnclassCols(stuDF3)
        colnames(stuDF3) <- toupper(colnames(stuDF3))
      }
      if (min(is.na(ashL)) == 0) {
        stuDF3Lit <- read_sav(ashL, user_na = TRUE)
        stuDF3Lit <- UnclassCols(stuDF3Lit)
        colnames(stuDF3Lit) <- toupper(colnames(stuDF3Lit))
      }

      stuDFx <- rBindTibble(stuDF3, stuDF3Lit)

      if (anyDuplicated(stuDFx) > 0) {
        stuDFx <- dropTibbleDupes(stuDFx)
      }

      stuDF3 <- stuDFx
      stuDF3Lit <- NULL
      stuDFx <- NULL

      ids3 <- grep("^ID", names(stuDF3), ignore.case = TRUE, value = TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDPUNCH", "IDGRADER"))] # IDPUNCH should be omitted for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF3,
        by = idsmm3,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nrow(stuDF1) != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
    } else {
      idsmm3 <- ids12
    }
    # =========================

    # ASR file prep and merge====
    if (min(is.na(asr)) == 0 || min(is.na(asrL)) == 0) {
      stuDF4 <- NULL
      stuDF4Lit <- NULL

      if (min(is.na(asr)) == 0) {
        stuDF4 <- read_sav(asr, user_na = TRUE)
        stuDF4 <- UnclassCols(stuDF4)
        colnames(stuDF4) <- toupper(colnames(stuDF4))
      }
      if (min(is.na(asr)) == 0) {
        stuDF4Lit <- read_sav(asrL, user_na = TRUE)
        stuDF4Lit <- UnclassCols(stuDF4Lit)
        colnames(stuDF4Lit) <- toupper(colnames(stuDF4Lit))
      }

      stuDFx <- rBindTibble(stuDF4, stuDF4Lit)

      if (anyDuplicated(stuDFx) > 0) {
        stuDFx <- dropTibbleDupes(stuDFx)
      }

      stuDF4 <- stuDFx
      stuDF4Lit <- NULL
      stuDFx <- NULL

      ids4 <- grep("^ID", names(stuDF4), ignore.case = TRUE, value = TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      idsmm4 <- idsmm4[!(idsmm4 %in% c("IDPUNCH", "IDGRADER"))] # IDPUNCH should be omitted for merging

      # test here for duplicate rows::special case for PIRLS 2001 for 'HKG' datafile having multiple data rows
      # anyDuplicated will return the row index of the first duplicate found. if no duplicates found, then it returns '0'
      if (anyDuplicated(stuDF4) > 0) {
        stuDF4 <- dropTibbleDupes(stuDF4)
      }

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF4,
        by = idsmm4,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please email EdSurvey.help@air.org for assistance."
        ))
      }
      mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]
    }
    # ==========================

    stuFP <- file.path(dataFolderPath, paste0("asg", countryCode, yearCode, ".txt"))
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)
    # ===============================================================

    # Student-Teacher Linkage and Teacher Background=================
    ast <- unlist(fnames["ast"])[1]
    atg <- unlist(fnames["atg"])[1]

    astL <- unlist(fnamesLiteracy["ast"])[1]
    atgL <- unlist(fnamesLiteracy["atg"])[1]

    # AST merge and prep====
    stuTeachDF <- read_sav(ast, user_na = TRUE)
    stuTeachDF <- UnclassCols(stuTeachDF)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))

    stuTeachDFL <- read_sav(astL, user_na = TRUE)
    stuTeachDFL <- UnclassCols(stuTeachDFL)
    colnames(stuTeachDFL) <- toupper(colnames(stuTeachDFL))

    stuTeachDFx <- rBindTibble(stuTeachDF, stuTeachDFL)

    if (anyDuplicated(stuTeachDFx) > 0) {
      stuTeachDFx <- dropTibbleDupes(stuTeachDFx)
    }

    stuTeachDF <- stuTeachDFx
    stuTeachDFL <- NULL
    stuTeachDFx <- NULL
    # ======================

    # atg merge and prep====
    teachDF <- read_sav(atg, user_na = TRUE)
    teachDF <- UnclassCols(teachDF)
    colnames(teachDF) <- toupper(colnames(teachDF))

    # DO NOT MERGE THE TEACHER BACKGROUND FILES BETWEEN Literacy and Normal PIRLS datasets, they are identical

    ids1 <- grep("^ID", names(stuTeachDF), ignore.case = TRUE, value = TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case = TRUE, value = TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER", "IDCLASS"))] # IDPUNCH should be omitted for merging

    mm <- mergeTibble(stuTeachDF,
      teachDF,
      by = ids12,
      all.x = TRUE,
      all.y = FALSE,
      suffixes = c("", ".junk")
    )
    mm <- mm[, names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("atg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please email EdSurvey.help@air.org for assistance."
      ))
    }
    # =========================

    teachFP <- file.path(dataFolderPath, paste0("atg", countryCode, yearCode, ".txt"))
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    # ===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)

    # build data list and file format object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    # ===============================================================

    # calculate the dim0 to store in the .meta file for fast retreival
    nrow0 <- nrow(mm)
    ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, ffTeach$variableName)))
    dim0 <- c(nrow0, ncol0)

    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 4,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0("a", countryCode, yearCode, ".meta")))
  } else { # used the cache files
    if (verbose == TRUE) {
      cat(paste0("Found cached data for country code ", dQuote(countryCode), ".\n"))
    }
  } # end if(runProcessing==TRUE)

  return(list(
    dataList = dataList,
    dataListFF = dataListFF,
    dim0 = dim0
  ))
}


exportPIRLSToCSV <- function(folderPath, exportPath, cntryCodes, ...) {
  sdfList <- readPIRLS(folderPath, cntryCodes, ...)

  if (inherits(sdfList, "edsurvey.data.frame.list")) {
    for (i in 1:length(sdfList$datalist)) {
      sdf <- sdfList$datalist[[i]]
      cntry <- sdf$country

      cat(paste(cntry, "working.\n"))
      data <- getData(sdf, colnames(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)


      write.csv(data, file = file.path(exportPath, paste0(cntry, ".csv")), na = "", row.names = FALSE)
      cat(paste(cntry, "completed.\n"))
    }
  } else if (inherits(sdfList, "edsurvey.data.frame")) {
    sdf <- sdfList
    cntry <- sdf$country

    cat(paste(cntry, "working.\n"))
    data <- getData(sdf, colnames(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)

    write.csv(data, file = file.path(exportPath, paste0(cntry, ".csv")), na = "", row.names = FALSE)
    cat(paste(cntry, "completed.\n"))
  }
}

# when a tibble has rows dropped/removed from it, the field attributes are striped from the columns, this preserves the column attributes
# \code{a} will be the tibble in which we want to remove the duplicates
# borrowed code from the 'mergeTibble' function in 'readTIMSS.r' file
dropTibbleDupes <- function(a) {
  ab <- a[!duplicated(a), ]
  cols <- names(ab)
  abt <- as_tibble(ab)

  for (i in 1:length(cols)) {
    coli <- cols[i]
    abcoli <- abt[[coli]]
    if (coli %in% names(a)) {
      ocoli <- a[[coli]]

      newAtrs <- attributes(ocoli)
      oldAnames <- names(attributes(abcoli))
      transname <- names(newAtrs)
      transname <- transname[!transname %in% oldAnames]
      for (tri in 1:length(transname)) {
        if ((!is.null(transname[tri])) && (!is.na(transname[tri])) && (length(transname[tri]) > 0)) {
          attr(abcoli, transname[tri]) <- newAtrs[[transname[tri]]]
        }
      }
      abt[[coli]] <- abcoli
    }
  }
  return(abt)
}

# get the full country name to aide the user, so they won't have to track them down.
# cntryCode should be the 3 character country code vector defined in the data filename scheme (e.g., usa = United States, swe = Sweden)
# if a match is not found, this funtion will return a character value indicating it is unknown '(unknown) CountryCode: xxx'
getPIRLSCountryName <- function(countryCode) {
  cntryCodeDF <- data.frame(
    cntryCode = c(
      "aad", "aba", "adu", "are", "arg", "aus", "aut", "aze",
      "bfl", "bfr", "bgr", "bhr", "blz", "bwa",
      "cab", "can", "cbc", "chl", "cns", "col", "cot", "cqu", "cyp", "cze",
      "deu", "dn3", "dnk",
      "ean", "egy", "ema", "eng", "esp",
      "fin", "fra",
      "geo", "grc",
      "hkg", "hnd", "hrv", "hun",
      "idn", "irl", "irn", "is5", "isl", "isr", "ita",
      "kaz", "kwt",
      "ltu", "lux", "lva",
      "ma6", "mac", "mar", "mda", "mkd", "mln", "mlt",
      "nir", "nld", "no4", "no5", "nor", "nzl",
      "omn",
      "pol", "prt",
      "qat",
      "rmo", "rom", "rus",
      "sau", "sco", "se3", "sgp", "svk", "svn", "swe",
      "tto", "tur", "twn",
      "usa",
      "za5", "zaf"
    ),
    cntryName = c(
      "Abu Dhabi, UAE", "Buenos Aires, Argentina", "Dubai, UAE", "United Arab Emirates", "Argentina", "Australia", "Austria", "Azerbaijan",
      "Belgium (Flemish)", "Belgium (French)", "Bulgaria", "Bahrain", "Belize", "Botswana",
      "Alberta, Canada", "Canada", "British Columbia, Canada", "Chile", "Nova Scotia, Canada", "Colombia", "Ontario, Canada", "Quebec, Canada", "Cyprus", "Czech Republic",
      "Germany", "Denmark (3rd Grade)", "Denmark",
      "Andalusia, Spain", "Egypt", "Madrid, Spain", "England", "Spain",
      "Finland", "France",
      "Georgia", "Greece",
      "Hong Kong SAR", "Honduras", "Croatia", "Hungary",
      "Indonesia", "Ireland", "Iran, Islamic Rep. of", "Iceland (Grade 5)", "Iceland", "Israel", "Italy",
      "Kazakhstan", "Kuwait",
      "Lithuania", "Luxembourg", "Latvia",
      "Morocco (6th Grade)", "Macao SAR", "Morocco", "Moldova, Republic of", "Macedonia, Rep. of", "Maltese - Malta", "Malta",
      "Northern Ireland", "Netherlands", "Norway (Grade 4)", "Norway (Grade 5)", "Norway", "New Zealand",
      "Oman",
      "Poland", "Portugal",
      "Qatar",
      "Moscow City, Russian Fed.", "Romania", "Russian Federation",
      "Saudi Arabia", "Scotland", "Sweden (Grade 3)", "Singapore", "Slovak Republic", "Slovenia", "Sweden",
      "Trinidad and Tobago", "Turkey", "Chinese Taipei",
      "United States",
      "Eng Afr Zulu RSA (5)", "South Africa"
    ),
    stringsAsFactors = FALSE
  ) # be sure to not create any factors::factors not needed at all

  lookupNames <- vector(mode = "character", length = length(countryCode))

  for (i in 1:length(countryCode)) {
    testName <- cntryCodeDF[cntryCodeDF$cntryCode == countryCode[i], "cntryName"]

    if (length(testName) == 0) { # test if no value found
      testName <- paste("(unknown) CountryCode:", countryCode[i])
    }

    lookupNames[i] <- testName
  }

  return(lookupNames)
}

# builds the PIRLS dataList object
buildPIRLS_dataList <- function(stuLaf, stuFF, schLaf, schFF, tchLaf, tchFF) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(
    lafObject = stuLaf,
    fileFormat = stuFF,
    levelLabel = "Student",
    forceMerge = TRUE,
    parentMergeLevels = NULL,
    parentMergeVars = NULL,
    mergeVars = NULL,
    ignoreVars = NULL,
    isDimLevel = FALSE
  )

  dataList[["School"]] <- dataListItem(
    lafObject = schLaf,
    fileFormat = schFF,
    levelLabel = "School",
    forceMerge = FALSE,
    parentMergeLevels = c("Student", "Student"),
    parentMergeVars = c("idcntry", "idschool"),
    mergeVars = c("idcntry", "idschool"),
    ignoreVars = names(schLaf)[names(schLaf) %in% names(stuLaf)],
    isDimLevel = FALSE
  )

  dataList[["Teacher"]] <- dataListItem(
    lafObject = tchLaf,
    fileFormat = tchFF,
    levelLabel = "Teacher",
    forceMerge = FALSE,
    parentMergeLevels = c("Student", "Student"),
    parentMergeVars = c("idcntry", "idstud"),
    mergeVars = c("idcntry", "idstud"),
    ignoreVars = names(tchLaf)[names(tchLaf) %in% names(stuLaf)],
    isDimLevel = TRUE
  )

  return(dataList)
}
