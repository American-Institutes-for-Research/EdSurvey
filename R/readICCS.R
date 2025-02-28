#' @title Connect to ICCS and CivED Data
#'
#' @description Opens a connection to an ICCS (2009, 2016) or CivEd (1999) data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value of the full directory to the ICCS/CivED extracted SPSS (.sav) set of data
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit International Organization for Standardization (ISO) country code.
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}
#'                  or other online sources. Consult the \emph{ICCS/CivED User Guide} to help determine what countries
#'                  are included within a specific testing year of ICCS/CivED.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param dataSet a character value of either \code{student} or \code{teacher} to indicate which set of data is returned.
#'                  The student-level and teacher-level datasets cannot both be returned at the same time, unlike other IEA datasets.
#'                  Note: The CivED 1999 study also included student-to-teacher data for Grade 8.  Specifying \code{dataSet="student"} and \code{gradeLvl=8}
#'                  will include both the \code{student} and \code{teacher} data in the resulting \code{edsurvey.data.frame}.
#' @param gradeLvl a character value of the grade level to return
#'                 \itemize{
#'                      \item{\strong{8} = eighth grade (the default if not specified)}
#'                      \item{\strong{9} = ninth grade}
#'                      \item{\strong{12} = 12th grade (for CivED 1999 only)}
#'                 }
#'
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the \code{readCivEDICCS} function by using existing read-in data already processed.
#'
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the international database(s) using the \href{https://www.iea.nl/data-tools/repository}{IEA Study Data Repository}.
#'          Data files require the SPSS data file (.sav) format using the default filenames.
#'
#' @details When using the \code{getData} function with a CivED or ICCS study \code{edsurvey.data.frame},
#'          the requested data variables are inspected, and it handles any necessary data merges automatically.
#'          The \code{school} data always will be returned merged to the \code{student}
#'          data, even if only \code{school} variables are requested.
#'          If a 1999 CivED Grade 8 \code{edsurvey.data.frame} with \code{teacher} data variables is requested by the \code{getData} call,
#'          it will cause \code{teacher} data to be merged.
#'          Many \code{students} can be linked to many \code{teachers}, which varies widely between countries,
#'          and not all countries contain \code{teacher} data.
#'
#' @details Calling the \code{dim} function for a CivED 1999 Grade 8 \code{edsurvey.data.frame} will result in the row count as if the \code{teacher} dataset was merged.
#'          This row count will be considered the \code{full data N} of the \code{edsurvey.data.frame}, even if no \code{teacher} data were included in an analysis.
#'          The column count returned by \code{dim} will be the count of unique column variables across all data levels.
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or an \code{edsurvey.data.frame.list} if multiple countries specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, \code{\link{getData}}, and \code{\link{downloadCivEDICCS}}
#' @author Tom Fink
#'
#' @example man/examples/readICCS.R
#'
#' @importFrom haven read_sav
#' @import tibble
#' @export
readCivEDICCS <- function(path,
                          countries,
                          dataSet = c("student", "teacher"),
                          gradeLvl = c("8", "9", "12"),
                          forceReread = FALSE,
                          verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl = TRUE, ignore.case = TRUE), dirname(path), path)

  if (!all(dir.exists(path))) {
    stop(paste0("The argument ", sQuote("path"), " cannot be located: ", pasteItems(dQuote(path[!dir.exists(path)])), "."))
  }

  dataSet <- tolower(dataSet)
  dataSet <- match.arg(dataSet)

  gradeLvl <- tolower(gradeLvl)
  gradeLvl <- match.arg(gradeLvl)

  if (length(dataSet) > 1) {
    dataSet <- dataSet[1]
  }
  if (length(gradeLvl) > 1) {
    gradeLvl <- gradeLvl[1]
  }
  if (sum(!(dataSet %in% c("student", "teacher")) > 0)) {
    stop(paste0("The argument ", sQuote("dataSet"), " must be either ", sQuote("student"), " or ", sQuote("teacher"), "."))
  }
  if (sum(!dir.exists(path)) > 0) { # validate the paths to ensure they all exist
    stop(paste0("Cannot find ", sQuote("path"), " value in ", pasteItems(dQuote(path[!dir.exists(path)])), "."))
  }
  if (!is.logical(forceReread)) {
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if (!is.logical(verbose)) {
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }

  # prepwork
  countries <- tolower(unique(countries))
  # grab first gradeLvl to use for all
  gradeLvl <- gradeLvl[[1]]
  if (gradeLvl == "8") { # i is for ICCS and b is for 1999 CivEd
    gradeL <- c("i", "b")
  } else if (gradeLvl == "9") { # j is for ICCS 9th grade
    gradeL <- "j"
  } else if (gradeLvl == "12") { # c is for CivED 12th grade
    gradeL <- "c"
  } else {
    stop(paste0("The argument ", sQuote("gradeLvl"), " must be either ", sQuote("8"), ", ", sQuote("9"), " or ", sQuote("12"), "."))
  }

  if (unlist(countries)[1] == "*") { # user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path,
      pattern = paste0(
        "^", "(", paste(gradeL, collapse = "|"), ")", ".....(",
        paste(getICCSYearCodes(), collapse = "|"), ")\\.sav$"
      ), full.names = FALSE, ignore.case = TRUE
    ), 4, 6)))
  }

  # gather datafile listing::be sure we only pickup ICILS years based on the yearcodes
  filenames <- list.files(path,
    pattern = paste0(
      "^", "(", paste(gradeL, collapse = "|"), ")", "..", "(", paste(countries, collapse = "|"), ")(",
      paste(getICCSYearCodes(), collapse = "|"), ")", "\\.sav$"
    ), full.names = TRUE, ignore.case = TRUE
  )


  if (length(filenames) == 0) {
    countries <- ifelse(length(countries) == 0, "*", countries)
    stop(paste0(
      "Could not find any CivED/ICCS datafiles for countries ", paste(countries, collapse = ", "),
      " in the following folder(s) ", pasteItems(dQuote(path)), "."
    ))
  }

  fSubPart <- tolower(substring(basename(filenames), 1, 8)) # includes grade level code, country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames), 7, 8))))

  procCountryData <- list()
  iProcCountry <- 0 # index counter for adding countries to the list

  for (yrCode in fileYrs) { # loop through all the year codes first
    for (cntry in countries) {
      ICCSfiles <- list() # empty list
      CivEDfiles <- list() # empty list

      if (gradeLvl == "8") { # i is for ICCS and b is for 1999 CivEd
        if (dataSet == "student") {
          ICCSfiles <- c(
            "icg", # school background
            "isa", # student achievement
            "ise", # student european module
            "isg", # student background
            "isl", # student latin american module
            "isr", # student scoring reliability
            "iss"
          ) # student asian module
          CivEDfiles <- c(
            "bc_", # school background
            "bs_", # student background
            "bt_", # teacher background
            "bl_"
          ) # student-teacher linkage
        } else if (dataSet == "teacher") {
          ICCSfiles <- c(
            "icg", # school background
            "itg"
          ) # teacher background
          CivEDfiles <- c(
            "bc_", # school background
            "bt_"
          ) # teacher background
        }
      } else if (gradeLvl == "9") { # j is for ICCS 9th grade
        ICCSfiles <- c(
          "jcg", # school background
          "jsa", # student achievement
          "jse", # student european module
          "jsg", # student background
          "jsl", # student latin american module
          "jsr", # student scoring reliability
          "jss"
        ) # student asian module
        CivEDfiles <- NULL # not applicable
      } else if (gradeLvl == "12") { # c is for CivED 12th grade
        ICCSfiles <- NULL # not applicable
        CivEDfiles <- c(
          "cc_", # school background::this filetype does not exist for 12th grade, but still pass it along
          "cs_", # student background
          "ct_", # teacher background::this doesn't exist but pass it along
          "cl_"
        ) # teacher/student link::this doesn't exist but pass it along
      }

      fnamesICCS <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnamesCivED <- NULL

      fnamesICCS <- sapply(ICCSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f, cntry, yrCode))] # added check here to only grab files for our specific CivED/ICCS grade level, country, and year
      }, simplify = FALSE)

      fnamesCivED <- sapply(CivEDfiles, function(f) {
        filenames[(fSubPart %in% paste0(f, cntry, yrCode))] # added check here to only grab files for our specific CivED/ICCS grade level, country, and year
      }, simplify = FALSE)

      # determine what datasets we are working with, either CivED or ICCS
      hasICCSData <- FALSE
      hasCivEDData <- FALSE

      # both of these will never be true since we are looping on the year codes
      if (sum(nchar(unlist(fnamesICCS))) > 0) { # test if we have any ICCS files
        hasICCSData <- TRUE
      }
      if (sum(nchar(unlist(fnamesCivED))) > 0) { # test if we have any CivED files
        hasCivEDData <- TRUE
      }

      hasMissingICCS <- unlist(sapply(fnamesICCS, function(g) {
        sum(nchar(g)) == 0
      }, simplify = TRUE))

      # omit these files specifically from checks due to the region modules
      hasMissingICCS["ise"] <- FALSE
      hasMissingICCS["isl"] <- FALSE
      hasMissingICCS["iss"] <- FALSE
      hasMissingICCS["jse"] <- FALSE
      hasMissingICCS["jsl"] <- FALSE
      hasMissingICCS["jss"] <- FALSE
      hasMissingICCS["jcg"] <- FALSE # 9th grade ICCS student

      hasMissingCivED <- unlist(sapply(fnamesCivED, function(g) {
        sum(nchar(g)) == 0
      }, simplify = TRUE))

      # for 8th grade student data if they don't have a teacher or linkage data
      hasMissingCivED["bt_"] <- FALSE
      hasMissingCivED["bl_"] <- FALSE

      hasMissingCivED["cc_"] <- FALSE # for 12th grade student data there are no school records
      hasMissingCivED["ct_"] <- FALSE # for 12th grade student data there are no teacher records
      hasMissingCivED["cl_"] <- FALSE # for 12th grade student data there are no student/teacher records

      hasExcessICCS <- unlist(sapply(fnamesICCS, function(h) {
        length(h) > 1
      }, simplify = TRUE))

      hasExcessCivED <- unlist(sapply(fnamesCivED, function(h) {
        length(h) > 1
      }, simplify = TRUE))

      # test if no teacher file exists if we are looking at teacher level data
      if (sum(nchar(unlist(fnamesICCS["itg"]))) == 0 && hasICCSData == TRUE && dataSet == "teacher") {
        warning(paste0("No ICCS teacher background file. Skipping country ", sQuote(cntry), " for year ", sQuote(convertICCSYearCode(yrCode)), "."))
        next
      }

      # test if no teacher file exists if we are looking at teacher level data
      if (sum(nchar(unlist(fnamesCivED["bt_"]))) == 0 && hasCivEDData == TRUE && dataSet == "teacher") {
        warning(paste0("No CivED teacher background file. Skipping country ", sQuote(cntry), "."))
        next
      }

      # test for any missing files from the datasets for ICCS Datafiles
      if (sum(hasMissingICCS) > 0 && hasICCSData == TRUE && sum(nchar(unlist(fnamesICCS))) > 0) {
        stop(paste0("Missing ICCS datafile(s) for country (", cntry, ") ", paste(ICCSfiles[hasMissingICCS], collapse = ", "), " for dataset ", sQuote(dataSet), "."))
      }
      if (sum(hasExcessICCS) > 0 && hasICCSData == TRUE && sum(nchar(unlist(fnamesICCS))) > 0) {
        stop(paste0("Excess/duplicate ICCS datafile(s) for country (", cntry, ") ", paste(ICCSfiles[hasExcessICCS], collapse = ", "), " for dataset ", sQuote(dataSet), "."))
      }

      # test for any missing files from the datasets for CivED Datafiles
      if (sum(hasMissingCivED) > 0 && hasCivEDData == TRUE && sum(nchar(unlist(fnamesCivED))) > 0) {
        stop(paste0("Missing CivED datafile(s) for country (", cntry, ") ", paste(CivEDfiles[hasMissingCivED], collapse = ", "), " for dataset ", sQuote(dataSet), "."))
      }
      if (sum(hasExcessCivED) > 0 && hasCivEDData == TRUE && sum(nchar(unlist(fnamesCivED))) > 0) {
        stop(paste0("Excess/duplicate CivED datafile(s) for country (", cntry, ") ", paste(CivEDfiles[hasExcessCivED], collapse = ", "), " for dataset ", sQuote(dataSet), "."))
      }

      # test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (sum(nchar(unlist(fnamesICCS))) == 0 && sum(nchar(unlist(fnamesCivED))) == 0) {
        warning(paste0("No data found. Skipping country ", sQuote(cntry), "."))
        next
      }

      iProcCountry <- iProcCountry + 1 # update the processed country index value after we confirm that there is data to process
      processedData <- list()

      if (dataSet == "student" && hasICCSData == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnamesICCS))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnamesICCS,
          fileYrs = yrCode,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processICCS.Student", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processICCS.Student", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process ICCS student data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }

        # ICCS student level achievement levels
        if (convertICCSYearCode(yrCode) == "2009") {
          processedData$achievementLevels <- c("395", "479", "563")
          names(processedData$achievementLevels) <- c("Level 1", "Level 2", "Level 3")
        } else if (convertICCSYearCode(yrCode) == "2016") {
          processedData$achievementLevels <- c(310.9999, 394.9999, 478.9999, 562.9999)
          names(processedData$achievementLevels) <- c("Level D", "Level C", "Level B", "Level A")
        } else {
          warning(paste0("ICCS Student achievement levels undefined for year code: ", sQuote(yrCode), ", and will not be available for analysis."))
        }


        testJKprefix <- c("JK") # have any jk prefix values here that are applicable for this dataset
        weights <- NULL # default value

        for (i in seq_along(testJKprefix)) {
          ujkz <- unique(tolower(grep(paste0("^", "(", testJKprefix[i], ")", "[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) # remove jk to leave the numeric values

          if (length(ujkz) > 0) {
            if (testJKprefix[i] == "JK") {
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase = "jk", jksuffixes = as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgts"
              weights <- c(weights, tmpWgt)
              attr(weights, "default") <- "totwgts"
            }
          }
        }

        if (!is.null(weights)) {
          attr(weights, "default") <- "totwgts"
        }

        processedData$weights <- weights
        processedData$psuVar <- "jkreps"
        processedData$stratumVar <- "jkzones"

        processedData$dataType <- "Student"
        processedData$pvvars <- buildPVVARS_ICCS(processedData$dataListFF$student, defaultPV = "civ")
        processedData$jkSumMultiplier <- 1.0 # defined by the method of JK weight replication used (JK1)
      } else if (dataSet == "teacher" && hasICCSData == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnamesICCS))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnamesICCS,
          fileYrs = yrCode,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processICCS.Teacher", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processICCS.Teacher", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process ICCS teacher data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }


        processedData$achievementLevels <- NULL # no achievement levels

        testJKprefix <- c("JK.TOTWGTT") # have any jk prefix values here that are applicable for this dataset
        weights <- NULL # default value

        for (i in seq_along(testJKprefix)) {
          ujkz <- unique(tolower(grep(paste0("^", "(", testJKprefix[i], ")", "[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) # remove jk to leave the numeric values

          if (length(ujkz) > 0) {
            if (testJKprefix[i] == "JK.TOTWGTT") {
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase = "jk.totwgtt", jksuffixes = as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgtt"
              weights <- c(weights, tmpWgt)
              attr(weights, "default") <- "totwgtt"
            }
          }
        }

        if (!is.null(weights)) {
          attr(weights, "default") <- "totwgtt"
        }

        processedData$weights <- weights
        processedData$dataType <- "Teacher"
        processedData$psuVar <- "jkrept"
        processedData$stratumVar <- "jkzonet"

        processedData$pvvars <- NULL # no pvvars for teachers
        processedData$jkSumMultiplier <- 1.0 # defined by the method of JK weight replication used (JK1)
      } else if (dataSet == "student" && hasCivEDData == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnamesCivED))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnamesCivED,
          fileYrs = yrCode,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processCivED.Student", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processCivED.Student", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process CivED student data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }

        processedData$achievementLevels <- NULL # no achievement levels

        testJKprefix <- c("JK", "JK.TOTWGTCH") # have any jk prefix values here that are applicable for this dataset
        weights <- NULL # default value

        for (i in seq_along(testJKprefix)) {
          ujkz <- unique(tolower(grep(paste0("^", "(", testJKprefix[i], ")", "[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) # remove jk to leave the numeric values

          # for CivED some countries don't have any jkzone/jkrep data, thus they really don't have weight variables
          if (length(ujkz) > 0) {
            if (testJKprefix[i] == "JK") {
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase = "jk", jksuffixes = as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgt"
              weights <- c(weights, tmpWgt)
            }
            if (testJKprefix[i] == "JK.TOTWGTCH") {
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase = "jk.totwgtch", jksuffixes = as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgtch"
              weights <- c(weights, tmpWgt)
            }
          }
        }

        if (!is.null(weights)) {
          attr(weights, "default") <- "totwgt"
        }

        processedData$weights <- weights
        processedData$psuVar <- "jkrep"
        processedData$stratumVar <- "jkzone"

        processedData$dataType <- "Student"
        processedData$pvvars <- buildPVVARS_ICCS(processedData$dataListFF$student, defaultPV = "civ")
        processedData$jkSumMultiplier <- 1.0 # defined by the method of JK weight replication used (JK1)
      } else if (dataSet == "teacher" && hasCivEDData == TRUE) {
        processArgs <- list(
          dataFolderPath = unique(dirname(unlist(fnamesCivED))), # specify only the directory in which the files exist
          countryCode = cntry,
          fnames = fnamesCivED,
          fileYrs = yrCode,
          forceReread = forceReread,
          verbose = verbose
        )

        retryProc <- tryCatch(
          {
            processedData <- do.call("processCivED.Teacher", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("processCivED.Teacher", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process CivED teacher data for country code ", sQuote(cntry),
                " having year code ", sQuote(yrCode), " at folder path(s) ", paste(sQuote(path), collapse = " & "),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }

        processedData$achievementLevels <- NULL # no achievement levels

        processedData$dataType <- "Teacher"
        processedData$weights <- NULL
        processedData$pvvars <- NULL # no pvvars for teachers
        processedData$psuVar <- NULL # "jkrept"
        processedData$stratumVar <- NULL # "jkzonet"
        processedData$jkSumMultiplier <- 1.0 # defined by the method of JK weight replication used (JK1)
      } # end if(dataSet=="xxx" && hasCivED/ICCSData==xxx) block


      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL

      processedData$subject <- c("Civic and Citizenship")
      processedData$year <- convertICCSYearCode(yrCode)
      processedData$assessmentCode <- "International"

      processedData$gradeLevel <- paste("Grade", gradeLvl)

      processedData$omittedLevels <- c(
        "Multiple", NA, "OMITTED", "OMITTED OR INVALID",
        "LOGICALLY NOT APPLICABLE", "MISSING", "INVALID",
        "NOT ADMIN", "NOT ADMIN.", "NOT ADMINISTERED", "NOT REACHED", "(Missing)"
      )


      processedData$survey <- ifelse(hasICCSData, "ICCS", "CivED")
      processedData$country <- getICCSCountryName(cntry)

      procCountryData[[iProcCountry]] <- edsurvey.data.frame(
        userConditions = processedData$userConditions,
        defaultConditions = processedData$defaultConditions,
        dataList = buildICCS_dataList(
          dataSet, hasICCSData, hasCivEDData,
          processedData$dataList$student,
          processedData$dataListFF$student,
          processedData$dataList$school,
          processedData$dataListFF$school,
          processedData$dataList$teacher,
          processedData$dataListFF$teacher
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
        psuVar = processedData$psuVar,
        stratumVar = processedData$stratumVar,
        jkSumMultiplier = processedData$jkSumMultiplier,
        reqDecimalConversion = FALSE,
        dim0 = processedData$dim0
      )
    } # end country loop
  } # end for(fileYr in fileYrs)


  if (iProcCountry > 1) {
    return(edsurvey.data.frame.list(procCountryData)) # do not apply country labels::the edsurvey.data.frame.list does a better job of detecting the differences
  } else {
    # just one country
    if (length(procCountryData) > 0) {
      return(procCountryData[[1]])
    } else {
      return(NULL) # no data to return
    }
  }
}

convertICCSYearCode <- function(yrCode) {
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% c("f2")] <- 1999
  yrTest[yrTest %in% c("c2")] <- 2009
  yrTest[yrTest %in% c("c3")] <- 2016

  return(yrTest)
}

getICCSYearCodes <- function() {
  # retrieve the TIMMS years based on their filenaming structure

  yrVals <- c("f2", "c2", "c3")
  names(yrVals) <- c(1999, 2009, 2016)

  return(yrVals)
}

# process the ICCS student level data..be sure the cache files are seperate from the teacher cache files
processICCS.Student <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  gradeLvlCode <- unique(tolower(substr(basename(unlist(fnames[nchar(unlist(fnames)) > 0])), 1, 1))) # get the first character from the base filenames for the grade level code (i=8 or j=9)

  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "s", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "[sc].", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < ifelse(gradeLvlCode == "j", 1, 2) || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "ICCS")) { # cacheMetaReqUpdates resides in own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats

      if (!is.null(cacheFile$dataListFF$school)) {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "sg")], cacheFile$dataListFF$student)
        schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "cg")], cacheFile$dataListFF$school)


        dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      } else {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "sg")], cacheFile$dataListFF$student)

        dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      }
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<ifelse(gradeLvlCode=="j",1,2) || forceReread==TRUE)

  if (runProcessing) {
    if (verbose) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }
    # SCHOOL LEVEL===================================================
    cg <- unlist(fnames[paste0(gradeLvlCode, "cg")])[1]

    if (min(is.na(cg)) == 0) {
      schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "cg")])[1], ignore.case = TRUE)
      schoolDF1 <- read_sav(cg, user_na = TRUE)
      schoolDF1 <- UnclassCols(schoolDF1)
      colnames(schoolDF1) <- toupper(colnames(schoolDF1))
      ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP, jkType = "JK1")
    } else {
      ffsch <- NULL
    }
    # ===============================================================

    # STUDENT LEVEL==================================================
    sa <- unlist(fnames[paste0(gradeLvlCode, "sa")])[1] # student achievement
    sg <- unlist(fnames[paste0(gradeLvlCode, "sg")])[1] # student questionaire
    sr <- unlist(fnames[paste0(gradeLvlCode, "sr")])[1] # student scoring reliablity

    # this file will depend on the country region so most will be empty
    se <- unlist(fnames[paste0(gradeLvlCode, "se")])[1] # student european module
    sl <- unlist(fnames[paste0(gradeLvlCode, "sl")])[1] # student latin-american module
    ss <- unlist(fnames[paste0(gradeLvlCode, "ss")])[1] # student asian module

    stuDF1 <- read_sav(sa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case = TRUE, value = TRUE)

    stuDF2 <- read_sav(sg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)
    colnames(stuDF2) <- toupper(colnames(stuDF2))
    ids2 <- grep("^ID", names(stuDF2), ignore.case = TRUE, value = TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

    mm <- mergeTibble(stuDF1,
      stuDF2,
      by = ids12,
      all.x = FALSE,
      all.y = FALSE,
      suffixes = c("", ".junk")
    )
    mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuDF1) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "sa")), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
      ))
    }
    if (nrow(stuDF2) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "sg")), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
      ))
    }
    if (min(is.na(sr)) == 0) {
      stuDF3 <- read_sav(sr, user_na = TRUE)
      stuDF3 <- UnclassCols(stuDF3)
      colnames(stuDF3) <- toupper(colnames(stuDF3))
      ids3 <- grep("^ID", names(stuDF3), ignore.case = TRUE, value = TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF3,
        by = idsmm3,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nrow(stuDF1) != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "sr")), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "sr")), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
    } else {
      idsmm3 <- ids12
    }

    if (min(is.na(se)) == 0) {
      stuDF4 <- read_sav(se, user_na = TRUE)
      stuDF4 <- UnclassCols(stuDF4)
      colnames(stuDF4) <- toupper(colnames(stuDF4))
      ids4 <- grep("^ID", names(stuDF4), ignore.case = TRUE, value = TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      idsmm4 <- idsmm4[!(idsmm4 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF4,
        by = idsmm4,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "se")), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
    } else {
      idsmm4 <- idsmm3
    }

    if (min(is.na(sl)) == 0) {
      stuDF5 <- read_sav(sl, user_na = TRUE)
      stuDF5 <- UnclassCols(stuDF5)
      colnames(stuDF5) <- toupper(colnames(stuDF5))
      ids5 <- grep("^ID", names(stuDF5), ignore.case = TRUE, value = TRUE)
      idsmm5 <- idsmm4[idsmm4 %in% ids5]
      idsmm5 <- idsmm5[!(idsmm5 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF5,
        by = idsmm5,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "sl")), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
    } else {
      idsmm5 <- idsmm4
    }

    if (min(is.na(ss)) == 0) {
      stuDF6 <- read_sav(ss, user_na = TRUE)
      stuDF6 <- UnclassCols(stuDF6)
      colnames(stuDF6) <- toupper(colnames(stuDF6))
      ids6 <- grep("^ID", names(stuDF6), ignore.case = TRUE, value = TRUE)
      idsmm6 <- idsmm5[idsmm5 %in% ids6]
      idsmm6 <- idsmm6[!(idsmm6 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
        stuDF6,
        by = idsmm6,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote(paste0(gradeLvlCode, "ss")), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
    }

    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "sg")])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP, jkType = "JK1")
    # ===============================================================

    if (!is.null(ffsch)) {
      schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu, school = ffsch)

      # calculate the dim0 to store in the .meta file for fast retreival
      nrow0 <- nrow(mm)
      ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName)))
      dim0 <- c(nrow0, ncol0)
    } else { # for 9th grade data since they don't have any school metrics

      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu)

      # calculate the dim0 to store in the .meta file for fast retreival
      nrow0 <- nrow(mm)
      ncol0 <- length(unique(c(ffstu$variableName)))
      dim0 <- c(nrow0, ncol0)
    }

    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 6,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0(gradeLvlCode, "s", countryCode, yearCode, ".meta")))
  } else { # used the cache files
    if (verbose) {
      cat(paste0("Found cached data for country code ", dQuote(countryCode), ".\n"))
    }
  } # end if(runProcessing)

  return(list(
    dataList = dataList,
    dataListFF = dataListFF,
    dim0 = dim0
  ))
}


# process the ICCS teacer level data..be sure the cache files are seperate from the teacher cache files
processICCS.Teacher <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  gradeLvlCode <- unique(tolower(substr(basename(unlist(fnames[nchar(unlist(fnames)) > 0])), 1, 1))) # get the first character from the base filenames for the grade level code (i=8 or j=9)

  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "t", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "[tc].", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < 2 || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "ICCS")) { # cacheMetaReqUpdates resides in own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "tg")], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "cg")], cacheFile$dataListFF$school)


      dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF

      dim0 <- cacheFile$dim0

      runProcessing <- FALSE
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<2 || forceReread==TRUE)

  if (runProcessing == TRUE) {
    if (verbose == TRUE) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    # SCHOOL LEVEL===================================================
    cg <- unlist(fnames[paste0(gradeLvlCode, "cg")])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "cg")])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(cg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP, jkType = "JK1")
    # ===============================================================

    # TEACHER LEVEL===================================================
    tg <- unlist(fnames[paste0(gradeLvlCode, "tg")])[1]
    teacherFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "tg")])[1], ignore.case = TRUE)
    teacherDF1 <- read_sav(tg, user_na = TRUE)
    teacherDF1 <- UnclassCols(teacherDF1)
    colnames(teacherDF1) <- toupper(colnames(teacherDF1))
    fftch <- writeTibbleToFWFReturnFileFormat(teacherDF1, teacherFP, jkType = "JK1")
    # ===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    teacherLAF <- getFWFLaFConnection(teacherFP, fftch)

    # build data list and link metadata object=======================
    dataList <- list(student = teacherLAF, school = schoolLAF) # teachers will be considered 'students' in this context since it's the primary data level
    dataListFF <- list(student = fftch, school = ffsch)
    # ===============================================================

    # calculate the dim0 to store in the .meta file for fast retreival
    nrow0 <- nrow(teacherDF1)
    ncol0 <- length(unique(c(ffsch$variableName, fftch$variableName)))
    dim0 <- c(nrow0, ncol0)

    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 6,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0(gradeLvlCode, "t", countryCode, yearCode, ".meta")))
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

# process the CivED student level data..be sure the cache files are seperate from the teacher cache files
processCivED.Student <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  gradeLvlCode <- unique(tolower(substr(basename(unlist(fnames[nchar(unlist(fnames)) > 0])), 1, 1))) # get the first character from the base filenames for the grade level code (b=8 or c=12)

  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "s", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "[scz].", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < ifelse(gradeLvlCode == "c", 1, 2) || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "CivED")) { # cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats

      if (!is.null(cacheFile$dataListFF$school)) {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "s_")], cacheFile$dataListFF$student)
        schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "c_")], cacheFile$dataListFF$school)

        if (!is.null(cacheFile$dataListFF$teacher)) {
          teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "z_")], cacheFile$dataListFF$teacher) # use the 'z' here for cache file since we don't want to interfere w/ the teacher function and we include linkage data
          dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        } else {
          dataList <- list(student = studentLAF, school = schoolLAF) # only student and school levels
        }

        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      } else {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "s_")], cacheFile$dataListFF$student)

        dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      }
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<ifelse(gradeLvlCode=="c",1,3) || forceReread==TRUE)

  if (runProcessing == TRUE) {
    if (verbose == TRUE) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    # SCHOOL LEVEL===================================================
    cg <- unlist(fnames[paste0(gradeLvlCode, "c_")])[1]

    if (min(is.na(cg)) == 0) {
      schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "c_")])[1], ignore.case = TRUE)
      schoolDF1 <- read_sav(cg, user_na = TRUE)
      schoolDF1 <- UnclassCols(schoolDF1)
      colnames(schoolDF1) <- toupper(colnames(schoolDF1))

      # update or fix any strange value labels for the school dataset
      schoolDF1 <- fixCivED_DataLabels(schoolDF1, yearCode, countryCode)

      ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP, jkType = "JK1")
    } else {
      ffsch <- NULL
    }
    # ===============================================================

    # STUDENT LEVEL==================================================
    sg <- unlist(fnames[paste0(gradeLvlCode, "s_")])[1] # student file

    stuDF1 <- read_sav(sg, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "s_")])[1], ignore.case = TRUE)

    # update or fix any strange value labels for the student dataset
    stuDF1 <- fixCivED_DataLabels(stuDF1, yearCode, countryCode)

    ffstu <- writeTibbleToFWFReturnFileFormat(stuDF1, stuFP, jkType = "JK1")
    # ===============================================================

    # TEACHER LEVEL==================================================

    tg <- unlist(fnames[paste0(gradeLvlCode, "t_")])[1] # teacher file
    tl <- unlist(fnames[paste0(gradeLvlCode, "l_")])[1] # teacher/student linkage file

    if (min(is.na(tg)) == 0 && min(is.na(tl)) == 0) {
      tchLink <- read_sav(tl, user_na = TRUE)
      tchLink <- UnclassCols(tchLink)

      # a few countries in the BL_ file have duplicate records in them::this will remove them
      if (anyDuplicated(tchLink) > 0) {
        tchLink <- dropTibbleDupes(tchLink)
      }

      tchBK <- read_sav(tg, user_na = TRUE)
      tchBK <- UnclassCols(tchBK)

      if (anyDuplicated(tchBK) > 0) {
        tchBK <- dropTibbleDupes(tchBK)
      }

      colnames(tchLink) <- toupper(colnames(tchLink))
      colnames(tchBK) <- toupper(colnames(tchBK))

      ids1 <- grep("^ID", names(tchLink), ignore.case = TRUE, value = TRUE)
      ids2 <- grep("^ID", names(tchBK), ignore.case = TRUE, value = TRUE)
      ids12 <- ids1[ids1 %in% ids2]
      ids12 <- ids12[!(ids12 %in% c("IDGRADER", "IDPUNCH"))] # omit these vars for merging

      mm <- mergeTibble(tchLink,
        tchBK,
        by = ids12,
        all.x = TRUE,
        all.y = FALSE,
        suffixes = c("", ".junk")
      )
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

      # need to make adjustment to have this file with the 'z' instead of the 't'
      tchFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "t_")])[1], ignore.case = TRUE)
      tempBN <- tolower(basename(tchFP))
      tempBN <- gsub(paste0(gradeLvlCode, "t_"), paste0(gradeLvlCode, "z_"), tempBN, fixed = TRUE) # swap the 't' char for a 'z' to not interfere w/ only teacher level cache file
      tchFP <- file.path(dirname(tchFP), tempBN)

      # update or fix any strange value labels for the teacher dataset
      mm <- fixCivED_DataLabels(mm, yearCode, countryCode)

      fftch <- writeTibbleToFWFReturnFileFormat(mm, tchFP, jkType = "JK1")
    } else {
      fftch <- NULL
    } # end if(min(is.na(tg)) == 0 &&  min(is.na(tl)) == 0)
    # ===============================================================

    if (!is.null(ffsch)) {
      schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      if (!is.null(fftch)) {
        teacherLAF <- getFWFLaFConnection(tchFP, fftch)

        dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- list(student = ffstu, school = ffsch, teacher = fftch)

        # calculate the dim0 to store in the .meta file for fast retreival
        nrow0 <- nrow(mm)
        ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, fftch$variableName)))
        dim0 <- c(nrow0, ncol0)
      } else { # no teacher level data

        dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- list(student = ffstu, school = ffsch)

        # calculate the dim0 to store in the .meta file for fast retreival
        nrow0 <- nrow(stuDF1)
        ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName)))
        dim0 <- c(nrow0, ncol0)
      }
    } else { # for 9th grade data since they don't have any school or teacher level data

      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu)

      # calculate the dim0 to store in the .meta file for fast retreival
      nrow0 <- nrow(stuFP)
      ncol0 <- length(unique(c(ffstu$variableName)))
      dim0 <- c(nrow0, ncol0)
    }


    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 6,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0(gradeLvlCode, "s", countryCode, yearCode, ".meta")))
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

# process the CivED teacher level data
processCivED.Teacher <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  yearCode <- unlist(fileYrs)[1]
  gradeLvlCode <- unique(tolower(substr(basename(unlist(fnames[nchar(unlist(fnames)) > 0])), 1, 1))) # get the first character from the base filenames for the grade level code (b=8 or c=12)

  metaCacheFP <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "t", "(", paste(countryCode), ")",
      yearCode, "\\.meta$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
    pattern = paste0(
      "^", gradeLvlCode, "[tc].", "(", paste(countryCode), ")",
      yearCode, "\\.txt$"
    ), full.names = TRUE, ignore.case = TRUE
  )

  # determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if (length(metaCacheFP) == 0 || length(txtCacheFWF) < ifelse(gradeLvlCode == "c", 1, 2) || forceReread == TRUE) { # ensure we have a full dataset of cache files
    runProcessing <- TRUE
  } else {
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "CivED")) { # cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    } else {
      # rebuild the file connections from the .meta serialized cache file using the stored fileFormats

      if (!is.null(cacheFile$dataListFF$school)) {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "t_")], cacheFile$dataListFF$student)
        schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "c_")], cacheFile$dataListFF$school)


        dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      } else {
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF), 1, 3)) == paste0(gradeLvlCode, "t_")], cacheFile$dataListFF$student)

        dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
        dataListFF <- cacheFile$dataListFF

        dim0 <- cacheFile$dim0

        runProcessing <- FALSE
      }
    }
  } # end if(length(metaCacheFP)==0 || length(txtCacheFWF)<ifelse(gradeLvlCode=="c",1,2) || forceReread==TRUE)

  if (runProcessing == TRUE) {
    if (verbose == TRUE) {
      cat(paste0("Processing data for country ", dQuote(countryCode), ".\n"))
    }

    # delete the .meta file (if exists) before processing in case of error/issue
    if (length(metaCacheFP) > 0 && file.exists(metaCacheFP)) {
      file.remove(metaCacheFP)
    }

    # SCHOOL LEVEL===================================================
    cg <- unlist(fnames[paste0(gradeLvlCode, "c_")])[1]

    if (min(is.na(cg)) == 0) {
      schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "c_")])[1], ignore.case = TRUE)
      schoolDF1 <- read_sav(cg, user_na = TRUE)
      schoolDF1 <- UnclassCols(schoolDF1)

      colnames(schoolDF1) <- toupper(colnames(schoolDF1))

      # update or fix any strange value labels for the school dataset
      schoolDF1 <- fixCivED_DataLabels(schoolDF1, yearCode, countryCode)

      ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP, jkType = "JK1")
    } else {
      ffsch <- NULL
    }
    # ===============================================================

    # teacher level will be considered 'student' data for out data model as it's the base level
    # TEACHER LEVEL==================================================
    sg <- unlist(fnames[paste0(gradeLvlCode, "t_")])[1] # student file

    stuDF1 <- read_sav(sg, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(gradeLvlCode, "t_")])[1], ignore.case = TRUE)

    # update or fix any strange value labels for the teacher dataset
    stuDF1 <- fixCivED_DataLabels(stuDF1, yearCode, countryCode)

    ffstu <- writeTibbleToFWFReturnFileFormat(stuDF1, stuFP, jkType = "JK1")
    # ===============================================================

    if (!is.null(ffsch)) {
      schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      dataList <- list(student = studentLAF, school = schoolLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu, school = ffsch)

      # calculate the dim0 to store in the .meta file for fast retreival
      nrow0 <- nrow(stuDF1)
      ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName)))
      dim0 <- c(nrow0, ncol0)
    } else { # for 9th grade data since they don't have any school metrics

      studentLAF <- getFWFLaFConnection(stuFP, ffstu)

      dataList <- list(student = studentLAF) # ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu)

      # calculate the dim0 to store in the .meta file for fast retreival
      nrow0 <- nrow(stuDF1)
      ncol0 <- length(unique(c(ffstu$variableName)))
      dim0 <- c(nrow0, ncol0)
    }


    # save the cachefile to be read-in for the next call
    cacheFile <- list(
      ver = packageVersion("EdSurvey"),
      cacheFileVer = 6,
      ts = Sys.time(),
      dataListFF = dataListFF,
      dim0 = dim0
    )

    saveRDS(cacheFile, file.path(dataFolderPath, paste0(gradeLvlCode, "t", countryCode, yearCode, ".meta")))
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

# builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_ICCS <- function(fileFormat, defaultPV = "civ") {
  pvFields <- subset(fileFormat, nchar(fileFormat$Type) > 0) # type is identified in writeTibbleToFWFReturnFileFormat function
  constructs <- unique(pvFields$Type)
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

# returns the full country name based on the three letter country abbrev
getICCSCountryName <- function(countryCode) {
  cntryCodeDF <- data.frame(
    cntryCode = c(
      "aus", "aut",
      "bfl", "bfr", "bgr",
      "che", "chl", "col", "cyp", "cze",
      "deu", "dnk", "dom", "dnw",
      "eng", "esp", "est",
      "fin",
      "grc", "gtm",
      "hkg", "hun", "hrv",
      "idn", "irl", "isr", "ita",
      "kor",
      "lie", "ltu", "lux", "lva",
      "mex", "mlt",
      "nld", "nor", "nzl",
      "pol", "prt", "pry", "per",
      "rom", "rus",
      "svk", "svn", "swe",
      "tha", "twn",
      "usa"
    ),
    cntryName = c(
      "Australia", "Austria",
      "Belgium (Flemish)", "Belgium (French)", "Bulgaria",
      "Switzerland", "Chile", "Colombia", "Cyprus", "Czech Republic",
      "Germany", "Denmark", "Dominican Republic", "North Rhine-Westphalia (Germany)",
      "England", "Spain", "Estonia",
      "Finland",
      "Greece", "Guatemala",
      "Hong Kong (SAR)", "Hungary", "Croatia",
      "Indonesia", "Ireland", "Israel", "Italy",
      "Korea, Republic of",
      "Liechtenstein", "Lithuania", "Luxembourg", "Latvia",
      "Mexico", "Malta",
      "Netherlands", "Norway", "New Zealand",
      "Poland", "Portugal", "Paraguay", "Peru",
      "Romania", "Russian Federation",
      "Slovak Republic", "Slovenia", "Sweden",
      "Thailand", "Chinese Taipei",
      "United States"
    ),
    stringsAsFactors = FALSE
  ) # be sure to not create any factors::factors not needed at all

  lookupNames <- vector(mode = "character", length = length(countryCode))

  for (i in seq_along(countryCode)) {
    testName <- cntryCodeDF[cntryCodeDF$cntryCode == countryCode[i], "cntryName"]

    if (length(testName) == 0) { # test if no value found
      testName <- paste("(unknown) CountryCode:", countryCode[i])
    }

    lookupNames[i] <- testName
  }

  return(lookupNames)
}

exportCivEDICCSToCSV <- function(folderPath, exportPath, cntryCodes, dataSet, gradeLvl, ...) {
  sdfList <- readCivEDICCS(folderPath, cntryCodes, dataSet = dataSet, gradeLvl = gradeLvl, ...)

  if (inherits(sdfList, "edsurvey.data.frame.list")) {
    for (i in seq_along(sdfList$datalist)) {
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

# return back an updated spssDF with fixed/updated CivED data labels that are undefined/incorrectly labeled
# passing along the yearCode in case further CivED studies
fixCivED_DataLabels <- function(spssDF, yrCode, cntryCode) {
  # ensure CivED 1999 study
  if (tolower(yrCode) == "f2") {
    for (var in colnames(spssDF)) {
      # grab the labels and the names of the labels for this column
      tmpLbl <- attr(spssDF[[var]], "labels")
      tmpNames <- names(tmpLbl)

      # School Level Vars=====================================
      if (var == "BCVIOLEN") {
        tmpLbl <- c(tmpLbl, 0)
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:0>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BCYOURSC") {
        tmpLbl <- c(tmpLbl, 3)
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:3>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }
      # ======================================================

      # Student Level Vars====================================
      if (var == "BSGASOFT") {
        tmpLbl <- c(tmpLbl, 7)
        names(tmpLbl) <- c(tmpNames, "INVALID")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BSGDSCR") {
        tmpLbl <- c(tmpLbl, 0, 6, 7, 8, 9, 10)
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:0>",
          "<UNKNOWN LABEL:6>",
          "<UNKNOWN LABEL:7>",
          "<UNKNOWN LABEL:8>",
          "<UNKNOWN LABEL:9>",
          "<UNKNOWN LABEL:10>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BSGTALK") {
        tmpLbl <- c(tmpLbl, 7)
        names(tmpLbl) <- c(tmpNames, "INVALID")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BSGWATC") {
        tmpLbl <- c(1, 2, 3, 4, 5, 7, 8, 9)
        names(tmpLbl) <- c(
          "NO TIME",
          "LESS THAN 1 HOUR",
          "1 TO 2 HOURS",
          "3 TO 5 HOURS",
          "MORE THAN 5 HOURS",
          "INVALID",
          "NOT ADMIN.",
          "MISSING"
        )
        attr(spssDF[[var]], "labels") <- tmpLbl
        next
      }

      if (var %in% c("ITPART1", "ITPART2")) { # this applies to the 12th grade data as well
        tmpLbl <- c(tmpLbl, 4, 6)
        names(tmpLbl) <- c(
          tmpNames,
          "<NATIONALLY DEFINED RESPONSE:4>",
          "<NATIONALLY DEFINED RESPONSE:6>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }
      # ======================================================

      # Student Grade 12======================================
      if (var %in% c(
        "CS101", "CS102", "CS103", "CS105", "CS106", "CS107", "CS108", "CS109", "CS110", "CS111", "CS112", "CS113",
        "CS114", "CS115", "CS116", "CS117", "CS118", "CS119", "CS120", "CS121", "CS122", "CS124", "CS125", "CS126",
        "CS127", "CS128", "CS129", "CS130", "CS131", "CS132", "CS134", "CS135", "CS136", "CS137", "CS138", "CS140",
        "CS141", "CS142", "CS143"
      )) {
        tmpLbl <- c(tmpLbl, 7)
        names(tmpLbl) <- c(tmpNames, "INVALID")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c(
        "CS1P5", "CS1P6", "CS1P7", "CS1P8",
        "CS1R1", "CS1R2", "CS1R3", "CS1R4", "CS1R5",
        "CS1S1", "CS1S2", "CS1S3", "CS1S4", "CS1S5",
        "CS1T1", "CS1T2", "CS1T3", "CS1T4", "CS1T5",
        "CS4Q1", "CS4Q2", "CS4Q3", "CS4Q4", "CS4Q5"
      )) {
        tmpLbl <- c(tmpLbl, 0) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:0>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGADU1")) {
        tmpLbl <- c(tmpLbl, 0) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:0>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGADU2")) {
        tmpLbl <- c(tmpLbl, 3, 5, 6, 7) # add new code
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:3>",
          "<UNKNOWN LABEL:5>",
          "<UNKNOWN LABEL:6>",
          "INVALID"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGAS08", "CSGAS14", "CSGAS15")) {
        tmpLbl <- c(tmpLbl, 3) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:3>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGASOFT")) {
        tmpLbl <- c(tmpLbl, 0, 5, 6) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:0>", "<UNKNOWN LABEL:5>", "<UNKNOWN LABEL:6>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGBOOK")) {
        tmpLbl <- c(tmpLbl, 7) # add new code
        names(tmpLbl) <- c(tmpNames, "INVALID")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGBRN1")) {
        tmpLbl <- c(tmpLbl, 3) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:3>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGDSCR")) {
        tmpLbl <- c(tmpLbl, 6, 7, 8, 9, 10, 11) # add new code
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:6>",
          "<UNKNOWN LABEL:7>",
          "<UNKNOWN LABEL:8>",
          "<UNKNOWN LABEL:9>",
          "<UNKNOWN LABEL:10>",
          "<UNKNOWN LABEL:11>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGEDUF", "CSGEDUM")) {
        tmpLbl <- c(tmpLbl, 8, 9, 10, 11, 12, 13) # add new code
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:8>",
          "<UNKNOWN LABEL:9>",
          "<UNKNOWN LABEL:10>",
          "<UNKNOWN LABEL:11>",
          "<UNKNOWN LABEL:12>",
          "<UNKNOWN LABEL:13>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGLANG")) {
        tmpLbl <- c(tmpLbl, 4) # add new code
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:4>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSGNEWS")) {
        tmpLbl <- c(tmpLbl, 3, 4, 5) # add new code
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:3>",
          "<UNKNOWN LABEL:4>",
          "<UNKNOWN LABEL:5>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("CSNAT1", "CSNAT2", "CSNAT3", "CSNAT4")) {
        tmpLbl <- c(tmpLbl, 2, 3, 4) # add new code
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:2>",
          "<UNKNOWN LABEL:3>",
          "<UNKNOWN LABEL:4>"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }
      # ======================================================

      # TEACHER LEVEL VARS====================================
      if (var %in% c(
        "BTCASSE1", "BTCASSE2", "BTCASSE3", "BTCASSE4", "BTCASSE5", "BTCASSE6",
        "BTCBEMAT", "BTCCOPEX", "BTCCOPSU", "BTCMOAUT", "BTCMOMAT", "BTCMOOPP", "BTCMORES", "BTCMOTIM", "BTCSUBME", "BTCTEMET"
      )) {
        tmpLbl <- c(tmpLbl, 0, 7)
        names(tmpLbl) <- c(
          tmpNames,
          "<UNKNOWN LABEL:0>",
          "INVALID"
        )
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var %in% c("BTCMEM1", "BTCMEM2")) {
        tmpLbl <- c(tmpLbl, 7)
        names(tmpLbl) <- c(tmpNames, "INVALID")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BTGEDUC") {
        tmpLbl <- c(tmpLbl, 6)
        names(tmpLbl) <- c(tmpNames, "<UNKNOWN LABEL:6>")
        attr(spssDF[[var]], "labels") <- sort(tmpLbl)
        next
      }

      if (var == "BTGEDUC3") { # convert this to a continuous var like BTGEDUC1 by removing the unlabeled value of '1'
        tmpLbl <- c(98, 99)
        names(tmpLbl) <- c(
          "NOT ADMIN.",
          "MISSING"
        )
        attr(spssDF[[var]], "labels") <- tmpLbl
        next
      }

      if (var %in% c("BTGWEXP1", "BTGWEXP2", "BTGWEXP3", "BTTEASUB")) {
        tmpLbl <- c(
          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
          21, 22, 23, 24, 25, 26, 27, 28, 30, 31, 32, 33, 34, 40, 41, 42, 43, 46,
          47, 50, 51, 52, 53, 61, 62, 63, 64, 71, 72, 73, 74, 80, 98, 99
        )
        names(tmpLbl) <- c(
          "<SUBJECT 1>", "<SUBJECT 2>", "<SUBJECT 3>", "<SUBJECT 4>", "<SUBJECT 5>", "<SUBJECT 6>", "<SUBJECT 7>",
          "<UNKNOWN LABEL:8>", "<UNKNOWN LABEL:9>", "<UNKNOWN LABEL:10>", "<UNKNOWN LABEL:11>", "<UNKNOWN LABEL:12>",
          "<UNKNOWN LABEL:13>", "<UNKNOWN LABEL:14>", "<UNKNOWN LABEL:15>", "<UNKNOWN LABEL:16>", "<UNKNOWN LABEL:17>",
          "<UNKNOWN LABEL:18>", "<UNKNOWN LABEL:19>", "<UNKNOWN LABEL:20>", "<UNKNOWN LABEL:21>", "<UNKNOWN LABEL:22>",
          "<UNKNOWN LABEL:23>", "<UNKNOWN LABEL:24>", "<UNKNOWN LABEL:25>", "<UNKNOWN LABEL:26>", "<UNKNOWN LABEL:27>",
          "<UNKNOWN LABEL:28>", "<UNKNOWN LABEL:30>", "<UNKNOWN LABEL:31>", "<UNKNOWN LABEL:32>", "<UNKNOWN LABEL:33>",
          "<UNKNOWN LABEL:34>", "<UNKNOWN LABEL:40>", "<UNKNOWN LABEL:41>", "<UNKNOWN LABEL:42>", "<UNKNOWN LABEL:43>",
          "<UNKNOWN LABEL:46>", "<UNKNOWN LABEL:47>", "<UNKNOWN LABEL:50>", "<UNKNOWN LABEL:51>", "<UNKNOWN LABEL:52>",
          "<UNKNOWN LABEL:53>", "<UNKNOWN LABEL:61>", "<UNKNOWN LABEL:62>", "<UNKNOWN LABEL:63>", "<UNKNOWN LABEL:64>",
          "<UNKNOWN LABEL:71>", "<UNKNOWN LABEL:72>", "<UNKNOWN LABEL:73>", "<UNKNOWN LABEL:74>", "<UNKNOWN LABEL:80>",
          "NOT ADMIN.", "MISSING"
        )
        attr(spssDF[[var]], "labels") <- tmpLbl
        next
      }
      # ======================================================
    } # end for(var in cntryFixList[[cntryCode]]$vars)
  } # end if(tolower(yrCode)=="f2")

  return(spssDF)
}

# temporary helper function only used a few times to help identify CivED variables with missing/issue value labels
# will leave code here for reference only
testBadCivED_Labels <- function(spssDF, cntry) {
  cntryVal <- c()
  varNames <- c()
  lblVars <- c()
  dataVars <- c()

  # loop through each column to inspect the value labels to determine if they have defined all labels.
  # if the value labels don't match, then we want to include them for output
  for (col in names(spssDF)) {
    if (!is.null(attr(spssDF[[col]], "labels"))) {
      uVal <- sort(unique(spssDF[[col]]))
      lbls <- attr(spssDF[[col]], "labels")

      if (all(names(lbls) %in% c("missing", "not admin."))) {
        next
      }

      if (all(is.na(uVal))) {
        next
      }

      if (!all(uVal %in% lbls)) {
        cntryVal <- c(cntryVal, cntry)
        varNames <- c(varNames, col)
        lblVars <- c(lblVars, paste0(lbls, "=", names(lbls), collapse = "^"))
        dataVars <- c(dataVars, paste0(uVal, collapse = "^"))
      }
    }
  }

  return(data.frame(cntryVal, varNames, lblVars, dataVars))
}

# build the ICCS Datalist.
# dataSet is either 'teacher' or 'student'
# hasICCSData and hasCivEDData are logicals, only one of which will be TRUE
buildICCS_dataList <- function(dataSet, hasICCSData, hasCivEDData, stuLaf, stuFF, schLaf, schFF, tchLaf, tchFF) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  if (hasICCSData == TRUE && dataSet == "student") {
    dataList[["Student"]] <- dataListItem(
      lafObject = stuLaf,
      fileFormat = stuFF,
      levelLabel = "Student",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )

    if (!is.null(schLaf)) {
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
    }
  }

  if (hasICCSData == TRUE && dataSet == "teacher") {
    # use the student LAF and student FF here as a product of the processing, it's really the teacher data
    dataList[["Teacher"]] <- dataListItem(
      lafObject = stuLaf,
      fileFormat = stuFF,
      levelLabel = "Teacher",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )

    dataList[["School"]] <- dataListItem(
      lafObject = schLaf,
      fileFormat = schFF,
      levelLabel = "School",
      forceMerge = FALSE,
      parentMergeLevels = c("Teacher", "Teacher"),
      parentMergeVars = c("idcntry", "idschool"),
      mergeVars = c("idcntry", "idschool"),
      ignoreVars = names(schLaf)[names(schLaf) %in% names(stuLaf)],
      isDimLevel = FALSE
    )
  }

  if (hasCivEDData == TRUE && dataSet == "student") {
    dataList[["Student"]] <- dataListItem(
      lafObject = stuLaf,
      fileFormat = stuFF,
      levelLabel = "Student",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )

    if (!is.null(schLaf)) {
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
    }

    if (!is.null(tchLaf)) {
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

      dataList[["Student"]]$isDimLevel <- FALSE
    }
  }

  if (hasCivEDData == TRUE && dataSet == "teacher") {
    # use the student LAF and student FF here as a product of the processing, it's really the teacher data
    dataList[["Teacher"]] <- dataListItem(
      lafObject = stuLaf,
      fileFormat = stuFF,
      levelLabel = "Teacher",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )

    dataList[["School"]] <- dataListItem(
      lafObject = schLaf,
      fileFormat = schFF,
      levelLabel = "School",
      forceMerge = FALSE,
      parentMergeLevels = c("Teacher", "Teacher"),
      parentMergeVars = c("idcntry", "idschool"),
      mergeVars = c("idcntry", "idschool"),
      ignoreVars = names(schLaf)[names(schLaf) %in% names(stuLaf)],
      isDimLevel = FALSE
    )
  }

  return(dataList)
}
