#' @title Connect to ePIRLS Data
#'
#' @description Opens a connection to an ePIRLS data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the ePIRLS extracted SPSS (.sav) set of data
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}
#'                  or other online sources. Consult the \emph{ePIRLS User Guide} to help determine what countries
#'                  are included within a specific testing year of ePIRLS.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the \code{read_ePIRLS} function by
#'                    using existing read-in data already processed.
#'
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the ePIRLS international
#' database(s) using the \href{https://www.iea.nl/data-tools/repository}{IEA Study Data Repository}.
#' Data files require the SPSS data file (.sav) format using the default filenames.
#'
#' @details An ePIRLS \code{edsurvey.data.frame} includes three distinct data levels:
#'          \itemize{
#'               \item student
#'               \item school
#'               \item teacher
#'          }
#'
#'          When the \code{getData} function is called using an ePIRLS \code{edsurvey.data.frame},
#'          the requested data variables are inspected, and it handles any necessary data merges automatically.
#'          The \code{school} data always will be returned merged to the \code{student}
#'          data, even if only \code{school} variables are requested.
#'          If \code{teacher} variables are requested by the \code{getData} call, it will cause \code{teacher} data to be merged.
#'          A \code{student} can be linked to many \code{teachers}, which varies widely between countries.
#'
#' @details Please note that calling the \code{dim} function for an ePIRLS \code{edsurvey.data.frame} will result in
#'          the row count as if the \code{teacher} dataset was merged.
#'          This row count will be considered the \code{full data N} of the \code{edsurvey.data.frame}, even if no \code{teacher} data were included in an analysis.
#'          The column count returned by \code{dim} will be the count of unique column variables across all three data levels.
#'
#' @return
#' an \code{edsurvey.data.frame} for a single specified country or an
#' \code{edsurvey.data.frame.list} if multiple countries are specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, \code{\link{getData}}, and \code{\link{download_ePIRLS}}
#' @author Tom Fink
#'
#' @example man/examples/read_ePIRLS.R
#'
#' @importFrom haven read_sav
#' @import tibble
#' @export
read_ePIRLS <- function(path,
                        countries,
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
  if (!is.logical(forceReread)) {
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if (!is.logical(verbose)) {
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }

  # prepwork
  countries <- tolower(unique(countries))

  gradeLvl <- 4 # PIRLS is only 4th grade data
  gradeL <- "a"

  if (unlist(countries)[1] == "*") { # user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path,
      pattern = paste0(
        "^", gradeL, ".....(",
        paste(get_ePIRLSYearCodes(), collapse = "|"), ")\\.sav$"
      ), full.names = FALSE, ignore.case = TRUE
    ), 4, 6)))
  }

  # gather datafile listing::be sure we only pickup PIRLS years based on the yearcodes
  filenames <- list.files(path,
    pattern = paste0(
      "^", gradeL, "..", "(", paste(countries, collapse = "|"), ")(",
      paste(get_ePIRLSYearCodes(), collapse = "|"), ")", "\\.sav$"
    ), full.names = TRUE, ignore.case = TRUE
  )
  if (length(filenames) == 0) {
    stop(paste0(
      "Could not find any ePIRLS datafiles for countries ", paste(sQuote(countries), collapse = ", "),
      " in the following folder(s): ", pasteItems(dQuote(path)), "."
    ))
  }

  fSubPart <- tolower(substring(basename(filenames), 1, 8)) # includes a (4th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames), 7, 8))))

  procCountryData <- list()
  iProcCountry <- 0 # index counter for adding countries to the list

  for (yrCode in fileYrs) { # loop through all the year codes
    for (cntry in countries) {
      ePIRLSfiles <- list() # empty list

      ePIRLSfiles <- c(
        "acg", # school background
        "asa", # student achievement
        "asg", # student background
        "ash", # student home background (Special file::might not always be present)
        "asr", # within-country scoring reliability
        "ast", # student-teacher linkage
        "atg"
      ) # teacher background


      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop

      fnames <- sapply(ePIRLSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f, cntry, yrCode))] # added check here to only grab files for our specific grade level, country, and year
      }, simplify = FALSE)

      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g)) == 0
      }, simplify = TRUE)

      hasExcess <- sapply(fnames, function(h) {
        length(h) > 1
      }, simplify = TRUE)

      hasData <- sum(nchar(unlist(fnames))) > 0

      # test for any missing files::also check for any duplicate or multiple files
      if (sum(hasMissing) > 0 && hasData == TRUE) {
        stop(paste0("Missing ePIRLS datafile(s) for country ", dQuote(cntry), " ", pasteItems(ePIRLSfiles[hasMissing]), "."))
      }
      if (sum(hasExcess) > 0 && hasData == TRUE) {
        stop(paste0("Excess/duplicate ePIRLS datafile(s) for country ", dQuote(cntry), " ", pasteItems(ePIRLSfiles[hasExcess]), "."))
      }

      # test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (hasData == FALSE) {
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
            processedData <- do.call("process_ePIRLS", processArgs, quote = TRUE)
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
          processedData <- tryCatch(do.call("process_ePIRLS", processArgs, quote = TRUE),
            error = function(e) {
              stop(paste0(
                "Unable to process ePIRLS data for country code ", dQuote(cntry),
                " having year code ", dQuote(yrCode), " at folder path(s) ", pasteItems(dQuote(path)),
                ". Possible file corruption with source data.",
                " Error message: ", e
              ))
            }
          )
        }
      } # end if(hasData==TRUE)


      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL

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
      processedData$pvvars <- buildPVVARS_ePIRLS(processedData$dataListFF$student, defaultPV = "erea")
      processedData$subject <- c("Reading")
      processedData$year <- convert_ePIRLSYearCode(yrCode)
      processedData$assessmentCode <- "International"
      processedData$dataType <- "Student Data"
      processedData$gradeLevel <- "Grade 4"

      # achievment level cutpoints as defined by PIRLS documentation
      processedData$achievementLevels <- c("625", "550", "475", "400")
      names(processedData$achievementLevels) <- c("Advanced International Benchmark", "High International Benchmark", "Intermediate International Benchmark", "Low International Benchmark")

      processedData$omittedLevels <- c(
        "Multiple",
        NA,
        "N/A",
        "NA",
        "OMITTED",
        "OMITTED OR INVALID",
        "NOT REACHED",
        "INVALID RESPONSE",
        "NOT APPLICABLE",
        "LOGICALLY NOT APPLICABLE",
        "MISSING",
        "(Missing)"
      )


      processedData$survey <- "ePIRLS"
      processedData$country <- get_ePIRLSCountryName(cntry)

      procCountryData[[iProcCountry]] <- edsurvey.data.frame(
        userConditions = processedData$userConditions,
        defaultConditions = processedData$defaultConditions,
        dataList = build_ePIRLS_dataList(
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
convert_ePIRLSYearCode <- function(yrCode) {
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% "e1"] <- 2016

  return(yrTest)
}

get_ePIRLSYearCodes <- function() {
  # retrieve the TIMMS years based on their filenaming structure

  yrVals <- c("e1")
  names(yrVals) <- c(2016)

  return(yrVals)
}

# builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_ePIRLS <- function(fileFormat, defaultPV = "rrea") {
  pvFields <- subset(fileFormat, nchar(fileFormat$Type) > 0) # type is identified in writeTibbleToFWFReturnFileFormat function
  constructs <- unique(pvFields$Type)

  # drop the international benchmark contructs as they are not true plausible values, only discrete numerics
  constructs <- constructs[!grepl("^(e|r)ibm$", constructs, ignore.case = TRUE)]

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
process_ePIRLS <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
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
    mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuDF1) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
      ))
    }
    if (nrow(stuDF2) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
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
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nrow(stuDF1) != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
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
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
      if (nr != nrow(mm)) {
        stop(paste0(
          "Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
          "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
        ))
      }
      mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]
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
    mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

    if (nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0(
        "Failed consistency check for filetype ", sQuote("atg"), " country code ", sQuote(tolower(countryCode)), ". ",
        "Please report this to https://github.com/American-Institutes-for-Research/EdSurvey/issues for assistance."
      ))
    }

    teachFP <- gsub(".sav$", "\\.txt", unlist(fnames["atg"])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    # ===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)

    # perform any data label corrections/fixes
    ffstu <- ePIRLS_ValueLabelCorrection(ffstu, yearCode)

    # build data list and link metadata object=======================
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
      cat(paste0("Found cached data for country code ", dQuote(tolower(countryCode)), ".\n"))
    }
  } # end if(runProcessing==TRUE)

  return(list(
    dataList = dataList,
    dataListFF = dataListFF,
    dim0 = dim0
  ))
}

export_ePIRLSToCSV <- function(folderPath, exportPath, cntryCodes, ...) {
  sdfList <- read_ePIRLS(folderPath, cntryCodes, ...)

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


# get the full country name to aide the user, so they won't have to track them down.
# cntryCode should be the 3 character country code vector defined in the data filename scheme (e.g., usa = United States, swe = Sweden)
# if a match is not found, this funtion will return a character value indicating it is unknown '(unknown) CountryCode: xxx'
get_ePIRLSCountryName <- function(countryCode) {
  cntryCodeDF <- data.frame(
    cntryCode = c(
      "aad", "adu", "are",
      "can",
      "dnk",
      "geo",
      "irl", "isr", "ita",
      "nor",
      "prt",
      "sgp", "svn", "swe",
      "twn",
      "usa"
    ),
    cntryName = c(
      "Abu Dhabi, UAE", "Dubai, UAE", "United Arab Emirates",
      "Canada",
      "Denmark",
      "Georgia",
      "Ireland", "Israel", "Italy",
      "Norway (5th Grade)",
      "Portugal",
      "Singapore", "Slovenia", "Sweden",
      "Chinese Taipei",
      "United States"
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

# perform any label modifications here.  these should not be needed but we found an issue with the read_sav
# not correclty applying the labels to these specific variables
ePIRLS_ValueLabelCorrection <- function(fileFormat, yrCode) {
  if (tolower(yrCode) == "e1") { # for 2016 only
    valLbl <- fileFormat$labelValues[tolower(fileFormat$variableName) == "en11mtiml"] # the value label we are going to model after (correctly applied)

    if (nchar(valLbl) > 0) {
      varPattern <- "EN11(M|R|B|Z|T)TIM(L|S)"

      fileFormat$labelValues[grepl(varPattern, fileFormat$variableName, ignore.case = TRUE)] <- valLbl
    }
  }

  return(fileFormat)
}

# builds the PIRLS dataList object
build_ePIRLS_dataList <- function(stuLaf, stuFF, schLaf, schFF, tchLaf, tchFF) {
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
