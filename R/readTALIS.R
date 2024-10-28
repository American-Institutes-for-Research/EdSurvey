#' @title Connect to TALIS Data
#'
#' @description Opens a connection to a TALIS data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character vector to the full directory path(s) to the TALIS SPSS files (.sav)
#' @param countries a character vector of the country/countries to include using the
#'                  three-digit ISO country code. A list of country codes can be found in
#'                  the TALIS codebook, or you can use
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}.
#'                  You can use \code{*} to indicate all countries available.
#' @param isced a character value that is one of \code{a}, \code{b}, or \code{c}. \code{a} stands for \emph{Primary Level},
#'        \code{b} is for \emph{Lower Secondary Level}, and \code{c} is for \emph{Upper Secondary Level}. Default to \code{b}.
#' @param dataLevel a character value that indicates which data level to be used. It can be \code{teacher} (the default) or \code{school} (see details).
#' @param forceReread a logical value to force rereading of all processed data. Defaults to \code{FALSE}.
#'        Setting \code{forceReread} to be \code{TRUE} will cause \code{readTALIS} data to be reread and increase processing time.
#' @param verbose a logical value that will determine if you want verbose output while the function is running to indicate the progress.
#'        Defaults to \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the TALIS database using the OECD Repository (\url{https://www.oecd.org/education/talis.html}).
#'        If \code{dataLevel} is set to be \code{teacher}, it treats the teacher data file as the main dataset, and merges school data into teacher data for
#'        each country automatically. Use this option if wanting to analyze just teacher variables, or both teacher and school level variables together.
#'        If \code{dataLevel} is set \code{school}, it uses only the school data file (no teacher data will be available).
#'
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or
#'   an \code{edsurvey.data.frame.list} if multiple countries specified
#' @seealso \code{\link{getData}} and \code{\link{downloadTALIS}}
#' @author Paul Bailey, Tom Fink, and Trang Nguyen
#'
#' @example man/examples/readTALIS.R
#'
#' @references
#'  Organisation for Economic Co-operation and Development. (2018). \emph{TALIS 2018 technical report}. Retrieved from \emph{\url{https://www.oecd.org/education/talis/TALIS_2018_Technical_Report.pdf}}
#' @importFrom haven read_sav
#' @importFrom LaF laf_open_csv
#' @export
readTALIS <- function(path,
                      countries,
                      isced = c("b", "a", "c"),
                      dataLevel = c("teacher", "school"),
                      forceReread = FALSE,
                      verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  userOp2 <- options(scipen = 999)
  on.exit(options(userOp), add = TRUE)
  on.exit(options(userOp2), add = TRUE)

  dataLevel <- tolower(dataLevel)
  dataLevel <- match.arg(dataLevel)

  path <- normalizePath(path, winslash = "/") # to match IEA read-in function
  forceRead <- forceReread # to match IEA read-in function
  sdf <- list() # list to store edsurvey.data.frame elements
  icntry <- 0 # index to store edsurvey.data.frame elements
  if (missing(countries)) {
    stop("Missing argument ", sQuote("countries"), ".")
  }
  countries <- tolower(countries)
  if (missing(isced)) {
    isced <- "b" # get's defaulted to 'b' in the function call. missing==TRUE if parameter not specifed
  }
  isced <- tolower(isced)
  isced <- match.arg(isced)

  if (!isced %in% c("a", "b", "c")) {
    stop(paste0(
      "Argument ", sQuote("isced"), " only accepts three values: ",
      sQuote("a"), ", ", sQuote("b"), ", or ", sQuote("c"), "."
    ))
  }
  for (filepath in path) { # loop through the vector of path(s)
    filepath <- gsub("/$", "", filepath)
    filepath <- ifelse(grepl("[.][a-zA-Z]{1,4}$", filepath, perl = TRUE, ignore.case = TRUE), dirname(filepath), filepath)
    if (!dir.exists(filepath)) {
      stop(paste0("Cannot find ", sQuote("filepath"), "value in ", pasteItems(dQuote(filepath[!dir.exists(filepath)])), "."))
    }
    # Unzip files (usually this step is done in download function but TALIS data cannot be downloaded automatically)
    zFiles <- list.files(filepath, pattern = "SPSS.*\\.zip$", ignore.case = TRUE, full.names = FALSE)
    zFiles <- file.path(filepath, zFiles)
    for (z in zFiles) {
      lst <- unzip(z, list = TRUE)
      for (i in 1:nrow(lst)) {
        if (grepl("\\.sav$", basename(lst$Name[i]), ignore.case = TRUE)) {
          if (!file.exists(file.path(filepath, basename(lst$Name[i]))) || file.info(file.path(filepath, basename(lst$Name[i])))$size != lst$Length[i]) {
            if (verbose) {
              cat(paste0("Unzipping file ", sQuote(basename(lst$Name[i])), ".\n"))
            }
            unzip(z, files = lst$Name[i], exdir = filepath)
            if (basename(lst$Name[i]) != lst$Name[i]) {
              file.rename(file.path(filepath, lst$Name[i]), file.path(filepath, basename(lst$Name[i])))
            }
          }
        } # end if file is a sav file
      } # end for(i in 1:nrow(lst))
    } # end for(z in zFiles)

    # Process data files
    runProcessing <- FALSE
    metaCacheFP <- list.files(filepath, pattern = paste0(toupper(isced), "[0-9].*_ALL.meta"))
    if (length(metaCacheFP) == 0) {
      runProcessing <- TRUE
    } else {
      for (i in seq_along(metaCacheFP)) {
        cacheFile <- tryCatch(readRDS(file.path(filepath, unlist(metaCacheFP)[i])),
          error = function(err) {
            return(NULL)
          },
          warning = function(w) {
            return(NULL)
          }
        )
        if (is.null(cacheFile) || cacheMetaReqUpdate(cacheFile$cacheFileVer, "TALIS")) {
          if (i < length(metaCacheFP)) {
            next
          }
          runProcessing <- TRUE
        } else {
          break
        }
      }
    }
    if (runProcessing || forceRead) {
      if (verbose) {
        cat("Writing cache files.\n")
      }
      cacheFile <- processReturnFFormat(filepath, isced)
    }
    all_countries <- cacheFile$countryDict$cntry
    if (unlist(countries)[1] == "*") {
      countries <- all_countries
    }
    countries <- tolower(countries)
    year <- cacheFile$year
    if (year == 2008) {
      if (isced != "b") {
        warning(paste0("TALIS 2008 only provides data at lower secondary level, so ", sQuote("isced"), " is automatically set to ", dQuote("b"), "."))
        isced <- "b" # 2008 only has one isced level
      }
    }
    bad_countries <- countries[!countries %in% tolower(all_countries)]
    if (length(bad_countries) == 1) {
      stop("Missing TALIS data file(s) at isced level ", dQuote(convertISCEDcode(isced)), " for country ", pasteItems(dQuote(bad_countries)), ".")
    } else if (length(bad_countries) > 1) {
      stop("Missing TALIS data file(s) at isced level ", dQuote(convertISCEDcode(isced)), " for countries ", pasteItems(dQuote(bad_countries)), ".")
    }

    countries <- countries[countries %in% tolower(all_countries)]
    # Find cache files and construct an sdf
    for (cntry in countries) {
      processedData <- processCountry(filepath, cntry, isced, cacheFile)
      if (!runProcessing & !forceRead) {
        if (verbose) {
          cat("Found cached data for country code ", dQuote(cntry), ".\n")
        }
      }
      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL

      # Set up weights
      uklz <- unique(processedData$dataListFF$teacher[ , "pvWT"])
      uklz <- max(as.integer(uklz[uklz != "" & !is.na(uklz)]))
      weights <- list(tchwgt = list(jkbase = "trwgt", jksuffixes = as.character(1:uklz)))

      # 2. school-level weights
      uklz <- unique(processedData$dataListFF$school[ , "pvWT"])
      uklz <- max(as.integer(uklz[uklz != "" & !is.na(uklz)]))
      weights$schwgt <- list(jkbase = "srwgt", jksuffixes = as.character(1:uklz))

      if (dataLevel == "teacher") {
        attr(weights, "default") <- "tchwgt"
      } else if (dataLevel == "school") {
        attr(weights, "default") <- "schwgt"
      }

      processedData$weights <- weights
      processedData$pvvars <- NULL
      processedData$subject <- NULL
      processedData$year <- year
      processedData$assessmentCode <- "International"
      if (dataLevel == "teacher") {
        processedData$dataType <- "Teacher Data"
      } else {
        processedData$dataType <- "School Data"
      }
      processedData$gradeLevel <- convertISCEDcode(isced)
      processedData$achievementLevels <- NULL
      processedData$omittedLevels <- c("OMITTED", NA, "OMITTED OR VALID", "(Missing)")

      processedData$survey <- "TALIS"
      processedData$country <- cacheFile$countryDict$country.name[cacheFile$countryDict$cntry == toupper(cntry)]

      sdf[[cntry]] <- edsurvey.data.frame(
        userConditions = processedData$userConditions,
        defaultConditions = processedData$defaultConditions,
        dataList = buildTALIS_dataList(
          dataLevel,
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
        psuVar = NULL,
        stratumVar = NULL,
        jkSumMultiplier = 0.04, # see Reference (TALIS 2013 Chapter 9)
        reqDecimalConversion = FALSE
      )
    }
  } # end for(filepath in path)

  # Return output
  if (length(sdf) > 1) {
    return(edsurvey.data.frame.list(sdf))
  } else {
    return(sdf[[1]])
  }
}


# process each country in 2013
processCountry <- function(filepath, countryCode, isced, cacheFile) {
  teacherFP <- file.path(
    filepath,
    list.files(filepath,
      pattern = paste0(isced, "tg", countryCode, ".*\\.txt"),
      full.names = FALSE, ignore.case = TRUE
    )
  )
  schoolFP <- file.path(
    filepath,
    list.files(filepath,
      pattern = paste0(isced, "cg", countryCode, ".*\\.txt"),
      full.names = FALSE, ignore.case = TRUE
    )
  )
  if (length(teacherFP) > 1 | length(schoolFP) > 1) {
    warning(paste0("There is more than one relevant FWF file for ", countryCode, "."))
  }
  teacherFP <- teacherFP[1]
  schoolFP <- schoolFP[1]
  if (!file.exists(teacherFP) || !file.exists(schoolFP)) {
    stop(paste0("There are no text files of country ", countryCode, " at the isced level ", isced, "."))
  }
  teacherLAF <- getCSVLaFConnection(teacherFP, cacheFile$dataListFF$teacher)
  schoolLAF <- getCSVLaFConnection(schoolFP, cacheFile$dataListFF$school)
  dataList <- list(student = NULL, school = schoolLAF, teacher = teacherLAF)
  dataListFF <- cacheFile$dataListFF
  dataListMeta <- cacheFile$dataListMeta

  return(list(
    dataList = dataList,
    dataListFF = dataListFF,
    dataListMeta = dataListMeta
  ))
}
# This function reads in combined sav file and split it by country
processReturnFFormat <- function(filepath, isced) {
  fnames <- list.files(filepath,
    pattern = paste0("^", isced, ".*\\.sav"),
    full.names = FALSE, ignore.case = TRUE
  )
  year <- 0

  if (length(fnames) > 0) {
    testFN <- gsub(".sav$", "", fnames[1], ignore.case = TRUE)

    # make year assignment based on last two characters of filename
    if (grepl("t1$", testFN, ignore.case = TRUE)) {
      year <- 2008
    }
    if (grepl("t2$", testFN, ignore.case = TRUE)) {
      year <- 2013
    }
    if (grepl("t3$", testFN, ignore.case = TRUE)) {
      year <- 2018
    }
  }
  # SCHOOL LEVEL ============================
  cg <- grep("cg", fnames, value = TRUE, ignore.case = TRUE)
  if (length(cg) == 0) {
    stop("Missing TALIS data file(s) for school level at isced level of ", sQuote(isced), " in the path ", sQuote(filepath), ".")
  }
  schoolFP <- gsub("\\.sav", "\\.txt", cg) # basename for schoolFP for each country - replace INT with countryCode

  # reading in combined files for this isced level
  schoolDF <- read_sav(gsub("//", "/", paste0(filepath, "/", cg)))
  schoolDF <- UnclassCols(schoolDF) # remove haven column classes

  ffsch <- returnFF(schoolDF)

  # TEACHER LEVEL =============================
  tg <- grep("tg", fnames, value = TRUE, ignore.case = TRUE)
  if (length(tg) == 0) {
    stop("Missing TALIS data file(s) for teacher level at isced level of ", sQuote(isced), " in the path ", sQuote(filepath))
  }
  teacherFP <- gsub("\\.sav", "\\.txt", tg) # basename for teacherFP for each country - replace INT with countryCode

  # reading in combined files for this isced level
  teacherDF <- read_sav(gsub("//", "/", paste0(filepath, "/", tg)))
  teacherDF <- UnclassCols(teacherDF) # remove haven column classes

  fftch <- returnFF(teacherDF)

  # write countryDict with
  # 1. idcntry: numeric code
  # 2. cntry: iso alpha-code
  # 3. country.name: full country name

  if ("CNTRY" %in% fftch$variableName) { # TALIS 2013 has CNTRY variable
    temp <- strsplit(unlist(strsplit(fftch$labelValues[fftch$variableName == "IDCNTRY"], "\\^")), "=")
    options(stringsAsFactors = FALSE)
    countryDict <- data.frame(do.call("rbind", temp))
    colnames(countryDict) <- c("idcntry", "country.name")
    countryDict <- merge(countryDict, unique(teacherDF[ , c("IDCNTRY", "CNTRY")]), by.x = "idcntry", by.y = "IDCNTRY", all.x = FALSE, all.y = TRUE)
    colnames(countryDict) <- tolower(colnames(countryDict))
  } else { # TALIS 2008 does not have CNTRY variable
    temp <- strsplit(unlist(strsplit(fftch$labelValues[fftch$variableName == "IDCNTRY"], "\\^")), "=")
    options(stringsAsFactors = FALSE)
    countryDict <- data.frame(do.call("rbind", temp))
    colnames(countryDict) <- c("idcntry", "country.name")
    if (all(grepl(" - ", countryDict[ , "country.name"], ignore.case = TRUE))) {
      temp <- strsplit(countryDict[ , "country.name"], " - ")
      tempDF <- data.frame(do.call("rbind", temp))
      colnames(tempDF) <- c("cntry", "country.name")
      countryDict <- cbind(idcntry = countryDict$idcntry, tempDF)
      countryDict$idcntry <- as.numeric(countryDict$idcntry)
      countryDict <- countryDict[countryDict$idcntry %in% unique(teacherDF$IDCNTRY), ]
    }
  }

  # write out CSV/ FWF files
  all_countries <- countryDict$cntry
  for (cnti in 1:nrow(countryDict)) {
    cnt <- all_countries[cnti]
    schoolFPcntry <- gsub("//", "/", paste0(filepath, "/", gsub("int", toupper(cnt), schoolFP, ignore.case = TRUE)))
    tempTibble <- schoolDF[schoolDF$IDCNTRY %in% countryDict$idcntry[cnti], ]
    writeFWF(tempTibble, schoolFPcntry, ffsch)
  }

  for (cnti in 1:nrow(countryDict)) {
    cnt <- all_countries[cnti]
    teacherFPcntry <- gsub("//", "/", paste0(filepath, "/", gsub("int", toupper(cnt), teacherFP, ignore.case = TRUE)))
    tempTibble <- teacherDF[teacherDF$IDCNTRY == countryDict$idcntry[cnti], ]
    writeFWF(tempTibble, teacherFPcntry, fftch)
  }

  # toupper all variableNames in ff
  fftch$variableName <- toupper(fftch$variableName)
  ffsch$variableName <- toupper(ffsch$variableName)
  # cacheFile
  dataListFF <- list(student = NULL, school = ffsch, teacher = fftch)
  dataListMeta <- list()
  dataListMeta$student <- NULL
  dataListMeta$school <- NULL
  dataListMeta$teacher <- list(school = "idcntry;idschool")
  cacheFile <- list(
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 4,
    ts = Sys.time(),
    dataListFF = dataListFF,
    dataListMeta = dataListMeta,
    countryDict = countryDict,
    year = year
  )
  saveRDS(cacheFile, file.path(filepath, paste0(toupper(isced), year, "_ALL.meta")))
  return(cacheFile)
}


# re-customized from writeTibbleToFWFReturnFileFormat in readTIMSS
# changes: in weights and PVs
writeTALISTibbleToFWFReturnFileFormat <- function(spssDF, outF) {
  ff <- returnFF(spssDF)
  writeFWF(spssDF, outF, ff)
  return(ff)
}

returnFF <- function(spssDF) {
  if (!inherits(spssDF, "tbl_df")) stop("argument ", sQuote("spssDF"), " must be a tibble.")
  colInfo <- data.frame(names = colnames(spssDF), stringsAsFactors = FALSE)
  colInfo$format <- sapply(colInfo$names, function(z) {
    attributes(spssDF[[z]])$format.spss
  })
  colInfo$decimal <- as.numeric(ifelse(substr(colInfo$format, 1, 1) == "F", sapply(strsplit(colInfo$format, "\\."), function(x) {
    tail(x, 1)
  }), rep(NA, nrow(colInfo))))
  colInfo$decimal[is.na(colInfo$decimal) & !(tolower(colInfo$class) %in% "date")] <- 0 # dates are omitted based on SPSS class type so they are characters
  colInfo$multiplier <- as.integer(ifelse(is.na(colInfo$decimal), 1, 10^colInfo$decimal))
  colInfo$size <- gsub("[a-zA-Z]", "", sapply(strsplit(colInfo$format, "\\."), function(x) {
    head(x, 1)
  }))
  colInfo$size <- as.numeric(colInfo$size)

  # return output
  ff <- data.frame(variableName = colInfo$names, stringsAsFactors = FALSE)
  ff$Start <- c(1, 1 + cumsum(colInfo$size))[1:nrow(colInfo)]
  ff$End <- cumsum(colInfo$size)
  ff$Width <- colInfo$size
  ff$Decimal <- colInfo$decimal
  ff$multiplier <- colInfo$multiplier
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


  # for replicate weights it is the jackknife replicate weight number
  # for plausible value variables it is the index within the construct
  ff$pvWT <- sapply(colInfo$names, function(zz) {
    if (grepl("WGT[0-9]*", zz, ignore.case = TRUE)) {
      return(gsub("[^0-9]", "", zz))
    } else {
      return("")
    }
  })
  ff$Type <- ""
  # characters will have an N/A for their decimal value
  ff$dataType <- ifelse(ff$Decimal %in% 1:32, rep("numeric", nrow(colInfo)),
    ifelse(ff$Decimal %in% 0, rep("integer", nrow(colInfo)),
      rep("character", nrow(colInfo))
    )
  )
  ff$weights <- grepl("WGT$", colInfo$names, ignore.case = TRUE)

  # Add labels for CNTRY
  # Note: TALIS 2013: CSH/156001 is Shanghai, China (not in the labels); GEO, NZL, RUS
  exception <- c(
    "SHANGHAI, CHINA" = 156001,
    "GEORGIA" = 268,
    "NEW ZEALAND" = 554,
    "RUSSIA" = 643
  )
  for (ei in seq_along(exception)) {
    if (exception[ei] %in% spssDF$IDCNTRY) {
      if (!grepl(paste0(exception[ei], "="), ff$labelValues[ff$variableName == "IDCNTRY"])) {
        ff$labelValues[ff$variableName == "IDCNTRY"] <- paste0(ff$labelValues[ff$variableName == "IDCNTRY"], paste0("^", exception[ei], "=", names(exception)[ei]))
      }
    }
  }

  if ("CNTRY" %in% ff$variableName) {
    countryLookup <- unique(spssDF[ , c("CNTRY", "IDCNTRY")])
    replacementText <- as.character(countryLookup$CNTRY)
    names(replacementText) <- as.character(countryLookup$IDCNTRY)
    temp <- gsub(pattern = names(replacementText)[1], replacement = replacementText[1], x = ff$labelValues[ff$variableName == "IDCNTRY"], ignore.case = TRUE)
    if (length(replacementText) > 1) {
      for (replacementTexti in 2:length(replacementText)) {
        temp <- gsub(pattern = names(replacementText)[replacementTexti], replacement = replacementText[replacementTexti], x = temp, ignore.case = TRUE)
      }
    }
    ff$labelValues[ff$variableName == "CNTRY"] <- temp
  }
  # missing and labelled
  ff$labelled <- logical(nrow(ff))
  ff$missing <- ""
  missing_rules <- c(
    9, 99, 999, 9999, 99999, 999999,
    8, 98, 998, 9998, 99998, 999998,
    7, 97, 997, 9997, 99997, 999997,
    96, 996, 9996, 99996, 999996
  )
  for (ri in 1:nrow(ff)) {
    lv <- ff$labelValues[ri]
    keysTemp <- strsplit(
      unlist(strsplit(lv, "^", fixed = TRUE)),
      "="
    )
    keys <- sapply(keysTemp, function(k) k[1])
    keys <- keys[keys != ""]
    missing <- intersect(missing_rules, keys)
    ff$labelled[ri] <- length(missing) < length(keys)
    if (length(missing) != 0) {
      ff$missing[ri] <- paste0(missing, collapse = ";")
    }
  }
  return(ff)
}

# write out csv files from a tibble with variable names in order with fileFormat
writeFWF <- function(spssDF, outF, ff) {
  spssDF <- spssDF[ , ff$variableName]

  charIdx <- which(tolower(ff$dataType) == "character", arr.ind = TRUE)

  for (i in charIdx) {
    spssDF[[i]] <- gsub(",", ";", spssDF[[i]], fixed = TRUE)
    # fix for FULL-WIDTH comma which is used in Chinese (U+FF0C) which gets converted to regular comma on write out
    spssDF[[i]] <- gsub("\uFF0C", ";", spssDF[[i]], fixed = TRUE)
  }

  write.table(spssDF,
    file = outF,
    sep = ",", col.names = FALSE, na = "", row.names = FALSE,
    quote = FALSE
  ) # all numeric
}

# convert a one-letter isced code to a long name
convertISCEDcode <- function(isced) {
  isced <- tolower(isced)
  return(ifelse(isced == "a", "Primary (ISCED level 1)",
    ifelse(isced == "b", "Lower Secondary (ISCED level 2)",
      ifelse(isced == "c", "Upper Secondary (ISCED level 3)",
        ifelse(isced == "p", "TALIS-PISA Link",
          "N/A"
        )
      )
    )
  ))
}

# TALIS 2008 does not have country code
convertCountryCodeTALIS2008 <- function(countryCode) {
  # Source: TALIS IDB Analyer's Guide (Table 1.1 page 7)
  dict <- readRDS(system.file("extdata", "TALIS2008Dict.rds", package = "EdSurvey"))
  countryCode <- toupper(countryCode)
  if (!countryCode %in% dict$cntry) {
    warning(paste0(countryCode, " did not participate in TALIS 2008."))
    return(NA)
  } else {
    return(dict$country.name[dict$cntry == countryCode])
  }
}

# Opens a LaF connection of type CSV
getCSVLaFConnection <- function(datFP, ff) {
  LaF::laf_open_csv(datFP,
    column_types = ff$dataType,
    column_names = tolower(ff$variableName)
  )
}

# builds the TALIS dataList object
buildTALIS_dataList <- function(dataLevel, schoolLaf, schoolFF, teacherLaf, teacherFF) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  # teacher data is main dataset with schools below it if both datasets available
  if (dataLevel == "teacher") {
    dataList[["Teacher"]] <- dataListItem(
      lafObject = teacherLaf,
      fileFormat = teacherFF,
      levelLabel = "Teacher",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )

    dataList[["School"]] <- dataListItem(
      lafObject = schoolLaf,
      fileFormat = schoolFF,
      levelLabel = "School",
      forceMerge = FALSE,
      parentMergeLevels = c("Teacher", "Teacher"),
      parentMergeVars = c("idcntry", "idschool"),
      mergeVars = c("idcntry", "idschool"),
      ignoreVars = names(schoolLaf)[names(schoolLaf) %in% names(teacherLaf)],
      isDimLevel = FALSE
    )
  } else { # school level specified by user
    dataList[["School"]] <- dataListItem(
      lafObject = schoolLaf,
      fileFormat = schoolFF,
      levelLabel = "School",
      forceMerge = TRUE,
      parentMergeLevels = NULL,
      parentMergeVars = NULL,
      mergeVars = NULL,
      ignoreVars = NULL,
      isDimLevel = TRUE
    )
  }

  return(dataList)
}
