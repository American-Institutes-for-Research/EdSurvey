#' @title Connect to PIAAC Data
#'
#' @description Opens a connection to a PIAAC data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value to the full directory path to the PIAAC .csv
#'             files and Microsoft Excel codebook
#' @param countries a character vector of the country/countries to include
#'                  using the three-digit ISO country code. A list of country
#'                  codes can be found in the PIAAC codebook or
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}.
#'                  If files are downloaded using \code{\link{downloadPIAAC}},
#'                  a country dictionary text file can be
#'                  found in the filepath. You can use \code{*} to indicate
#'                  all countries available. For the \code{usa}, the year must
#'                  be specified using: \code{usa12_14} or  \code{usa17}.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    Defaults to \code{FALSE}.
#'                    Setting \code{forceReread} to be \code{TRUE} will cause
#'                    PIAAC data to be reread and increase the processing time.
#' @param verbose a logical value that will determine if you want verbose
#'                output while the function is running to indicate the progress.
#'                Defaults to \code{TRUE}.
#' @param usaOption a character value of \code{12_14} or \code{17} that specifies
#'                  what year of the USA survey should be used when loading all countries by
#'                  using \code{*} in the \code{countries} argument. This will only make a difference
#'                  when loading all countries. Defaults to \code{12_14}.
#'
#'
#' @details
#' Reads in the unzipped .csv files downloaded from the PIAAC dataset using
#' the OECD repository (\url{https://www.oecd.org/skills/piaac.html}). Users can use
#' \code{\link{downloadPIAAC}} to download all required files automatically.
#'
#' @return
#' an \code{edsurvey.data.frame} for a single specified country or
#' an \code{edsurvey.data.frame.list} if multiple countries specified
#' @seealso \code{\link{getData}} and \code{\link{downloadPIAAC}}
#' @author Trang Nguyen
#'
#' @example man/examples/readPIAAC.R
#' @references
#'  Organisation for Economic Co-operation and Development. (2016). \emph{Technical report of the survey of adult skills (PIAAC)} (2nd ed.). Paris, France: Author. Retrieved from \emph{\url{https://www.oecd.org/skills/piaac/PIAAC_Technical_Report_2nd_Edition_Full_Report.pdf}}
#' @importFrom readxl read_excel
#' @export
readPIAAC <- function(path,
                      countries,
                      forceReread = FALSE,
                      verbose = TRUE,
                      usaOption = "12_14") {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  userOp2 <- options(scipen = 999)
  on.exit(options(userOp), add = TRUE)
  on.exit(options(userOp2), add = TRUE)

  filepath <- normalizePath(path, winslash = "/") # to match IEA read-in function
  forceRead <- forceReread # to match IEA read-in function
  filepath <- ifelse(grepl("[.][a-zA-Z]{1,4}$", filepath, perl = TRUE, ignore.case = TRUE), dirname(filepath), filepath)

  csvfiles <- list.files(filepath, pattern = "^prg.*\\.csv$", full.names = FALSE, ignore.case = TRUE)

  all_countries <- unique(tolower(substr(csvfiles, 4, 6)))
  if (unlist(countries)[1] == "*") {
    all <- TRUE # keeping track of a "*" for usaOption
    countries <- all_countries
  } else {
    all <- FALSE
    countries <- tolower(unlist(countries))
  }

  # Process file formats from excel codebook
  ffname <- file.path(filepath, "processed-codebook.meta")
  if (forceRead || !file.exists(ffname)) {
    if (verbose) {
      cat("Processing codebook.\n")
    }
    ff <- processFileFormatReturnFF(filepath)
  } else {
    cacheFile <- tryCatch(readRDS(ffname),
      error = function(err) {
        return(NULL)
      }, warning = function(w) {
        return(NULL)
      }
    )
    if (is.null(cacheFile) || cacheMetaReqUpdate(cacheFile$cacheFileVer, "PIAAC")) {
      if (verbose) {
        cat("Processing codebook.\n")
      }
      ff <- processFileFormatReturnFF(filepath)
      forceRead <- TRUE
    } else {
      ff <- cacheFile$ff
    }
  }

  # Set up PVs
  # Set up PVS ======================================================================
  # piaacAchievementLevelHelp returns a list of achievement level information for a specific round
  achievement <- piaacAchievementLevelHelp(1) # currently only PIAAC round 1 has available data
  pvs <- list()
  pv_subset <- subset(ff,
    select = c("Type", "variableName"),
    ff$Type != ""
  )
  uniquePvTypes <- tolower(unique(pv_subset$Type))
  for (i in uniquePvTypes) {
    vars <- tolower(pv_subset$variableName[tolower(pv_subset$Type) == i])
    temp_list <- list(varnames = vars)
    checkregex <- sapply(achievement$regex, grepl, i, ignore.case = TRUE)
    subject <- achievement$subjects[which(checkregex)]
    temp_list$achievementLevel <- sort(achievement$achievementLevels[[subject]])
    pvs[[i]] <- temp_list
  }
  attr(pvs, "default") <- names(pvs)[1]

  # Process data for each country
  sdf <- list()
  for (cntry in countries) {
    # check usaOption
    if (cntry == "usa") {
      if (usaOption == "17") {
        cntry <- paste0(cntry, "17")
      } else {
        if (usaOption == "12_14") {
          cntry <- paste0(cntry, "12_14")
        } else {
          stop(paste0(dQuote("usaOption"), " must either be ", sQuote("12_14"), ", or ", sQuote("17"), " not: ", sQuote(usaOption)))
        }
      }
    }

    # process
    processedData <- processCountryPIAAC(filepath, cntry, ff, forceRead, verbose)
    processedData$userConditions <- list()
    processedData$defaultConditions <- NULL
    processedData$data <- processedData$dataList$student

    # Set up weights ===================================================================
    uklz <- processedData$cacheFile$reps
    weights <- list(spfwt0 = list(jkbase = "spfwt", jksuffixes = as.character(1:uklz)))
    attr(weights, "default") <- "spfwt0"

    processedData$weights <- weights
    processedData$pvvars <- pvs
    processedData$subject <- achievement$subjects
    processedData$year <- "Round 1"
    processedData$assessmentCode <- "International"
    processedData$dataType <- "Adult Data"
    processedData$gradeLevel <- "N/A"
    processedData$achievementLevels <- achievement$achievementLevels
    processedData$omittedLevels <- c(
      "(Missing)", "DON'T KNOW", "NOT STATED OR INFERRED", "VALID SKIP", "REFUSED",
      "DON'T KNOW/REFUSED", "NO RESPONSE", "NOT REACHED/NOT ATTEMPTED", "ALL ZERO RESPONSE", NA
    )
    processedData$fileFormat <- ff

    processedData$survey <- "PIAAC"
    processedData$country <- countryDictPIAAC(cntry)
    processedData$jkSumMultiplier <- processedData$cacheFile$jkSumMultiplier
    sdf[[cntry]] <- edsurvey.data.frame(
      userConditions = processedData$userConditions,
      defaultConditions = processedData$defaultConditions,
      dataList = buildPIAAC_dataList(
        processedData$dataList$student,
        processedData$fileFormat
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
      psuVar = ifelse(as.numeric(processedData$cacheFile$method) == 1, "JK1", "varunit"),
      stratumVar = ifelse(as.numeric(processedData$cacheFile$method) == 1, "JK1", "varstrat"),
      jkSumMultiplier = processedData$jkSumMultiplier,
      reqDecimalConversion = FALSE,
      validateFactorLabels = TRUE
    ) # toggled to TRUE for issue found with cntryid (#1848)
  }

  if (length(sdf) > 1) {
    return(edsurvey.data.frame.list(sdf, labels = countries))
  } else {
    return(sdf[[1]])
  }
}

# @return:
# 1. dataList
# 2. cacheFile: jkSumMultiplier, reps, and jk method
processCountryPIAAC <- function(filepath, countryCode, ff, forceRead, verbose) {
  # If Country code is USA, special processing, else regular
  if (grepl("usa", countryCode)) {
    origCC <- countryCode
    date <- gsub("usa(.*)", "\\1", countryCode)
    if (date == "12_14") {
      txtCacheFile <- list.files(filepath, pattern = paste0("prgusap1", ".*\\.txt$"), full.names = FALSE, ignore.case = FALSE)
      metaCacheFile <- list.files(filepath, pattern = paste0(origCC, "\\.meta$"), full.names = FALSE, ignore.case = TRUE)
    }
    if (date == "17") {
      txtCacheFile <- list.files(filepath, pattern = paste0("Prgusap1_2017", ".*\\.txt$"), full.names = FALSE, ignore.case = FALSE)
      metaCacheFile <- list.files(filepath, pattern = paste0(origCC, "\\.meta$"), full.names = FALSE, ignore.case = TRUE)
    }
    countryCode <- "usa"
  } else {
    origCC <- "none"
    txtCacheFile <- list.files(filepath, pattern = paste0(countryCode, ".*\\.txt$"), full.names = FALSE, ignore.case = TRUE)
    metaCacheFile <- list.files(filepath, pattern = paste0(countryCode, "\\.meta$"), full.names = FALSE, ignore.case = TRUE)
  }

  if (length(txtCacheFile) == 0 || length(metaCacheFile) == 0) {
    forceRead <- TRUE
  } else {
    cacheFile <- tryCatch(readRDS(file.path(filepath, metaCacheFile[1])),
      error = function(err) {
        forceRead <<- TRUE
      },
      warning = function(w) {
        forceRead <<- TRUE
      }
    )
  }

  dataList <- list()
  dataListFF <- list()

  if (!forceRead) {
    if (verbose) {
      cat(paste0("Found cached data for country code ", dQuote(countryCode), ".\n"))
    }
    dataList$student <- getCSVLaFConnection(file.path(filepath, txtCacheFile), ff)
    cacheFile <- readRDS(file.path(filepath, metaCacheFile[1]))
    return(list(
      dataList = dataList,
      cacheFile = cacheFile
    ))
  }
  if (verbose) {
    cat("Processing data for country code ", dQuote(countryCode), ".\n")
  }
  # Reading country csv file
  # If Country code is USA, special processing, else regular
  if (grepl("usa", countryCode)) {
    # Is the date for the US 17 or 12_14; note that 'date' and 'origCC' were defined earlier
    if (grepl("17", date)) {
      # usa 17
      fname <- list.files(filepath, pattern = paste0(countryCode, ".*2017\\.csv"), full.names = FALSE, ignore.case = TRUE)
      if (length(fname) > 1) {
        stop(paste0(sQuote(countryCode), ": there is more than one csv files."))
      }
      fname <- fname[1]
      # 17 is "|" delimited
      dat <- read.csv(file.path(filepath, fname), header = TRUE, sep = , "|", colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    } else {
      if (grepl("12_14", date)) {
        # usa 12
        fname <- list.files(filepath, pattern = paste0(countryCode, ".*1\\.csv"), full.names = FALSE, ignore.case = TRUE)
        if (length(fname) > 1) {
          stop(paste0(countryCode, ": there is more than one csv files."))
        }
        fname <- fname[1]
        dat <- read.csv(file.path(filepath, fname), header = TRUE, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
      } else {
        stop(paste0(dQuote("usaOption"), " must either be ", sQuote("12_14"), ", or ", sQuote("17"), " not: ", sQuote(origCC)))
      } # end US cases
    }
  } else {
    # non-US cases
    fname <- list.files(filepath, pattern = paste0(countryCode, ".*\\.csv"), full.names = FALSE, ignore.case = TRUE)
    if (length(fname) > 1) {
      stop(paste0(countryCode, ": there is more than one csv files."))
    }
    fname <- fname[1]
    dat <- read.csv(file.path(filepath, fname), header = TRUE, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
  } # end reading country csv file

  colnames(dat) <- toupper(colnames(dat))

  # checking whether any missing columns in the data file
  # replace with NAs
  missingcolumns <- setdiff(ff$variableName, colnames(dat))
  if (length(missingcolumns) > 0) {
    dat[ , missingcolumns] <- NA
  }
  dat <- dat[ , ff$variableName]

  # replace SAS missing values with SPSS values
  for (ci in 1:nrow(ff)) {
    if (!is.na(ff$replacement[ci]) && ff$replacement[ci] != "") {
      repl <- strsplit(unlist(strsplit(ff$replacement[ci], "^", fixed = TRUE)), "=")
      replv <- sapply(repl, function(x) {
        x[2]
      })
      names(replv) <- sapply(repl, function(x) {
        x[1]
      })
      for (replvi in seq_along(replv)) {
        dat[[ci]] <- gsub(pattern = names(replv)[replvi], replacement = replv[replvi], x = dat[[ci]])
      }
    }


    # there are some typos in csv files
    if (ff$dataType[ci] != "character") {
      temp <- dat[[ci]]
      suppressWarnings(temp <- as.numeric(temp))
      temp <- ifelse(grepl("NA", temp), NA, temp)
      dat[[ci]] <- temp
    }
  }
  # write out processed csv files, must use write.table as write.csv ALWAYS includes col.names even if col.names=FALSE is set
  write.table(dat, file.path(filepath, gsub("\\.csv$", ".txt", fname)), sep = ",", col.names = FALSE, na = "", row.names = FALSE)

  # return output
  dataList$student <- getCSVLaFConnection(
    file.path(filepath, gsub("\\.csv", ".txt", fname)),
    ff
  )
  dataListFF$student <- ff

  # calculate jkSumMultiplier
  # source: https://www.oecd.org/skills/piaac/PIAAC_Technical_Report_2nd_Edition_Full_Report.pdf
  # source: http://www.oecd.org/skills/piaac/PIACTOOLS_16OCT_for_web.pdf
  vemethodn <- unique(dat[ , "VEMETHODN"])
  if (length(vemethodn) > 1) {
    warning("There is more than one variance method. Using the first one.")
    vemethodn <- vemethodn[1]
  }
  if (vemethodn == 2) {
    jkSumMultiplier <- 1.0
  } else if (vemethodn == 1) {
    jkSumMultiplier <- 79 / 80 # IDB uses reps = 80 for all countries
  }
  cacheFile <- list(
    jkSumMultiplier = jkSumMultiplier,
    method = vemethodn,
    reps = 80
  )
  # special process usa12_14 and usa17 cases
  if (origCC != "none") {
    saveRDS(cacheFile, file.path(filepath, paste0("jk", tolower(origCC), ".meta")))
  } else {
    saveRDS(cacheFile, file.path(filepath, paste0("jk", tolower(countryCode), ".meta")))
  }

  return(list(
    dataList = dataList,
    cacheFile = cacheFile
  ))
}

# Reads in excel codebook to return a data.frame fileFormat
processFileFormatReturnFF <- function(filepath) {
  ffname <- list.files(filepath, pattern = "codebook.*\\.xls", ignore.case = TRUE, full.names = FALSE)
  ffname <- file.path(filepath, ffname)
  if (!file.exists(ffname) || length(ffname) == 0) {
    stop(paste0("The codebook Excel file does not exist. It is recommended that users use downloadPIAAC to get all necessary files for the database."))
  }
  codebook <- list()
  codebook$variable <- read_excel(ffname, sheet = 1)
  codebook$value <- read_excel(ffname, sheet = 2)

  # retrieve variable information
  ff <- data.frame(
    variableName = toupper(codebook$variable$Name),
    Labels = toupper(codebook$variable$Label),
    Width = codebook$variable$Width,
    Decimal = codebook$variable$Decimals,
    dataType = tolower(codebook$variable$Type), stringsAsFactors = FALSE
  )
  ff$dataType <- ifelse(grepl("^i", ff$dataType), "integer",
    ifelse(grepl("^n", ff$dataType), "numeric",
      ifelse(grepl("^s", ff$dataType), "character", NA)
    )
  )
  # retrieve value labels
  codebook$value$`Variable Name` <- toupper(codebook$value$`Variable Name`)
  codebook$value$`Value (SAS)` <- gsub("^\\.", "", codebook$value$`Value (SAS)`)
  ff$labelValues <- ""
  ff$replacement <- ""
  ff$missing <- ""
  for (vi in 1:nrow(ff)) {
    v <- ff$variableName[vi]
    dict <- codebook$value[codebook$value$`Variable Name` == v, ]
    if (nrow(dict) != 0) {
      ff$labelValues[vi] <- toupper(paste(dict$`Value (SPSS)`, dict$`Value Label`, sep = "=", collapse = "^"))
      repl <- dict[dict$`Value (SAS)` != dict$`Value (SPSS)`, c("Value (SAS)", "Value (SPSS)")]

      # replacement: CSV data files use SAS missing code
      if (nrow(repl) != 0) {
        ff$replacement[vi] <- paste(repl$`Value (SAS)`, repl$`Value (SPSS)`, sep = "=", collapse = "^")
      }

      # missing values
      ff$missing[vi] <- paste(dict$`Value (SPSS)`[dict$`Value Type` == "Missing"], collapse = ";")

      # labelled
      ff$labelled[vi] <- any(dict$`Value Type` == "Valid")
    } else {
      ff$labelled[vi] <- FALSE
    }
  }


  # plausible value
  ff$Type <- sapply(ff$variableName, function(zzz) {
    if (grepl("^pv.*[0-9]", zzz, ignore.case = TRUE)) {
      return(gsub("(pv)|[0-9]", "", zzz, ignore.case = TRUE))
    } else {
      return("")
    }
  })

  # suffix for pvs and weights
  ff$pvWt <- ""
  ff$pvWt <- mapply(function(v, t) {
    if (!grepl("^pv", v, ignore.case = TRUE)) {
      return("")
    } else {
      gsub(paste("pv", t, sep = "|"), "", v, ignore.case = TRUE)
    }
  }, ff$variableName, ff$Type)
  ff$pvWt[grepl("^SPFWT", ff$variableName, ignore.case = TRUE)] <- gsub("[^0-9]", "", ff$variableName[grepl("^SPFWT", ff$variableName, ignore.case = TRUE)])
  ff$pvWt[is.na(ff$pvWt)] <- ""
  ff$weights <- ff$pvWt == 0
  ff$pvWt[ff$pvWt == 0] <- ""

  # Process start and end (not used but put here to make it consistent with other read-in functions)
  ff$Start <- c(1, 1 + cumsum(ff$Width))[1:nrow(ff)]
  ff$End <- cumsum(ff$Width)

  # column types
  ff$dataType <- ifelse(ff$Width >= 10, gsub("integer", "numeric", ff$dataType), ff$dataType)
  # meta file
  cacheFile <- list(
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 3,
    ts = Sys.time(),
    ff = ff
  )
  saveRDS(cacheFile, file.path(filepath, "processed-codebook.meta"))

  # return
  return(ff)
}

countryDictPIAAC <- function(countryCode) {
  dict <- readRDS(system.file("extdata", "PIAACDict.rds", package = "EdSurvey"))
  ussr <- c("arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "tjk", "ukr", "uzb")
  if (countryCode %in% c("usa12_14", "usa17")) {
    date <- ifelse(countryCode == "usa12_14", "'12", "'17")
    countryCode <- "usa"
    return(paste(dict$Country[dict$CODE == toupper(countryCode)][1], date))
  } else {
    if (countryCode %in% ussr) {
      return(paste(dict$Country[dict$CODE == toupper(paste0("ussr-", countryCode))][1]))
    } else {
      return(paste(dict$Country[dict$CODE == toupper(countryCode)][1]))
    }
  }
}

piaacAchievementLevelHelp <- function(round) {
  dict <- readRDS(system.file("extdata", "PIAACAL.rds", package = "EdSurvey"))
  dict$level <- paste0("Proficiency ", dict$level)
  ret <- list()
  ret$subjects <- unique(dict$domain)
  ret$regex <- unique(dict$regex)
  ret$default <- ret$subjects[1]
  ret$achievementLevels <- list()
  for (s in ret$subjects) {
    ret$achievementLevels[[s]] <- dict$cutpoints[dict$domain == s]
    names(ret$achievementLevels[[s]]) <- ifelse(is.na(dict$level[dict$domain == s]), "Not Defined", dict$level[dict$domain == s])
  }
  return(ret)
}

# builds the PIRLS dataList object
buildPIAAC_dataList <- function(studentLaf, studentFF) {
  dataList <- list()

  # build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(
    lafObject = studentLaf,
    fileFormat = studentFF,
    levelLabel = "Student",
    forceMerge = TRUE,
    parentMergeLevels = NULL,
    parentMergeVars = NULL,
    mergeVars = NULL,
    ignoreVars = NULL,
    isDimLevel = TRUE
  )

  return(dataList)
}
