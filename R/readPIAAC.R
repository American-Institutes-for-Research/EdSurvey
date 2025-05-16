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
#'                  when loading all countries. Defaults to \code{12_14}.  Applicable only for Cycle 1 data.
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

  path <- normalizePath(path, winslash = "/") # to match IEA read-in function
  forceRead <- forceReread # to match IEA read-in function
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl = TRUE, ignore.case = TRUE), dirname(path), path)

  all_csvfiles <- list.files(path, pattern = "^prg.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
  all_codebookFiles <- list.files(path, pattern = "^.*international.*codebook[.]xlsx$", full.names = TRUE, ignore.case = TRUE)

  cycles <- sort(unique(tolower(substring(basename(all_csvfiles), 7, 8)))) #get the cycle file indicator (p1 = Cycle 1; p2 = Cycle 2)
  #get unique list of the ISO 3 country codes
  all_countries <- unique(tolower(substr(basename(all_csvfiles), 4, 6)))
  if (unlist(countries)[1] == "*") {
    all <- TRUE # keeping track of a "*" for usaOption
    countries <- all_countries
  } else {
    all <- FALSE
    countries <- tolower(unlist(countries))
  }
  
  #list index for building edsurvey.data.frame or edsurvey.data.frame.list object
  sdf <- list()
  iCountry <- 1 
  
  for(cy in cycles) {
    # Process file formats from excel codebook
    if(cy == "p1") {
      ffname <- list.files(path, "^processed-codebook[.]meta$", full.names = TRUE, ignore.case = TRUE)
    }else if (cy == "p2"){
      ffname <- list.files(path, "^cy2-processed-codebook[.]meta$", full.names = TRUE, ignore.case = TRUE)
    } else {
      stop(paste0("readPIAAC error, undefined Cycle Code detected: ", cy))
    }
    
    if (forceRead || length(ffname) == 0) {
      if (verbose) {
        cat("Processing codebook.\n")
      }
      if (cy == "p1") {
        ff <- processFileFormatReturnFF_Cycle1(path)
      }else if (cy == "p2") {
        ff <- processFileFormatReturnFF_Cycle2(path)
      }
      
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
        
        if (cy == "p1") {
          ff <- processFileFormatReturnFF_Cycle1(path)
        }else if (cy == "p2") {
          ff <- processFileFormatReturnFF_Cycle2(path)
        }
        
        forceRead <- TRUE
      } else {
        ff <- cacheFile$ff
      }
    } #end if (forceRead || !file.exists(ffname))
  
    # Set up PVs
    # Set up PVS ======================================================================
    # piaacAchievementLevelHelp returns a list of achievement level information for a specific round
    achievement <- piaacAchievementLevelHelp(cy) #pass the cycleCode here to filter achievementLevels
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
    for (cntry in countries) {
      # check usaOption only for cycle 1 data
      if (cntry == "usa" && cy == "p1") {
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
      processedData <- processCountryPIAAC(path, cntry, cy, ff, forceRead, verbose)
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
      processedData$year <- getPIAAC_CycleNumberFromCode(cy)
      processedData$assessmentCode <- "International"
      processedData$dataType <- "Adult Data"
      processedData$gradeLevel <- NULL
      processedData$achievementLevels <- achievement$achievementLevels
      
      #prepare the omitted levels for cycle 2 based on the defined missing values from the excel file format document
      if (tolower(cy) == "p2") {
        omitVals <- ff$missingSPSS
        omitVals <- unique(omitVals[!is.na(omitVals) & nchar(omitVals) > 0])
        
        retOL <- character()
        
        for(xVal in omitVals){
          xToken <- unlist(strsplit(xVal, "^", fixed = TRUE))
          xVal <- strsplit(xToken, "=", fixed = TRUE)
          
          for(i in length(xVal)){
            retOL <- c(retOL, paste(xVal[[i]][-1], collapse = "="))
          }
        }
        #assign unique list back for building edsurvey.data.frame
        processedData$omittedLevels <- unique(retOL)
        
      } else { # cycle 1 (p1) or others by default
        processedData$omittedLevels <- c(
          "(Missing)", "DON'T KNOW", "NOT STATED OR INFERRED", "VALID SKIP", "REFUSED",
          "DON'T KNOW/REFUSED", "NO RESPONSE", "NOT REACHED/NOT ATTEMPTED", "ALL ZERO RESPONSE", NA
        )
      }
      
      processedData$fileFormat <- ff
  
      processedData$survey <- "PIAAC"
      processedData$country <- countryDictPIAAC(cntry)
      processedData$jkSumMultiplier <- processedData$cacheFile$jkSumMultiplier
      
      sdf[[iCountry]] <- edsurvey.data.frame(
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
      
      iCountry <- iCountry + 1
    }
  } #end for(cy in cycles)
  
  if (length(sdf) > 1) {
    return(edsurvey.data.frame.list(sdf))
  } else {
    return(sdf[[1]])
  }
}

# @return:
# 1. dataList
# 2. cacheFile: jkSumMultiplier, reps, and jk method
processCountryPIAAC <- function(filepath, countryCode, cycleCode, ff, forceRead, verbose) {
  
  # If Country code is USA, special processing for cycle 1 only, else process regular
  if (any(grepl("usa", countryCode)) && cycleCode == "p1") {
    origCC <- countryCode
    date <- gsub("usa(.*)", "\\1", countryCode)
    if (date == "12_14") {
      txtPattern <- "^(prgusap1[.]txt|prgusap1_12_14[.]txt)$"
      metaPattern <- "^(jkusap1[.]meta|jkusa12_14[.]meta)$"
    }
    if (date == "17") {
      txtPattern <- "^Prgusap1_2017[.]txt$"
      metaPattern <- "^jkusa2017[.]meta$"
    }
    countryCode <- "usa"
  } else {
    origCC <- "none"
    
    txtPattern <- paste0(countryCode, cycleCode, "[.]txt$")
    metaPattern <- paste0(countryCode, ifelse(cycleCode == "p1", "", cycleCode), "[.]meta$")
  }
  
  #locate the files from the defined regex patterns based on the iso-code and cycle code 
  txtCacheFile <- list.files(filepath, pattern = txtPattern, full.names = TRUE, ignore.case = TRUE)
  metaCacheFile <- list.files(filepath, pattern = metaPattern, full.names = TRUE, ignore.case = TRUE)

  if (length(txtCacheFile) == 0 || length(metaCacheFile) == 0) {
    forceRead <- TRUE
  } else {
    cacheFile <- tryCatch(readRDS(metaCacheFile[1]),
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
    dataList$student <- getCSVLaFConnection(txtCacheFile[1], ff)
    cacheFile <- readRDS(metaCacheFile[1])
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
  if (any(grepl("usa", countryCode)) && cycleCode == "p1") {
    # Is the date for the US 17 or 12_14; note that 'date' and 'origCC' were defined earlier
    if (grepl("17", date)) {
      # usa 17
      fname <- list.files(filepath, pattern = "usap1.*2017[.]csv$", full.names = TRUE, ignore.case = TRUE)
      if (length(fname) > 1) {
        stop(paste0(sQuote(countryCode), ": there is more than one csv files."))
      }
      fname <- fname[1]
      # 17 is "|" delimited
      dat <- read.csv(fname, header = TRUE, sep = , "|", colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    } else {
      if (grepl("12_14", date)) {
        # usa 12
        fname <- list.files(filepath, pattern = "usa(p1|p1_12_14)[.]csv$", full.names = TRUE, ignore.case = TRUE)
        if (length(fname) > 1) {
          stop(paste0(countryCode, ": there is more than one csv files."))
        }
        fname <- fname[1]
        dat <- read.csv(fname, header = TRUE, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
      } else {
        stop(paste0(dQuote("usaOption"), " must either be ", sQuote("12_14"), ", or ", sQuote("17"), " not: ", sQuote(origCC)))
      } # end US cases
    }
  } else {
    # non-US cases
    fname <- list.files(filepath, pattern = paste0(countryCode, cycleCode, ".*[.]csv"), full.names = TRUE, ignore.case = TRUE)
    if (length(fname) > 1) {
      stop(paste0(countryCode, ": there is more than one csv files."))
    }
    
    fname <- fname[1]
    
    #Cycle 1 data uses ',' as the delimiter without quoted escaping
    #Cycle 2 data ues ';' as the delimiter with quoted escaping
    if (cycleCode == "p1"){
      dat <- read.csv(fname, header = TRUE, colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    } else if (cycleCode == "p2"){
      dat <- read.table(fname, header = TRUE, sep = ";", quote = "\"", colClasses = "character", na.strings = "", stringsAsFactors = FALSE)
    }
    
  } # end reading country csv file

  colnames(dat) <- toupper(colnames(dat))

  # checking whether any missing columns in the data file
  # replace with NAs
  missingcolumns <- ff$variableName[!(toupper(ff$variableName) %in% colnames(dat))]
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
        dat[[ci]] <- gsub(pattern = names(replv)[replvi], replacement = replv[replvi], x = dat[[ci]], ignore.case = TRUE)
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
  write.table(dat, file.path(dirname(fname), gsub("[.]csv$", ".txt", basename(fname), ignore.case = TRUE)), sep = ",", col.names = FALSE, na = "", row.names = FALSE)

  # return output
  dataList$student <- getCSVLaFConnection(
    file.path(dirname(fname), gsub("[.]csv", ".txt", basename(fname), ignore.case = TRUE)),
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
  
  vereps <- unique(dat[ , "VENREPS"])
  if (length(vereps) > 1) {
    warning("There is more than one variance method. Using the first one.")
    vereps <- vereps[1]
  }
  
  if (vemethodn == 2) { #JK2 - Jackknife 2 Method
    jkSumMultiplier <- 1.0
  } else if (vemethodn == 1) { #JK1 - Jackknife 1 Method
    jkSumMultiplier <- 79 / 80 # IDB uses reps = 80 for all countries
  } else if (vemethodn == 3) { #BRR - Balanced Repeated Replication (Not used in Cycle 1 or 2 for any countries)
    jkSumMultiplier <- 1/vereps
  } else if (vemethodn == 4) { #FAY - Balanced Repeated Replication w/ Fay's Adjustment (0.3 factor according to documentation)
    jkSumMultiplier <- 1/(vereps*((1-0.3)^2))
  }
  cacheFile <- list(
    jkSumMultiplier = jkSumMultiplier,
    method = vemethodn,
    reps = 80
  )
  
  jkCycleCode <- ifelse(cycleCode == "p1", "", cycleCode)
  
  # special process usa12_14 and usa17 cases
  if (origCC != "none") {
    jkPath <- file.path(dirname(fname), paste0("jk", tolower(origCC), jkCycleCode, ".meta"))
  } else {
    jkPath <- file.path(dirname(fname), paste0("jk", tolower(countryCode), jkCycleCode, ".meta"))
  }

  saveRDS(cacheFile, jkPath)
  
  return(list(
    dataList = dataList,
    cacheFile = cacheFile
  ))
}

# Reads in excel codebook to return a data.frame fileFormat
processFileFormatReturnFF_Cycle1 <- function(filepath) {
  
  ffname <- list.files(filepath, pattern = "codebook.*[.]xlsx$", ignore.case = TRUE, full.names = TRUE)
  ffname <- ffname[!grepl("cy\\d{1}", basename(ffname), ignore.case = TRUE)] #drop any future cycle codebook files (e.g., cy2, cy3)
  
  if (length(ffname) != 1 || !file.exists(ffname)){
    stop(paste0("The codebook Excel file does not exist, or found duplicate files. It is recommended that users use downloadPIAAC to get all necessary files for the database."))
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
  saveRDS(cacheFile, file.path(dirname(ffname), "processed-codebook.meta"))

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

#retrieves the saved/stored achievement levels 
piaacAchievementLevelHelp <- function(cycleCode) {
  
  dict <- readRDS(system.file("extdata", "PIAACAL.rds", package = "EdSurvey"))
  
  #filter to the cycle only
  dict <- dict[tolower(dict$cycleCode) == tolower(cycleCode), ]
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

#returns the Cycle Number From the Cycle Filename Code (p1 = Cycle 1; p2 = Cycle 2)
getPIAAC_CycleNumberFromCode <- function(cycleCode) {
  cycleCode <- tolower(cycleCode)
  
  if (cycleCode == "p1") {
    return("Cycle 1")
  }
  if (cycleCode == "p2") {
    return("Cycle 2")
  }
  
  return("Cycle Code Undefined")
}

# Reads in excel codebook to return a data.frame fileFormat
processFileFormatReturnFF_Cycle2 <- function(filepath) {

  #retrive excel file full path
  xlFilePath <- list.files(filepath, pattern = "cy2.*int.*codebook.*[.]xlsx$", ignore.case = TRUE, full.names = TRUE)
  xlFilePath <- xlFilePath[!grepl("~$", basename(xlFilePath), fixed = TRUE)] #remove any temporary files
  
  #validate codebook path and contents
  if(length(xlFilePath) < 1){
    stop(paste0("Unable to locate the expected international codebook file with filename: ", sQuote("piaac-cy2-international-codebook.xlsx")))
  } else if (length(xlFilePath) > 1){
    stop(paste0("Multiple Cycle 2 codebook files detected, ensure only 1 file is present, and the file is closed."))
  }
  xlFilePath <- xlFilePath[1] #will only be length 1 here, but make sure
  
  xlWS <- readxl::excel_sheets(xlFilePath)
  
  if (xlWS[[1]] != "PUF") {
    stop(paste0("Error processing Cycle 2 codebook. The first worksheet is expected to be named: ", sQuote("PUF")))
  }
  
  #extract workbook data to list of data.frames for processing
  codebook <- list()
  for(x in xlWS) {
    codebook[[x]] <- read_excel(xlFilePath, sheet = x)
  }

  # retrieve initial variable information
  ff <- data.frame(
    variableName = toupper(codebook[[1]]$Variable),
    Labels = codebook[[1]]$Label,
    Width = codebook[[1]]$Width,
    Decimal = codebook[[1]]$Decimals,
    dataType = ifelse(codebook[[1]]$Decimals > 0 | codebook[[1]]$Level == "Ratio", "numeric", "integer"), #assume all numeric/integers. deeper analysis later
    labelValues = codebook[[1]]$`Value Scheme Detailed`,
    missingSAS = codebook[[1]]$`Missing Scheme Detailed: SAS`,
    missingSPSS = codebook[[1]]$`Missing Scheme Detailed: SPSS`,
    Domain = codebook[[1]]$Domain,
    Comment = codebook[[1]]$Comment,
    
    stringsAsFactors = FALSE
  )
  
  ff$replacement <- rep("", nrow(ff))

  for(i in seq_len(nrow(ff))) {
    #1:: process value labels
    lblVal <- ff$labelValues[i]
    
    #these are special case variables where the label values are so long, they are defined in another sheet in the workbook
    if (toupper(lblVal) %in% toupper(names(codebook)) || toupper(lblVal) %in% c("CNT_BRTH", "CNT_H")) {
      
      #special case where the Worksheet names are inconsistent from the sheetname
      if (toupper(lblVal) %in% c("CNT_BRTH", "CNT_H")) {
        lblVal <- "CNT_H_BRTH"
      }
      
      wsIdx <- which(toupper(lblVal) == toupper(names(codebook))) #get index to avoid upper/lower naming issues
      ws <- codebook[[wsIdx]] #get the defined worksheet which the name corresponds to
      
      iVal <- ws[[1]] #first col
      iLbl <- ws[[2]] #second col
      
      #create the EdSurvey defined labelValue string
      ff$labelValues[i] <- paste(iVal, iLbl, sep = "=", collapse = "^")
      
      #test the datatype
      if (!isAllNumeric(iVal)) {
        ff$dataType[i] <- "character"
      }
    } else if (grepl(".", lblVal)) {
      iTokens <- strsplit(lblVal, ";")[[1]]
      iTokens <- strsplit(iTokens, ":")
      
      keep <- numeric()
      for(ii in seq_along(iTokens)){
        if(length(iTokens[[ii]]) == 1) {
          iTokens[[ii - 1]][2] <- paste(iTokens[[ii - 1]][2], iTokens[[ii]][1], sep = ";")
        } else {
          keep <- c(keep, ii)
        }
      }
      
      #drop tokens that are not key/value prairs
      iTokens <- iTokens[keep]
      
      iVal <- trimws(unlist(lapply(iTokens, function(x){x[1]})))
      iLbl <- trimws(unlist(lapply(iTokens, function(x){paste0(x[-1], sep = "", collapse = "=")})))
      
      #create the EdSurvey defined labelValue string
      ff$labelValues[i] <- paste(iVal, iLbl, sep = "=", collapse = "^")
      
      #test the datatype based on the labeling
      if (!isAllNumeric(iVal)) {
        ff$dataType[i] <- "character"
      }
    } else {
      ff$labelValues[i] <- "" #no value labels defined
    }
    
    #2:: process SAS Missings
    sasVals <- ff$missingSAS[i]
    if (grepl(".", sasVals)) {
      iTokens <- strsplit(sasVals, ";")[[1]]
      iTokens <- strsplit(iTokens, ":")
      
      iVal <- trimws(unlist(lapply(iTokens, function(x){x[1]})))
      iLbl <- trimws(unlist(lapply(iTokens, function(x){paste(x[-1], sep = "", collapse = ":")})))
      
      #create the EdSurvey defined labelValue string
      ff$missingSAS[i] <- paste(iVal, iLbl, sep = "=", collapse = "^")
    }
    
    #3:: process SPSS Missings
    spssVals <- ff$missingSPSS[i]
    if (grepl(".", spssVals)) {
      iTokens <- strsplit(spssVals, ";")[[1]]
      iTokens <- strsplit(iTokens, ":")
      
      iVal <- trimws(unlist(lapply(iTokens, function(x){x[1]})))
      iLbl <- trimws(unlist(lapply(iTokens, function(x){paste(x[-1], sep = "", collapse = ":")})))
      
      #remove 'Sysmis' items, treat them as R 'NA' values
      iLbl <- iLbl[!(tolower(iVal) %in% c("sysmis"))] #do label first to correctly evaluate before iVal is adjusted!
      iVal <- iVal[!(tolower(iVal) %in% c("sysmis"))]
      
      #create the EdSurvey defined labelValue string
      if(length(iVal) > 0) {
        ff$missingSPSS[i] <- paste(iVal, iLbl, sep = "=", collapse = "^")
      } else {
        ff$missingSPSS[i] <- ""
      }
      
      #update the valueLabels with the SPSS missing values to complete the full set of value labels
      if(grepl(".", ff$labelValues[i]) && grepl(".", ff$missingSPSS[i])) {
        ff$labelValues[i] <- paste0(ff$labelValues[i], "^", ff$missingSPSS[i])
      } else if(!grepl(".", ff$labelValues[i]) && grepl(".", ff$missingSPSS[i])) { #only missings are categorized
        ff$labelValues[i] <- ff$missingSPSS[i]
      }
    }
    
    #4:: determine the SAS missing codes to SPSS numeric codes for replacements
    if (grepl(".", spssVals)) {
      
      sasTokens <- strsplit(sasVals, ";")[[1]]
      sasTokens <- strsplit(sasTokens, ":")
      
      sasVals <- trimws(unlist(lapply(sasTokens, function(x){x[1]})))
      sasLbls <- trimws(unlist(lapply(sasTokens, function(x){x[2]})))
      
      spssTokens <- strsplit(spssVals, ";")[[1]]
      spssTokens <- strsplit(spssTokens, ":")
      
      spssVals <- trimws(unlist(lapply(spssTokens, function(x){x[1]})))
      spssLbls <- trimws(unlist(lapply(spssTokens, function(x){x[2]})))
      
      #where to store to replacement definitions from the SAS missing codes to the SPSS codes
      xSASVal <- character()
      xSPSSVal <- character()
      
      for(ii in seq_along(sasVals)){
        if (sasVals[ii] != ".") {
          zLbl <- sasLbls[ii]
          zMatch <- which(tolower(spssLbls) == tolower(zLbl), arr.ind = TRUE)
          
          if (length(zMatch) > 0) {
            xSASVal <- c(xSASVal, sasVals[ii])
            xSPSSVal <- c(xSPSSVal, spssVals[zMatch[1]])
          }
        }
      }#end for(ii in seq_along(sasVals))
      
      if (length(xSASVal) > 0) {
        ff$replacement[i] <- paste(xSASVal, xSPSSVal, sep = "=", collapse = "^")
      }
      
    #5:: cleanup duplicate label value entries (after SPSS 'missings' included)
      if (grepl(".", ff$labelValues[i])){
        
        iTokens <- strsplit(ff$labelValues[i], "^", fixed = TRUE)[[1]]
        iTokens <- strsplit(iTokens, "=", fixed = TRUE)
        
        iVals <- trimws(unlist(lapply(iTokens, function(x){x[1]})))
        iLbls <- trimws(unlist(lapply(iTokens, function(x){x[2]})))
        
        if (anyDuplicated(iVals) > 0){
          iVals <- unique(iVals)
          zLbls <- character()
          for(x in iVals){
            z <- iLbls[iVals == x]
            
            zLbls <- c(zLbls, z[1]) #grab only the first instance of the label
          }
          
          ff$labelValues[i] <- paste(iVals, zLbls, sep = "=", collapse = "^")
        }
      }
    
    }#end if (grepl(".", spssVals)) {
    
  } #end for(i in seq_along(nrow(ff)))

  # plausible value
  ff$Type <- sapply(ff$variableName, function(v) {
    if (grepl("^pv.*[0-9]", v, ignore.case = TRUE)) {
      return(gsub("(pv)|[0-9]", "", v, ignore.case = TRUE))
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
  saveRDS(cacheFile, file.path(dirname(xlFilePath), "cy2-processed-codebook.meta"))
  
  # return
  return(ff)
}
