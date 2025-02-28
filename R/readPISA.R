#' @title Connect to PISA Data
#'
#' @description Opens a connection to a PISA data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character vector to the full directory path(s) to the
#'             PISA-extracted fixed-width files and SPSS control files (.txt).
#' @param database a character to indicate a selected database. Must be one of
#'                 \code{INT} (general database that most people use),
#'                 \code{CBA} (computer-based database in PISA 2012 only),
#'                 or \code{FIN} (financial literacy database in PISA 2012, 2018, and 2022. Note that `INT` needs to be used for PISA 2015 financial literacy data as it could be merged to the general database).
#'                 Defaults to \code{INT}.
#' @param countries a character vector of the country/countries to include using the
#'                  three-digit ISO country code. A list of country codes can be found
#'                  in the PISA codebook or \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}.
#'                  If files are downloaded using \code{\link{downloadPISA}},
#'                  a country dictionary text file can be
#'                  found in the filepath.
#' @param cognitive one of \code{none}, \code{score}, or \code{response}. Default
#'                  is \code{score}. The PISA database often has three student
#'                  files: student questionnaire, cognitive item
#'                  response, and scored cognitive item response. The first
#'                  file is used as the main student file with student
#'                  background information. Users can choose whether to
#'                  merge \code{score} or
#'                  \code{response} data into the main file or not (if \code{none}).
#' @param forceReread a logical value to force rereading of all processed data.
#'                    Defaults to \code{FALSE}. Setting \code{forceReread}
#'                    to be \code{TRUE} will cause PISA data to be reread and
#'                    increase processing time.
#' @param verbose a logical value that will determine if you want verbose
#'                output while the function is running to indicate progress.
#'                Defaults to \code{TRUE}.
#' @details
#' Reads in the unzipped files downloaded from the PISA database using the
#' OECD Repository (\url{https://www.oecd.org/pisa.html}). Users can use
#' \code{\link{downloadPISA}} to download all required files.
#' Student questionnaire files (with weights and plausible values) are used as
#' main files, which are then
#' merged with cognitive, school, and parent files (if available).
#'
#' The average first-time processing time for 1 year and one database for all
#' countries is 10--15 minutes. If \code{forceReread} is set
#' to be \code{FALSE}, the next time this function is called will take only
#' 5--10 seconds.
#'
#' For the PISA 2000 study, please note that the study weights are subject
#' specific. Each weight has different adjustment factors for reading, mathematics, and science
#' based on it's original subject source file.  For example, the \code{w_fstuwt_read} weight is associated with the reading
#' subject data file.  Special care must be used to select the correct weight based on your specific analysis.  See the OECD
#' documentation for further details.  Use the \code{showWeights} function to see all three student level subject weights:
#' \itemize{
#'   \item \strong{w_fstuwt_read} = Reading (default)
#'   \item \strong{w_fstuwt_scie} = Science
#'   \item \strong{w_fstuwt_math} = Mathematics
#' }
#'
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or
#'  an \code{edsurvey.data.frame.list} if multiple countries are specified
#' @seealso \code{\link{getData}} and \code{\link{downloadPISA}}
#' @author Tom Fink, Trang Nguyen, Paul Bailey, and Yuqi Liao
#'
#' @example man/examples/readPISA.R
#'
#' @references
#' Organisation for Economic Co-operation and Development. (2017). \emph{PISA 2015 technical report}. Paris, France: OECD Publishing. Retrieved from \emph{\url{ https://www.oecd.org/pisa/data/2015-technical-report.html}}
#'
#' @importFrom utils write.table
#' @export
readPISA <- function(path,
                     database = c("INT", "CBA", "FIN"),
                     countries,
                     cognitive = c("score", "response", "none"),
                     forceReread = FALSE,
                     verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  # Check that the arguments path, database, cognitive, forceReread, and verbose are valid  ======
  # database must be length one and upper, so force that
  database <- toupper(database)
  database <- match.arg(database)
  
  cognitive <- tolower(cognitive)
  cognitive <- match.arg(cognitive)
  
  if (!database %in% c("INT", "CBA", "FIN")) {
    stop("The argument ", sQuote("database"), " must be one of ", sQuote("INT"), ", ", sQuote("CBA"), ", or ", sQuote("FIN"), ".")
  }
  if (!is.character(countries)) {
    stop("The argument ", sQuote("countries"), " must be a character vector.")
  }
  if (!cognitive %in% c("score", "none", "response")) {
    stop("The argument ", sQuote("cognitive"), " must be one of ", sQuote("none"), ", ", sQuote("score"), ", or ", sQuote("response"), ".")
  }
  if (!is.logical(verbose)) {
    stop("The argument ", sQuote("verbose"), " must be a logical value.")
  }
  if (!is.logical(forceReread)) {
    stop("The argument ", sQuote("forceReread"), " must be a logical value.")
  }
  
  path <- normalizePath(path, winslash = "/") # to match IEA read-in function
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl = TRUE, ignore.case = TRUE), dirname(path), path)
  if (!all(dir.exists(path))) {
    stop(paste0("The argument ", sQuote("path"), " cannot be located ", pasteItems(path[!dir.exists(path)]), "."))
  }
  
  # Gather file names to be read in ====
  databasecode <- ifelse(database == "INT", "", paste0(database, "_"))
  sdf <- list() # list to contain all edsurvey.data.frame elements
  icntry <- 0 # index of each edsurvey.data.frame in the list
  for (filepath in path) { # loop through different data paths
    # check to see whether it's 2022, 2018, 2015, or older data
    #===2022 block
    if (length(list.files(filepath, pattern = "cy08.*\\.sav", ignore.case = TRUE, full.names = FALSE)) > 0) {
      year <- 2022
      if ("CBA" %in% database) {
        warning("Seperate CBA data is only available in 2012, switching to INT database.")
        database <- "INT"
      }
      
      # check for meta file
      runProcessing <- FALSE
      metaCacheFile <- list.files(filepath, pattern = paste0("^", database, ".*\\.meta$"), ignore.case = TRUE)
      if (length(metaCacheFile) < 1 || forceReread) {
        runProcessing <- TRUE
      } else {
        cacheFile <- tryCatch(readRDS(file.path(filepath, metaCacheFile[1])),
                              warning = function(w) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              },
                              error = function(err) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              }
        )
        if (!is.null(cacheFile)) {
          if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
            runProcessing <- TRUE
            cat("Cache files are outdated. Reprocessing PISA files.\n")
          }
        }
      }
      if (runProcessing) {
        # for testing you can just pass 'countries' as the argument, otherwise ensure (*) all countries
        # are reprocessed at the same time for release as only one copy of the fileFormat is cached.
        if (database == "INT") {
          cacheFile <- processPISA2015(filepath, verbose, "*", year)
        } else if (database == "FIN") {
          # process FIN data for 2018 All countries must be processed together as only one fileFormat object is cached to ensure consistancy across countries #YL: note that I didn't change it for 2022 yet as the PISA 2022 Fin Lit data is not yet available
          cacheFile <- processPISA2018_FIN(filepath, verbose, "*", year)
        } else {
          stop("Only INT and FIN databases are supported for year 2022")
        }
      } # end if (runProcessing)
      
      ff <- list(fileFormat = cacheFile$dict)
      all_countries <- cacheFile$countryDict$cnt[cacheFile$countryDict$available]
      if (any(countries == "*")) {
        countries <- all_countries
      }
      countries <- tolower(countries)
      
      if (database == "INT") {
        processedValue <- list(
          datbasename = "M_DAT_CY8_MS_CMB_STU",
          countries = countries
        )
      } else if (database == "FIN") {
        processedValue <- list(
          datbasename = "M_DAT_CY8_FIN",
          countries = countries
        )
      }
      
      # ====END 2018 Block====
    } else if (length(list.files(filepath, pattern = "cy07.*\\.sav", ignore.case = TRUE, full.names = FALSE)) > 0) {
      year <- 2018
      if ("CBA" %in% database) {
        warning("Seperate CBA data is only available in 2012, switching to INT database.")
        database <- "INT"
      }
      
      # check for meta file
      runProcessing <- FALSE
      metaCacheFile <- list.files(filepath, pattern = paste0("^", database, ".*\\.meta$"), ignore.case = TRUE)
      if (length(metaCacheFile) < 1 || forceReread) {
        runProcessing <- TRUE
      } else {
        cacheFile <- tryCatch(readRDS(file.path(filepath, metaCacheFile[1])),
                              warning = function(w) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              },
                              error = function(err) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              }
        )
        if (!is.null(cacheFile)) {
          if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
            runProcessing <- TRUE
            cat("Cache files are outdated. Reprocessing PISA files.\n")
          }
        }
      }
      if (runProcessing) {
        # for testing you can just pass 'countries' as the argument, otherwise ensure (*) all countries
        # are reprocessed at the same time for release as only one copy of the fileFormat is cached.
        if (database == "INT") {
          cacheFile <- processPISA2015(filepath, verbose, "*", year)
        } else if (database == "FIN") {
          # process FIN data for 2018. All countries must be processed together as only one fileFormat object is cached to ensure consistancy across countries
          cacheFile <- processPISA2018_FIN(filepath, verbose, "*", year)
        } else {
          stop("Only INT and FIN databases are supported for year 2018.")
        }
      } # end if (runProcessing)
      
      ff <- list(fileFormat = cacheFile$dict)
      all_countries <- cacheFile$countryDict$cnt[cacheFile$countryDict$available]
      if (any(countries == "*")) {
        countries <- all_countries
      }
      countries <- tolower(countries)
      
      if (database == "INT") {
        processedValue <- list(
          datbasename = "M_DAT_CY7_MS_CMB_STU",
          countries = countries
        )
      } else if (database == "FIN") {
        processedValue <- list(
          datbasename = "M_DAT_CY7_FIN",
          countries = countries
        )
      }
      
      # ====END 2018 Block====
      # ==== 2015 and prior Block====
    } else if (length(list.files(filepath, pattern = "cy6.*\\.sav", ignore.case = TRUE, full.names = FALSE)) > 0) {
      year <- 2015
      if ("CBA" %in% database) {
        warning("Seperate CBA data is only available in 2012.")
      }
      if ("FIN" %in% database) {
        warning("FIN data is not available in 2015.")
      }
      
      database <- "INT" # in PISA 2015, financial literacy and problem solving can be merged back to the overall population
      # check for meta file
      runProcessing <- FALSE
      metaCacheFile <- list.files(filepath, pattern = "\\.meta$", ignore.case = TRUE)
      if (length(metaCacheFile) < 0 | forceReread) {
        runProcessing <- TRUE
      } else {
        cacheFile <- tryCatch(readRDS(file.path(filepath, metaCacheFile[1])),
                              warning = function(w) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              },
                              error = function(err) {
                                runProcessing <<- TRUE
                                cat("Cache corrupt. Reprocessing PISA files\n")
                                return(NULL)
                              }
        )
        if (!is.null(cacheFile)) {
          if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
            runProcessing <- TRUE
            cat("Cache files are outdated. Reprocessing PISA files.\n")
          }
        }
      }
      if (runProcessing) {
        cacheFile <- processPISA2015(filepath, verbose, "*", year)
      }
      ff <- list(fileFormat = cacheFile$dict)
      all_countries <- cacheFile$countryDict$cnt[cacheFile$countryDict$available]
      if (any(countries == "*")) {
        countries <- all_countries
      }
      countries <- tolower(countries)
      processedValue <- list(
        datbasename = "M_DAT_CY6_MS_CMB_STU",
        countries = countries
      )
    } else { # end if (length(list.files(filepath, pattern="cy6.*\\.sav", ignore.case=TRUE, full.names=FALSE)) > 0)
      # not 2015
      if (database == "INT") {
        controlFilenames <- list.files(filepath, pattern = "SPSS_[a-z].*\\.txt", ignore.case = FALSE, full.names = FALSE)
      } else {
        controlFilenames <- list.files(filepath, pattern = paste0("SPSS_", database, ".*\\.txt"), ignore.case = FALSE, full.names = FALSE)
      }
      if (length(controlFilenames) == 0) {
        stop("Missing PISA Datafile(s) (", database, " database) in the path ", sQuote(path))
      }
      
      year <- unique(as.numeric(gsub("PISA", "", strsplit(controlFilenames, "_")[[1]][1])))
      
      if ("CBA" %in% database & !2012 %in% year) {
        warning("Seperate CBA data is only available in 2012.")
      }
      
      # validate data
      if (length(year) == 0) {
        stop("Please make sure to download SPSS syntax files on the OECD website. You can use ", sQuote("downloadPISA"), " to download and organize data.")
      }
      
      if (length(year) > 1) {
        stop("Found more than 2 years of data in the folder. Please separate different years of data in different folders.")
      }
      # files different for each year
      if (year %in% c(2006, 2009, 2012)) {
        filebasenames <- c("student", "cognitive", "score", "parent", "school")
      } else if (year == 2000) {
        filebasenames <- c("student_reading", "student_science", "student_math", "cognitive", "school")
      } else if (year == 2003) {
        filebasenames <- c("student", "cognitive", "school")
      } else if (year == 2015) {
        # Use Sav files
        filebasenames <- c("stu_qqq", "stu_cog", "stu_flt", "stu_cps", "stu_qtm", "sch_qqq")
      } else {
        stop(sQuote(year), " is not a valid year. Valid years for PISA are 2000, 2003, 2006, 2009, 2012, and 2015.")
      }
      
      # controlFilenames is a vector of all SPSS syntax files
      # now we need to attach each SPSS syntax file to its right label (one of `filebasenames` above)
      labelFiles <- c()
      for (i in seq_along(controlFilenames)) {
        labelFiles[i] <- filebasenames[which(sapply(filebasenames, function(x) {
          # we need to match against the whole string i.e. SPSS_student because cognitive files
          # might be in SPSS_cognitvie_student which also has the word "student"
          grepl(paste0("SPSS_", databasecode, x), controlFilenames[i], ignore.case = TRUE)
        }))]
      }
      # controlFilenames now is a named vector that contains all SPSS syntax files we will process
      # names of the vector definie file type (i.e. student or school type)
      names(controlFilenames) <- labelFiles
      if (all(!grepl("student", labelFiles))) {
        stop("Missing student SPSS syntax file. Since student is the main level, it is required that there must be at least one student file in the directory.")
      }
      
      # rearrange to make sure student files are the main ones
      # must ensure it matches the exact ordering defined in the 'filebasenames' to ensure proper merge ordering!!!
      labelFiles <- labelFiles[order(match(labelFiles, filebasenames))]
      # filter out some unnecessary files
      if (cognitive == "none") {
        labelFiles <- labelFiles[!labelFiles %in% c("score", "cognitive")]
      } else if (cognitive == "response") {
        labelFiles <- labelFiles[!labelFiles %in% c("score")]
        if (year %in% c(2000, 2003)) {
          cognitive <- "cognitive"
        }
      } else if (cognitive == "score") {
        if (!year %in% c(2000, 2003)) {
          labelFiles <- labelFiles[!labelFiles %in% c("cognitive")]
        } else {
          cognitive <- "cognitive"
        }
      }
      
      # controlFilenames are re-ordered per the labelFile ordering, the order must be correct for merging!
      controlFilenames <- controlFilenames[labelFiles]
      controlFilenames <- file.path(filepath, controlFilenames)
      # Process control files for each database ======================================
      # Return a list of name of data file and dict table
      metaCacheFile <- gsub("\\.txt", "\\.meta", controlFilenames)
      
      masterData <- lapply(controlFilenames, function(x) {
        metaCacheFile <- gsub("\\.txt", "\\.meta", x)
        if (!file.exists(metaCacheFile) || forceReread) {
          if (verbose) {
            cat(paste0("Processing SPSS control file ", sQuote(x), "\n"))
          }
          forceReread <<- TRUE
          return(suppressWarnings(readDict(x)))
        }
        cacheFile <- tryCatch(readRDS(metaCacheFile),
                              warning = function(w) {
                                forceReread <<- TRUE
                                cat(paste0(sQuote(x), " is a corrupt file. Reprocessing PISA files\n"))
                                return(suppressWarnings(readDict(x)))
                              },
                              error = function(err) {
                                forceReread <<- TRUE
                                cat(paste0(sQuote(x), " is a corrupt file. Reprocessing PISA files\n"))
                                return(suppressWarnings(readDict(x)))
                              }
        )
        if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
          if (verbose) {
            cat(paste0("Processing SPSS control file ", sQuote(x), "\n"))
          }
          forceReread <<- TRUE
          return(suppressWarnings(readDict(x)))
        } else {
          return(cacheFile)
        }
      })
      
      names(masterData) <- labelFiles
      # Setting key variables for linkage with student data
      masterData <- lapply(masterData, function(l) {
        l$linkid <- intersect(c("cnt", "schoolid", "stidstd"), tolower(l$dict$variableName))
        return(l)
      })
      
      # Reading the merged FF format file
      # ff is a list with: FFname = name of the FF file (to be used for base file name)
      #                    fileFormat = dataframe with all merged variables
      LafDictList <- masterData
      masterData <- NULL
      
      mergeFFmeta <- list.files(filepath, pattern = paste0("^M_FF_.*", database, ".*", cognitive, ".*\\.meta"), full.names = FALSE, ignore.case = TRUE)
      if (length(mergeFFmeta) == 0 || forceReread) {
        # Write merged files
        # ff is a list that contains:
        # 1. FFname = file name of the meta processed file format file (as followed)
        # 2. fileFormat = data.frame that includes information on all variables from student questionaire
        # school, and (possibly) parent files
        ff <- mergeFF(
          filepath = filepath, LafDictList = LafDictList,
          by = lapply(LafDictList[2:length(LafDictList)], function(l) {
            return(l$by)
          }),
          mergeSuffixes = cognitive
        )
      } else { # end if (length(mergeFFmeta) == 0 || forceReread)
        for (i in seq_along(mergeFFmeta)) {
          ff <- tryCatch(readRDS(file.path(filepath, mergeFFmeta[i])),
                         error = function(err) {
                           cat(err, ". Reprocessing the meta file. \n")
                           return(mergeFF(
                             filepath = filepath, LafDictList = LafDictList,
                             by = lapply(LafDictList[2:length(LafDictList)], function(l) {
                               return(l$by)
                             }),
                             mergeSuffixes = cognitive
                           ))
                         },
                         warning = function(w) {
                           cat(w, "Reprocessing the meta file \n")
                           return(mergeFF(
                             filepath = filepath, LafDictList = LafDictList,
                             by = lapply(LafDictList[2:length(LafDictList)], function(l) {
                               return(l$by)
                             }),
                             mergeSuffixes = cognitive
                           ))
                         }
          )
          if (cacheMetaReqUpdate(ff$cacheFileVer, "PISA")) {
            if (i < length(mergeFFmeta)) {
              next
            }
            ff <- mergeFF(
              filepath = filepath, LafDictList = LafDictList,
              by = lapply(LafDictList[2:length(LafDictList)], function(l) {
                return(l$by)
              }),
              mergeSuffixes = cognitive
            )
          } else {
            break
          }
        }
      } # end else for if (length(mergeFFmeta) == 0 || forceReread)
      # Process merge txt data file ====================================
      processedValue <- processMergeTxt(filepath, LafDictList, countries, ff, database, forceReread, verbose)
    } # end else for if(length(list.files(filepath, pattern="cy6.*\\.sav", ignore.case=TRUE, full.names=FALSE)) > 0)
    
    # From this step, requires:
    # (1) ff: a data.frame of all merged variables
    # (2) year
    # (3) database
    # (4) processedValue: (a) datbasename and (b) countries
    
    # Set up weights =================================================================
    jksuffix <- gsub("[^0-9]", "", ff$fileFormat$variableName[grepl("w_.*[0-9]$", ff$fileFormat$variableName,
                                                                    ignore.case = TRUE
    )])
    jksuffix <- unique(jksuffix)
    weight_var <- grep("w_fstuwt", tolower(ff$fileFormat$variableName), ignore.case = TRUE, value = TRUE)
    weight_var <- weight_var[!grepl("sum$", weight_var, ignore.case = TRUE)]
    weights <- list()
    for (w in weight_var) {
      if (year %in% c(2015, 2018, 2022)) {
        weights[[w]] <- list(jkbase = "w_fsturwt", jksuffixes = jksuffix)
      } else {
        weights[[w]] <- list(jkbase = paste0("w_fstr", gsub("w_fstuwt", "", w)), jksuffixes = jksuffix)
      }
    }
    attr(weights, "default") <- names(weights)[1]
    
    # achievementLevels
    suppressWarnings(achievement <- pisaAchievementHelp(year, database[1]))
    
    # Set up PVS ======================================================================
    pvs <- list()
    pv_subset <- subset(ff$fileFormat,
                        select = c("Type", "variableName"),
                        ff$fileFormat$Type != ""
    )
    uniquePvTypes <- tolower(unique(pv_subset$Type))
    default_list <- c()
    
    for (i in uniquePvTypes) {
      vars <- tolower(pv_subset$variableName[tolower(pv_subset$Type) == i])
      temp_list <- list(varnames = vars)
      checkregex <- sapply(achievement$regex, grepl, i, ignore.case = TRUE)
      subject <- achievement$subjects[which(checkregex)]
      if (!is.null(subject) && length(subject) > 0) {
        temp_list$achievementLevel <- sort(achievement$achievementLevels[[subject]])
        if (any(subject %in% achievement$default)) {
          default_list <- c(default_list, i)
        }
      } else {
        temp_list$achievementLevel <- NULL
      }
      pvs[[i]] <- temp_list
    } # end for (i in uniquePvTypes)
    
    if (length(default_list) > 0) {
      attr(pvs, "default") <- default_list[1]
    } else {
      attr(pvs, "default") <- names(pvs)[1]
    }
    
    # Read text files and return output ===========================
    if (!(year %in% c(2015, 2018, 2022))) {
      countryDict <- read.csv(file.path(filepath, paste0(database, "_all-countries.txt")), stringsAsFactors = FALSE)
    } else {
      countryDict <- cacheFile$countryDict
    }
    all_countries <- countryDict$cnt[countryDict$available]
    for (cntry in tolower(processedValue$countries)) {
      if (verbose) {
        if (cntry %in% tolower(all_countries)) {
          cat(paste0("Found cached data for country code ", dQuote(cntry), "\n"))
        } else {
          cat(paste0("Data for country code ", dQuote(cntry), " is not available for PISA ", year, "\n"))
          next
        }
      }
      datLaf <- catchCountryTxt(filepath, datname = paste0(processedValue$datbasename, "_", cntry, ".txt"), ff)
      icntry <- icntry + 1
      
      # specify the PSU/Stratum for the year dataset (defaults below, modify based on year)
      psuVar <- "var_unit"
      stratumVar <- "wvarstrr"
      
      if (year %in% c(2003, 2015, 2018, 2022)) {
        psuVar <- "unit"
      }
      if (year %in% c(2006, 2009)) {
        psuVar <- "randunit"
      }
      if (year %in% 2000) {
        psuVar <- ""
      }
      
      sdf[[icntry]] <- edsurvey.data.frame(
        userConditions = list(),
        defaultConditions = NULL,
        dataList = buildPISA_dataList(datLaf, ff$fileFormat),
        weights = weights,
        pvvars = pvs,
        subject = achievement$subjects,
        year = as.character(year),
        assessmentCode = "International",
        dataType = "Student Data",
        gradeLevel = "15 years old or above",
        achievementLevels = achievement$achievementLevels,
        omittedLevels = c("Invalid", "N/A", "Missing", "Miss", NA, "(Missing)", "NO RESPONSE", "INVALID", "VALID SKIP", "NOT APPLICABLE", "MISSING", "NOT REACHED"),
        survey = "PISA",
        psuVar = psuVar,
        stratumVar = stratumVar,
        jkSumMultiplier = 0.05, # this number is from PISA 2015 Technical Report Chapter 8 (in reference)
        country = convertCountryName(countryDict, cntry),
        validateFactorLabels = TRUE,
        reqDecimalConversion = FALSE
      )
    }
  } # end for(filepath in path)
  # Return output
  if (length(sdf) == 1) {
    return(sdf[[1]])
  } else if (length(sdf) > 1) {
    return(edsurvey.data.frame.list(sdf))
  } else {
    return(invisible(NULL))
  }
}

# HELPER FUNCTION ========================================
# Used when .txt files for data and SPSS controller are provided
# @return: a list of
#   dat = data file name
#   dict = a data.frame that stores fwf information
readDict <- function(filename) {
  # Expected outcome
  dict <- data.frame(
    "variableName" = character(0),
    "Start" = integer(0),
    "End" = integer(0),
    "Width" = integer(0),
    "Decimal" = integer(0),
    "Labels" = list(),
    "labelValues" = character(0),
    "Type" = character(0),
    "pvWt" = character(0),
    "dataType" = character(0),
    "weights" = character(0),
    stringsAsFactors = FALSE
  )
  # Read in spss control files
  fCon <- file(filename, encoding = "cp1252") # must supply encoding here as of R v4.3.0 or else grep operators will throw errors!
  on.exit(close(fCon))
  controlFile <- readLines(fCon)
  
  controlFile <- gsub("\t", " \t", controlFile)
  controlFile <- gsub("[^[:print:]]", "", controlFile) # remove unprintable characters
  controlFile <- trimws(controlFile, which = "both") # remove leading or ending whitespace
  # Note: the following lines need to be in order
  # Some syntax files uses single quote instead of double quote
  # Because later code use quote as a pattern to get labelValues, it's necessary
  # to replace relevant single quotes with double quotes
  controlFile <- gsub(" \'", " \"", controlFile) # replace single quote with double quote for later use
  controlFile <- gsub("\' ", "\" ", controlFile)
  controlFile <- gsub("^\'|\'$", "\"", controlFile)
  controlFile <- gsub("\'/", "\"/", controlFile)
  controlFile <- gsub("\'\\.", "\"\\.", controlFile)
  # end Note
  
  if (grepl("2003", filename)) {
    controlFile <- gsub(" / ", " /\n ", controlFile)
    controlFile <- unlist(strsplit(controlFile, "\n "))
  }
  controlFile <- gsub("\\(40\" ", "\\(40 ", controlFile)
  controlFile <- controlFile[controlFile != ""]
  # Get data file name
  i <- 1
  while (!grepl("file.*C:", controlFile[i], ignore.case = TRUE)) {
    i <- i + 1
  }
  # Some syntax files uses single quote instead of double quote. For the sake of convenience,
  # change all of them to double quote
  controlFile[i] <- gsub("\'", "\"", controlFile[i])
  
  # Find the name of the corresponding flat file given the SPSS syntax file
  datFname <- gsub("\"", "", strsplit(regmatches(controlFile[i], regexpr('(?<=").*?(?=")"', controlFile[i], perl = TRUE))[[1]], split = "\\\\")[[1]][3])
  # Some error in SPSS control file in 2009
  datFname <- gsub("PAQ09", "PAR09", datFname)
  datFname <- gsub("INT_cogn_2003.txt", "INT_cogn_2003_v2.txt", datFname)
  # Get variable name, fixed width and data types
  i <- i + 1
  # This while loop will only look at the variable name and fixed width section
  # of the SPSS syntax file
  while (grepl("-", controlFile[i])) {
    # The next five lines remove some of leading symbols to avoid lack
    # of consistency between SPSS syntax files of different years
    # These are based on trial and error, and manually looking at different syntax files
    controlFile[i] <- gsub("^/", "", controlFile[i])
    controlFile[i] <- gsub("\\(", " \\(", controlFile[i])
    controlFile[i] <- gsub("-", " - ", controlFile[i])
    tempSplit <- unlist(strsplit(controlFile[i], " |\t"))
    tempSplit <- tempSplit[tempSplit != ""]
    # Exception: in PISA 2006, format of control file is different
    if (length(tempSplit) <= 3) {
      temp <- unlist(strsplit(tempSplit[2], "-"))
      tempDict <- data.frame(
        "variableName" = toupper(tempSplit[1]),
        "Start" = as.integer(temp[1]),
        "End" = as.integer(temp[2]), stringsAsFactors = FALSE
      )
      
      if (length(tempSplit) == 2) {
        tempDict$Decimal <- 0
        tempDict$dataType <- "numeric"
      } else {
        tempDict$Decimal <- NA
        tempDict$dataType <- "character"
      }
    } else {
      tempDict <- data.frame(
        "variableName" = toupper(tempSplit[1]),
        "Start" = as.integer(tempSplit[2]),
        "End" = as.integer(tempSplit[4]), stringsAsFactors = FALSE
      )
      tempType <- gsub("\\(|\\)", "", tempSplit[5])
      
      if (grepl("^a", tempType, ignore.case = TRUE)) {
        tempDict$Decimal <- NA
        tempDict$dataType <- "character"
      } else if (grepl("^f", tempType, ignore.case = TRUE)) {
        tempDict$Decimal <- as.integer(unlist(strsplit(tempType, "\\,"))[2])
        tempDict$dataType <- "numeric"
      } else {
        tempDict$Decimal <- NA
        tempDict$dataType <- NA
      }
    }
    tempDict$Width <- tempDict$End - tempDict$Start + 1
    i <- i + 1
    dict <- rbind(dict, tempDict)
  } # end while (grepl("-", controlFile[i]))
  dict$Decimal[is.na(dict$Decimal)] <- 0
  # --- End getting variable names, fixed width and data types
  
  # Get variable formats
  j <- 1
  while (!grepl("^format", controlFile[j], ignore.case = TRUE) && j <= length(controlFile)) {
    j <- j + 1
  }
  while (j <= length(controlFile) && !grepl("exe.*\\.$", controlFile[j], ignore.case = TRUE)) {
    if (grepl("miss|val", controlFile[j], ignore.case = TRUE)) {
      j <- j + 1
      next # some SPSS files do not list all "format lines" in one place
    }
    controlFile[j] <- gsub("\\(", " \\(", controlFile[j])
    formatTemp <- strsplit(controlFile[j], " ")[[1]]
    formatTemp <- formatTemp[formatTemp != ""]
    if (grepl("format", controlFile[j], ignore.case = TRUE)) {
      fvarname <- c()
    }
    fvarname <- c(fvarname, grep("(format|\\()", formatTemp, invert = TRUE, value = TRUE, ignore.case = TRUE))
    index_of_to_format <- which(tolower(fvarname) == "to")
    if (length(index_of_to_format) != 0) {
      additional <- c()
      for (ii in index_of_to_format) {
        index_of_first <- which(dict$variableName == toupper(fvarname[ii - 1]))
        index_of_last <- which(dict$variableName == toupper(fvarname[ii + 1]))
        additional <- c(additional, dict$variableName[seq(from = index_of_first, to = index_of_last, by = 1)])
      }
      fvarname <- c(fvarname, additional)
      fvarname <- fvarname[tolower(fvarname) != "to"]
    }
    if (any(grepl("\\(f", formatTemp, ignore.case = TRUE))) {
      decimal <- gsub("[^0-9]", "", unlist(strsplit(grep("\\(f", formatTemp, ignore.case = TRUE, value = TRUE), "\\."))[2])
      dict$Decimal[dict$variableName %in% toupper(fvarname)] <- as.numeric(decimal)
      fvarname <- c()
      if (all(!grepl("^format", controlFile[(j + 1):length(controlFile)], ignore.case = TRUE))) {
        break
      }
    }
    j <- j + 1
  } # end loop for reading format
  dict$dataType[is.na(dict$dataType)] <- "numeric"
  
  # --- End checking formats
  # This column 'multiplier' is created to be consistent with the columns in fileFormat for other datasets.
  # It is not used for PISA because PISA is written out to csv, not fwf.
  dict$multiplier <- as.integer(ifelse(is.na(dict$Decimal), 1, 10^dict$Decimal))
  
  
  # Get variable labels
  # In syntax file of 2009 and earlier, there are formats. Need to skip
  while (!grepl("(variable label|var lab)", controlFile[i], ignore.case = TRUE)) {
    i <- i + 1
  }
  i <- i + 1
  # Get Variable labels
  while (!grepl("^\\.", controlFile[i]) && !grepl("^exe.*\\.", controlFile[i], ignore.case = TRUE) && !grepl("\\.$", trimws(controlFile[i - 1], "right"))) {
    tempSplit <- strsplit(controlFile[i], "\"")[[1]]
    dict[which(dict$variableName == toupper(trimws(tempSplit[1]))), "Labels"] <- tempSplit[2]
    i <- i + 1
  }
  # --- End getting variable labels
  
  # Get labelValues
  j <- i
  while (!grepl("(value label|val lab)", controlFile[j], ignore.case = TRUE)) {
    j <- j + 1
  }
  # first variable exception: does not start with "/"
  # Note: There can be multiple variable names on one line separated by space
  # Note: Exception in SPSS syntax: W_FSTR1 to W_FSTR80
  # Note: In syntax file of 2009 and earlier, need to skip VALUE LABELS
  # j <- j + 1
  varnamelist <- c()
  tempValues <- ""
  while (j <= length(controlFile)) {
    # This if finds the first line that define value label for each variable
    # In some syntax files, it's defined by explicitly saying "value label"
    # In some other files, it's defined with a leading "/"
    if (grepl("^/", controlFile[j]) || grepl("value label|val lab", controlFile[j], ignore.case = TRUE) || grepl("/$|\\.$", controlFile[j - 1])) {
      # Assign tempValues to varnamelist value labels
      if (length(varnamelist) != 0) {
        # Cleaning tempValues to return labelValues
        tempValues <- gsub("(^\\^|\\^\\.$|\\.\\^$|\\^\\^$)", "", tempValues)
        tempValues <- gsub("\\[[:punct:]]$", "", tempValues)
        dict[which(dict$variableName %in% varnamelist), "labelValues"] <- tempValues
      }
      tempValues <- ""
      varnamelist <- c()
      
      if (grepl("exe.*\\.$|^miss|^format|^\\.", controlFile[j], ignore.case = TRUE) || controlFile[j] == "") {
        while (!grepl("(value label|val lab)", controlFile[j], ignore.case = TRUE) && j <= length(controlFile)) {
          j <- j + 1
        }
        next
      }
      # in PISA 2003, variable name can appear on the same line as value labels
      testLine <- strsplit(gsub("^/", "", controlFile[j]), " |\t")[[1]]
      varnamelist <- intersect(toupper(testLine), c(toupper(dict$variableName), "TO"))
    }
    
    if (grepl("\"", controlFile[j])) {
      tempLine <- unlist(strsplit(controlFile[j], " |\t"))
      tempLine <- tempLine[tempLine != ""]
      splitIndex <- grep("\"", tempLine)[1] - 1
      if (!is.na(splitIndex) && splitIndex >= 1) {
        if (!toupper(tempLine[splitIndex]) %in% dict$variableName) {
          splitIndex <- splitIndex - 1
        }
        if (splitIndex >= 1) {
          varnamelist <- c(varnamelist, intersect(toupper(tempLine[1:splitIndex]), c(toupper(dict$variableName), "TO")))
        }
      }
      tempValuesC <- paste0(tempLine[(splitIndex + 1):length(tempLine)], collapse = " ")
      tempValuesC <- gsub("/$|\\.$", "", tempValuesC)
      if (grepl("^\"|^\'", tempValuesC)) { # keys are characters
        tempValuesC <- unlist(strsplit(tempValuesC, "\""))
        tempValuesC <- tempValuesC[!tempValuesC %in% c("", " ")]
        tempValuesC <- gsub("^\'|\'$", "", tempValuesC)
        tempValuesC <- paste(tempValuesC[seq(1, length(tempValuesC), 2)],
                             gsub("=", "", tempValuesC[seq(2, length(tempValuesC), 2)]),
                             sep = "=", collapse = "^"
        )
      } else { # keys are numeric
        tempValuesC <- paste(gsub("\"", "", gsub(" \"", "=", gsub("=", "", unlist(strsplit(tempValuesC, "\" "))))),
                             collapse = "^"
        )
      }
      tempValuesC <- trimws(tempValuesC)
      tempValues <- gsub("^\\^", "", paste(tempValues, tempValuesC, sep = "^"))
    } else {
      varnamelist <- c(varnamelist, intersect(toupper(unlist(strsplit(controlFile[j], " |\t"))), c(toupper(dict$variableName), "TO")))
    }
    varnamelist <- toupper(varnamelist[varnamelist != ""])
    # some spss syntax include "to" to indicate variable names in sequence
    # i.e. wt1 to wt3 to represent wt1, wt2, wt3
    index_of_to <- which(tolower(varnamelist) == "to") # might be more than 1
    if (length(index_of_to) != 0) {
      additional <- c()
      for (ii in index_of_to) {
        index_of_first <- which(dict$variableName == toupper(varnamelist[ii - 1]))
        index_of_last <- which(dict$variableName == toupper(varnamelist[ii + 1]))
        additional <- c(additional, dict$variableName[seq(index_of_first, index_of_last, 1)])
      }
      varnamelist <- c(varnamelist, additional)
      varnamelist <- varnamelist[tolower(varnamelist) != "to"]
    }
    j <- j + 1
  } # end while (j <= length(controlFile))
  dict$labelValues[is.na(dict$labelValues)] <- ""
  # --- End getting labelValues
  
  # missing and labels
  dict$labelled <- logical(nrow(dict))
  dict$missing <- ""
  missing_rules <- c(
    9, 99, 999, 9999, 99999, 999999,
    8, 98, 998, 9998, 99998, 999998,
    7, 97, 997, 9997, 99997, 999997,
    96, 996, 9996, 99996, 999996
  )
  for (ri in 1:nrow(dict)) {
    lv <- dict$labelValues[ri]
    keysTemp <- strsplit(unlist(strsplit(lv, "^", fixed = TRUE)), "=")
    keys <- sapply(keysTemp, function(k) {
      k[1]
    })
    keys <- keys[keys != ""]
    missing <- intersect(missing_rules, keys)
    dict$labelled[ri] <- length(missing) < length(keys)
    if (length(missing) != 0) {
      dict$missing[ri] <- paste0(missing, collapse = ";")
    }
  }
  # --- End getting missing values and labels
  
  # Get Type (which PV)
  dict$Type <- sapply(dict$variableName, function(zzz) {
    if (grepl("pv[1-9]", zzz, ignore.case = TRUE)) {
      return(ifelse(substring(zzz, 4, 4) == 0, substring(zzz, 5), substring(zzz, 4))) # for PV10
    } else {
      return("")
    }
  })
  
  # Get pvWt and weights
  dict$pvWt <- ""
  # This if finds the first line that define value label for each variable
  # In some syntax files, it's defined by explicitly saying "value label"
  # In some other files, it's defined with a leading "/"
  dict$pvWt <- mapply(function(v, t) {
    if (!grepl("^pv", v, ignore.case = TRUE)) {
      return("")
    } else {
      gsub(paste("pv", t, sep = "|"), "", v, ignore.case = TRUE)
    }
  }, dict$variableName, dict$Type)
  dict$pvWt[grepl("W_.*[0-9]$", dict$variableName, ignore.case = TRUE)] <- gsub("[^0-9]", "", dict$variableName[grepl("W_.*[0-9]$", dict$variableName, ignore.case = TRUE)])
  dict$pvWt[is.na(dict$pvWt)] <- ""
  dict$weights <- grepl("^w_.*[^0-9]$", dict$variableName, ignore.case = TRUE)
  
  # For PISA 2000, weights for reading, math, and science are re-adjusted
  if (grepl("2000_SPSS_student_math", filename)) {
    dict$variableName <- gsub("W_FSTR", "W_FSTR_MATH", dict$variableName)
    dict$variableName <- gsub("W_FSTUWT", "W_FSTUWT_MATH", dict$variableName)
  }
  if (grepl("2000_SPSS_student_read", filename)) {
    dict$variableName <- gsub("W_FSTR", "W_FSTR_READ", dict$variableName)
    dict$variableName <- gsub("W_FSTUWT", "W_FSTUWT_READ", dict$variableName)
  }
  if (grepl("2000_SPSS_student_scie", filename)) {
    dict$variableName <- gsub("W_FSTR", "W_FSTR_SCIE", dict$variableName)
    dict$variableName <- gsub("W_FSTUWT", "W_FSTUWT_SCIE", dict$variableName)
  }
  
  # Return a data.frame that stores data variable information (width, start, end, labels, etc.)
  dict$variableName <- trimws(dict$variableName, which = "right")
  # fix to valid names in R
  dict$variableName <- make.names(dict$variableName)
  
  # validates the file format by scanning the data from the source Fixed-Width data file before caching it
  # to the RDS .meta cache file.  super important step to ensure analysis is correctly identifying value type
  # see: "typeOfVarible" function in edsurveyTable.R file
  dict <- fixDict_FWF_Spacing(dict) # fixes any FWF spacing issues as LaF works only set 'widths'
  dict <- validateFileFormat_PISA(file.path(dirname(filename), datFname), dict)
  
  # create meta files
  cacheFile <- list(
    datFile = datFname,
    dict = dict,
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 6,
    ts = Sys.time()
  )
  saveRDS(cacheFile, gsub("\\.txt$", "\\.meta", filename))
  return(cacheFile)
}


# Used to merge fileFormat data frames
# @return a list that includes:
# 1. FFname = filename of merged file format data.frame
# 2. fileFormat = merged file format data.frame
#' @author Trang Nguyen
#' @importFrom utils read.csv
mergeFF <- function(filepath, LafDictList, by, mergeSuffixes) {
  mainLabelsFile <- LafDictList[[1]]$dict
  mainFileName <- gsub("\\.txt", "", LafDictList[[1]]$datFile)
  if (length(LafDictList) < 2) {
    warning("There is only one dataset. No need to merge")
    return(list(fileFormat = mainLabelsFile))
  }
  oldnames <- mainLabelsFile$variableName
  by <- toupper(by)
  if (length(by) != length(LafDictList) - 1) {
    stop(paste0("length of by list is compatible with length of merge files."))
  }
  # If by is a list of variables
  for (i in 2:length(LafDictList)) {
    newLabelsFile <- LafDictList[[i]]$dict
    newnames <- newLabelsFile$variableName
    index_of_by <- which(newnames %in% by[i - 1])
    junkvars <- setdiff(intersect(oldnames, newnames), by[i - 1])
    index_of_junk <- which(newnames %in% junkvars)
    
    # Rewrite labelsfile
    index_of_removed <- c(index_of_by, index_of_junk)
    newLabelsFile <- newLabelsFile[-index_of_removed, ]
    newLabelsFile$Start[1] <- mainLabelsFile$End[nrow(mainLabelsFile)] + 1
    for (r in 2:nrow(newLabelsFile)) {
      newLabelsFile$Start[r] <- newLabelsFile$Start[r - 1] + newLabelsFile$Width[r - 1]
    }
    newLabelsFile$End <- newLabelsFile$Start + newLabelsFile$Width - 1
    mainLabelsFile <- rbind(mainLabelsFile, newLabelsFile)
    
    # Finish up before continue with the loop
    mainLabelsFile <- mainLabelsFile[order(mainLabelsFile$Start), ]
    oldnames <- mainLabelsFile$variableName
  }
  # make sure these are valid names
  mainLabelsFile$variableName <- make.names(tolower(mainLabelsFile$variableName))
  # the cache file is the merged file format
  cacheFile <- list(
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 6,
    ts = Sys.time(),
    fileFormat = mainLabelsFile,
    FFname = paste0("M_FF_", paste0(names(LafDictList), collapse = "_"), "_", mergeSuffixes, ".meta")
  )
  saveRDS(cacheFile, file.path(filepath, paste0("M_FF_", paste0(names(LafDictList), collapse = "_"), "_", mergeSuffixes, ".meta")))
  return(cacheFile)
}

# Used fileformat read by readDict to read in SPSS text file
# and then break it down by country to write out csv files
processMergeTxt <- function(filepath, LafDictList, countries, ff, database, forceReread, verbose) {
  # Checking unprocessed countries
  datbasename <- gsub("FF", "DAT", ff$FFname)
  datbasename <- gsub("\\.meta", "", datbasename)
  datFnames <- list.files(filepath,
                          pattern = datbasename,
                          full.names = FALSE,
                          ignore.case = TRUE
  )
  if (file.exists(paste0(filepath, "/", database, "_all-countries.txt")) && !forceReread) {
    countryDict <- read.csv(paste0(filepath, "/", database, "_all-countries.txt"), stringsAsFactors = FALSE)
    all_countries <- countryDict$cnt[countryDict$available]
    processDT <- FALSE
  } else {
    # Produce country dictionary
    countryLabels <- unlist(strsplit(ff$fileFormat$labelValues[ff$fileFormat$variableName == "cnt"], "\\^"))
    if (length(countryLabels) == 0) {
      is2003 <- TRUE
      countryLabels <- unlist(strsplit(ff$fileFormat$labelValues[ff$fileFormat$variableName == "country"], "\\^"))
      countryDict <- do.call("rbind", strsplit(countryLabels, "="))
      countryDict <- data.frame(countryDict, stringsAsFactors = FALSE)
      colnames(countryDict) <- c("country", "country.name")
    } else {
      is2003 <- FALSE
      countryDict <- do.call("rbind", strsplit(countryLabels, "="))
      countryDict <- data.frame(countryDict, stringsAsFactors = FALSE)
      colnames(countryDict) <- c("cnt", "country.name")
    }
    # Read Data tables using LaF for speed
    masterDTlist <- lapply(LafDictList, function(x) {
      getMasterDTList(filepath, x, countryDict)
    })
    names(masterDTlist) <- names(LafDictList)
    all_countries <- unique(masterDTlist[[1]])
    all_countries$available <- TRUE
    if (is2003) {
      # in 2003, alternative merge
      countryDict <- merge(countryDict, all_countries, by.x = "country", by.y = "cnt_index", all.x = TRUE, all.y = FALSE)
      all_countries <- all_countries[ , "cnt", drop = TRUE]
    } else {
      countryDict <- merge(countryDict, all_countries, by.x = "cnt", by.y = "cnt_index", all.x = TRUE, all.y = FALSE)
      all_countries <- all_countries[ , "cnt_index", drop = TRUE]
    }
    countryDict$available[is.na(countryDict$available)] <- FALSE
    write.csv(countryDict, paste0(filepath, "/", database, "_all-countries.txt"), row.names = FALSE)
    processDT <- TRUE
  }
  
  # get the list of countries to be processed
  if (any(countries == "*")) {
    countries <- all_countries
  }
  countries <- toupper(countries)
  for (ci in countries) {
    if (!ci %in% all_countries) {
      warning("The database does not have data for the country ", dQuote(ci), ". These data will be recorded as NA.")
    }
  }
  countries <- intersect(countries, all_countries)
  if (length(countries) == 0) {
    stop("All countries specified are not available in the database. \n")
  }
  if (length(datFnames) == 0 || forceReread) {
    unprocessed <- countries
  } else {
    processed <- gsub(paste0(datbasename, "_"), "", datFnames)
    processed <- gsub("\\.txt", "", processed)
    unprocessed <- setdiff(countries, processed)
  }
  
  if (length(unprocessed) == 0) {
    return(list(datbasename = datbasename, countries = countries, fileFormat = ff))
  }
  # If need to process some countries
  if (!processDT) {
    masterDTlist <- lapply(LafDictList, function(x) {
      getMasterDTList(filepath, x, countryDict)
    })
    names(masterDTlist) <- names(LafDictList)
  }
  masterDTlist <- NULL
  
  for (cntry in unprocessed) {
    if (verbose) {
      cat(paste0("Processing data for country code ", dQuote(cntry), "\n"))
    }
    for (li in seq_along(LafDictList)) {
      x <- LafDictList[[li]]
      dict <- x$dict
      
      fname <- file.path(filepath, x$datFile)
      fname <- fixPath(fname)
      
      lafD <- LaF::laf_open_fwf(
        filename = fname,
        column_types = dict$dataType,
        column_widths = dict$Width,
        column_names = dict$variableName,
        dec = ".",
        trim = TRUE
      )
      # get the country
      cnt <- lafD[ , tolower(dict$variableName) == "cnt"]
      datai <- lafD[cnt == cntry, ]
      LaF::close(lafD)
      if (nrow(datai) == 0) {
        next
      }
      colnames(datai) <- tolower(colnames(datai))
      if (li == 1) {
        mainDat <- datai
      } else {
        mainDat <- merge(mainDat, datai,
                         by = intersect(c("cnt", "schoolid", "stidstd"), colnames(datai)),
                         suffixes = c("", ".junk"), all.x = TRUE, all.y = FALSE
        )
        # these variables are duplicate, drop them
        mainDat <- mainDat[ , !grepl(".junk", colnames(mainDat))]
      }
    }
    missingcolumns <- setdiff(tolower(ff$fileFormat$variableName), colnames(mainDat))
    if (length(missingcolumns) > 0) {
      for (missingcolumn in missingcolumns) {
        mainDat[ , missingcolumn] <- NA
      }
    }
    if (length(missingcolumns <- setdiff(tolower(ff$fileFormat$variableName), colnames(mainDat))) > 0) {
      stop(paste0("Error processing variables:", pasteItems(missingcolumns)))
    }
    # trim ws, which appears to cause errors
    maybeTrim <- function(x) {
      if (inherits(x, "character")) {
        x <- trimws(x)
      }
      if (inherits(x, "factor")) {
        x <- trimws(as.character(x))
      }
      return(x)
    }
    mainDat <- as.data.frame(lapply(mainDat, maybeTrim), stringsAsFactors = FALSE)
    mainDat <- mainDat[ , tolower(ff$fileFormat$variableName)]
    
    # write out to CSV file
    write.table(mainDat,
                file = paste0(filepath, "/", datbasename, "_", cntry, ".txt"),
                sep = ",", col.names = FALSE, na = "", row.names = FALSE,
                quote = FALSE
    ) # all numeric
  }
  
  return(list(datbasename = datbasename, countries = countries, fileFormat = ff))
}

fixDict_FWF_Spacing <- function(dict) {
  # this gets fixed prior
  dict$dt <- ifelse(dict$dataType == "integer", "numeric", dict$dataType)
  
  # this is the width calculated base on column end points, if it
  # does not line up, then the widths are wrong
  dict$Width2 <- c(dict$End[1], dict$End[-1] - dict$End[-nrow(dict)])
  # in these cases the width did not line up, fix these
  issues <- (1:nrow(dict))[dict$Width != dict$Width2]
  
  # reverse issue order so that order stay correct
  for (issue in rev(issues)) {
    # issue is a row index with a problem
    # make a new row
    newrow <- dict[issue, ]
    # set the width to fill the gap
    newrow$Width <- newrow$Width2 - newrow$Width # fill the gap
    
    newrow$variableName <- "zzz_Filler"
    newrow$dataType <- newrow$dt <- "character" # make sure it can be read in
    newrow$Labels <- "Column Filler for Read-in"
    newrow$labelValues <- ""
    newrow$labelled <- FALSE
    newrow$missing <- ""
    newrow$Type <- ""
    newrow$pvWt <- ""
    newrow$weights <- FALSE
    dict[issue:nrow(dict) + 1, ] <- dict[issue:nrow(dict), ]
    dict[issue, ] <- newrow
  }
  
  # recalibrate the start/end positions after placing in the filler columns
  dict$Start <- c(1, 1 + cumsum(dict$Width))[1:nrow(dict)]
  dict$End <- cumsum(dict$Width)
  
  dict$variableName <- tolower(make.names(dict$variableName, unique = TRUE))
  
  return(dict)
}

# Used to serialize an existing csv files
catchCountryTxt <- function(filepath, datname, ff) {
  lafFile <- list.files(filepath, pattern = datname, ignore.case = TRUE)
  lafFile <- file.path(filepath, lafFile)
  if (length(lafFile) > 1) {
    mTime <- sapply(lafFile, function(f) {
      file.info(f)$mtime
    })
    if (length(which.max(mTime)) == 0) {
      lafFile <- lafFile[1]
    } else {
      lafFile <- lafFile[which.max(mTime)]
    }
  }
  columnTypes <- ifelse(ff$fileFormat$dataType == "numeric", "double",
                        ifelse(ff$fileFormat$dataType == "character", "string",
                               ff$fileFormat$dataType
                        )
  )
  
  ret <- LaF::laf_open_csv(lafFile,
                           column_types = columnTypes,
                           column_names = tolower(ff$fileFormat$variableName)
  )
  return(ret)
}

# Used to look up achievement levels for each year, database, and subject
pisaAchievementHelp <- function(year, database) {
  achievementDict <- readRDS(system.file("extdata", "PISAal.rds", package = "EdSurvey"))
  achievementDict$level <- paste0("Proficiency ", achievementDict$level)
  temp_subset <- achievementDict[achievementDict$year == year & achievementDict$database == database, ]
  
  # sort temp_subset based on subject name to have same ordering across the years
  # otherwise it will go based on the ordering in the RDS file
  temp_subset <- temp_subset[order(temp_subset$subject), ]
  
  ret <- list()
  ret$subjects <- unique(temp_subset$subject)
  ret$regex <- unique(temp_subset$regex)
  ret$default <- temp_subset$subject[temp_subset$major == 1][1]
  ret$achievementLevels <- list()
  for (s in ret$subjects) {
    ret$achievementLevels[[s]] <- temp_subset$lower_limit[temp_subset$subject == s]
    names(ret$achievementLevels[[s]]) <- ifelse(is.na(temp_subset$level[temp_subset$subject == s]),
                                                "Not Defined",
                                                temp_subset$level[temp_subset$subject == s]
    )
  }
  return(ret)
}

# Used to convert short country name to long names
convertCountryName <- function(countryDict, countrycode) {
  return(countryDict$country.name[tolower(countryDict$cnt) == tolower(countrycode) & !is.na(countryDict$cnt)])
}

#' @importFrom utils memory.limit
#' @importFrom haven read_sav write_sav
processPISA2015 <- function(filepath, verbose, countries, year) {
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  userOp2 <- options(scipen = 999) # ensure no scientific notation
  on.exit(options(userOp2), add = TRUE)
  if (suppressWarnings(memory.limit()) < 16000) {
    # only for older R versions
    memory.limit(16000) # COG takes a lot of memory to read in, increase memory limit to allow it to be read in.
  }
  studentSavFileList <- list.files(filepath, pattern = "stu.*\\.sav$", ignore.case = TRUE)
  schoolSavFileList <- list.files(filepath, pattern = "sch.*\\.sav$", ignore.case = TRUE)
  
  # Read in data
  mainFile <- grep("qqq", studentSavFileList, ignore.case = TRUE)
  if (length(mainFile) == 0) {
    stop("Missing Student Questionaire SPSS (.sav) data file. Since this is the main student file, it's required that this data file must be downloaded.")
  }
  full_fnames <- c("stu_qqq", "stu_qq2", "stu_cog", "stu_qtm", "stu_flt", "stu_cps", "stu_tim", "sch_qqq")
  savFileList <- sapply(full_fnames, function(f) {
    t <- grep(f, c(studentSavFileList, schoolSavFileList), ignore.case = TRUE, value = TRUE)
    if (length(t) > 1) {
      return(t[1])
    }
    if (length(t) == 0) {
      return(NULL)
    }
    return(t)
  })
  savFileList <- unlist(savFileList)
  if (verbose) {
    cat("Importing SAV data into R\n")
  }
  all_countries <- ""
  # parse dictionary files
  studentFFlist <- lapply(savFileList, function(f) {
    if (verbose) {
      cat(paste0("Processing ", sQuote(f), "\n"))
    }
    
    t <- read_sav(file.path(filepath, f), user_na = TRUE, n_max = 1) # only read 1 line; validate datatypes later
    dct <- readDict2015(t, year)
    rm(t)
    return(dct)
  })
  
  # resave files
  lapply(savFileList, function(f) {
    if (verbose) {
      cat(paste0("Importing ", sQuote(f), "\n"))
    }
    t <- read_sav(file.path(filepath, f), user_na = TRUE, col_select = "CNT")
    country <- factor(t$CNT)
    rm(t)
    if (f == savFileList[[1]]) {
      all_countries <<- unique(country)
    }
    cntrys <- countries
    # wild card means everying
    if (any(cntrys == "*")) {
      cntrys <- all_countries
    }
    for (ci in cntrys) {
      f2 <- paste0(gsub(".sav$", "", f, ignore.case = TRUE), "_", ci, ".rds")
      if (verbose) {
        cat(paste0("  saving tmp ", sQuote(f2), "\n"))
      }
      wci <- which(country == ci)
      if (length(wci) > 0) {
        # just read in rows that are in the range of this country's record
        tempi <- read_sav(file.path(filepath, f), user_na = TRUE, skip = min(wci) - 1, n_max = max(wci) - min(wci) + 1)
        # filter to just this country
        tempi <- tempi[tempi$CNT == ci, ]
        
        saveRDS(tempi, file = file.path(filepath, f2))
        rm(tempi)
      }
    }
    return(NULL)
  })
  
  idlinkage <- list(
    stu_qqq = "", stu_qq2 = tolower("CNTSTUID"),
    stu_cog = tolower("CNTSTUID"), stu_qtm = tolower("CNTSTUID"),
    stu_flt = tolower("CNTSTUID"), stu_cps = tolower("CNTSTUID"),
    stu_tim = tolower("CNTSTUID"),
    sch_qqq = tolower("CNTSCHID")
  )
  
  idlinkage <- idlinkage[names(idlinkage) %in% names(savFileList)]
  # Merge file formats
  mainLabelsFile <- studentFFlist[[1]]
  if (length(studentFFlist) < 2) {
    warning("There is only one dataset. No need to merge")
    ff <- mainLabelsFile
  } else {
    oldnames <- toupper(mainLabelsFile$variableName)
    by <- toupper(idlinkage[2:length(idlinkage)])
    if (length(by) != length(studentFFlist) - 1) {
      stop(paste0("length of by list is compatible with length of merge files."))
    }
    # If by is a list of variables
    for (i in 2:length(studentFFlist)) {
      newLabelsFile <- studentFFlist[[i]]
      newnames <- toupper(newLabelsFile$variableName)
      index_of_by <- which(newnames %in% by[i - 1])
      junkvars <- setdiff(intersect(oldnames, newnames), by[i - 1])
      # some duplicate vars are actually not junk
      vars_to_exclude_from_junk <- grep("ver_dat|senwt", junkvars, ignore.case = TRUE, value = TRUE)
      junkvars <- junkvars[!junkvars %in% vars_to_exclude_from_junk]
      index_of_junk <- which(newnames %in% junkvars)
      # Rewrite labelsfile
      index_of_removed <- c(index_of_by, index_of_junk)
      newLabelsFile <- newLabelsFile[-index_of_removed, ]
      if (length(vars_to_exclude_from_junk) > 0) {
        newLabelsFile$variableName <- ifelse(newLabelsFile$variableName %in% vars_to_exclude_from_junk,
                                             paste0(newLabelsFile$variableName, ".", names(studentFFlist)[i]),
                                             newLabelsFile$variableName
        )
      }
      newLabelsFile$Start[1] <- mainLabelsFile$End[nrow(mainLabelsFile)] + 1
      for (r in 2:nrow(newLabelsFile)) {
        newLabelsFile$Start[r] <- newLabelsFile$Start[r - 1] + newLabelsFile$Width[r - 1]
      }
      newLabelsFile$End <- newLabelsFile$Start + newLabelsFile$Width - 1
      mainLabelsFile <- rbind(mainLabelsFile, newLabelsFile)
      oldnames <- toupper(mainLabelsFile$variableName)
    }
    ff <- mainLabelsFile
  }
  ### Merging
  # process all countries.
  if (any(countries == "*")) {
    countries <- all_countries
  }
  countries <- intersect(toupper(countries), toupper(all_countries))
  for (cntry in countries) {
    if (verbose) {
      cat(paste0("Processing data for country code ", dQuote(cntry), "\n"))
    }
    f2 <- paste0(gsub(".sav$", "", savFileList[[1]], ignore.case = TRUE), "_", cntry, ".rds")
    if (!file.exists(file.path(filepath, f2))) {
      stop("Cache file ", sQuote(file.path(filepath, f2)), " missing.")
    }
    mm <- readRDS(file.path(filepath, f2))
    mm <- UnclassCols(mm)
    # "repeat." is the fixed name of "repeat", change both to this clearer name
    colnames(mm) <- toupper(colnames(mm))
    colnames(mm) <- gsub("^REPEAT\\.$", "REPEATGRADE", colnames(mm), ignore.case = TRUE)
    colnames(mm) <- gsub("^REPEAT$", "REPEATGRADE", colnames(mm), ignore.case = TRUE)
    colnames(mm) <- gsub("\\.$", "", colnames(mm))
    for (li in 2:length(savFileList)) {
      f2 <- paste0(gsub(".sav$", "", savFileList[li], ignore.case = TRUE), "_", cntry, ".rds")
      if (!file.exists(file.path(filepath, f2))) {
        if (verbose) {
          cat(paste(cntry, "does not have", names(savFileList)[li], "data.\n"))
        }
        next
      }
      m2 <- readRDS(file.path(filepath, f2))
      m2 <- UnclassCols(m2)
      # "repeat." is the fixed name of "repeat", change both to this clearer name
      colnames(m2) <- gsub("^REPEAT\\.$", "REPEATGRADE", colnames(m2), ignore.case = TRUE)
      colnames(m2) <- gsub("^REPEAT$", "REPEATGRADE", colnames(m2), ignore.case = TRUE)
      colnames(m2) <- gsub("\\.$", "", colnames(m2))
      ## Merging data
      mm <- merge(mm, m2, by = toupper(idlinkage[[li]]), suffixes = c("", ".junk"), all.x = TRUE, all.y = FALSE)
      rm(m2)
      var_duplicated_valid <- grep("(senwt|ver_dat)\\.junk", colnames(mm), value = TRUE, ignore.case = TRUE)
      colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)] <- gsub("\\.junk", paste0(".", names(savFileList)[li]), colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)])
      mm <- mm[ , grep("\\.junk", colnames(mm), invert = TRUE, value = TRUE, ignore.case = TRUE)]
    } # end li
    mm <- cbind("cnt" = cntry, mm)
    colnames(mm) <- tolower(colnames(mm))
    missingcolumns <- setdiff(tolower(ff$variableName), colnames(mm))
    if (length(missingcolumns) > 0) {
      naMa <- matrix(NA, nrow = nrow(mm), ncol = length(missingcolumns))
      colnames(naMa) <- missingcolumns
      mm <- cbind(mm, naMa)
    }
    mm <- mm[ , tolower(ff$variableName)]
    ## Exporting dat and ff txt file
    for (i in 1:ncol(mm)) {
      if (is.numeric(mm[[i]])) {
        mm[[i]] <- as.character(mm[[i]])
        mm[[i]] <- ifelse(is.na(mm[[i]]), "", format(mm[[i]], scientific = FALSE))
      }
    }
    
    if (year == 2015) {
      outf <- file.path(filepath, paste0("M_DAT_CY6_MS_CMB_STU_", cntry, ".txt"))
    } else if (year == 2018) {
      outf <- file.path(filepath, paste0("M_DAT_CY7_MS_CMB_STU_", cntry, ".txt"))
    } else if (year == 2022){
      outf <- file.path(filepath, paste0("M_DAT_CY8_MS_CMB_STU_", cntry, ".txt"))
    }
    
    if (file.exists(outf)) {
      unlink(outf)
    }
    
    # validate the file format against the full dataset (for each country) before it's cached in the .meta file
    ff <- validateFileFormat_PISA_2015(mm, ff)
    write.table(mm, file = outf, col.names = FALSE, row.names = FALSE, sep = ",", na = "")
  }
  ff$variableName <- gsub("^REPEAT\\.$", "REPEATGRADE", ff$variableName, ignore.case = TRUE)
  ff$variableName <- gsub("^REPEAT$", "REPEATGRADE", ff$variableName, ignore.case = TRUE)
  
  # Produce country dictionary
  countryLabels <- unlist(strsplit(ff$labelValues[toupper(ff$variableName) == toupper("cnt")], "\\^"))
  countryDict <- do.call("rbind", strsplit(countryLabels, "="))
  countryDict <- data.frame(countryDict, stringsAsFactors = FALSE)
  colnames(countryDict) <- c("cnt", "country.name")
  countryDict$available <- (tolower(countryDict$cnt) %in% tolower(all_countries))
  cacheFile <- list(
    countryDict = countryDict, dict = ff,
    list_files = names(savFileList),
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 6, ts = Sys.time()
  )
  saveRDS(cacheFile, file.path(filepath, "INT_all-countries.meta"))
  return(cacheFile)
}

readDict2015 <- function(l, year) {
  # Build a dataframe that stores information for each variable =========================================================
  # colInfo is a temporary data.frame that stores information on each variable (format, start, end, data type)
  # year is passed for 2018 in particular the SPSS formats occasionally do not match the raw data and must be validated causing slowdown in processing
  l <- UnclassCols(l)
  # change "repeat" and the fixed name "repeat." to the clearer "repeatgrade"
  colnames(l) <- gsub("^repeat$", "repeatgrade", colnames(l), ignore.case = TRUE)
  colnames(l) <- gsub("^repeat.$", "repeatgrade", colnames(l), ignore.case = TRUE)
  colInfo <- data.frame(variableName = colnames(l), stringsAsFactors = FALSE)
  colInfo$format <- sapply(colInfo$variableName, function(z) {
    attributes(l[[z]])$format.spss
  })
  
  colInfo$Decimal <- as.numeric(ifelse(substr(colInfo$format, 1, 1) == "F", sapply(strsplit(colInfo$format, "\\."), function(x) {
    tail(x, 1)
  }), rep(NA, nrow(colInfo))))
  colInfo$Decimal[is.na(colInfo$Decimal) & !(tolower(colInfo$class) %in% "date")] <- 0 # dates are omitted based on SPSS class type so they are characters
  colInfo$multiplier <- as.integer(ifelse(is.na(colInfo$Decimal), 1, 10^colInfo$Decimal))
  colInfo$Width <- gsub("[a-zA-Z]", "", sapply(strsplit(colInfo$format, "\\."), function(x) {
    head(x, 1)
  }))
  colInfo$Width <- as.numeric(colInfo$Width)
  colInfo$Labels <- sapply(colnames(l), function(z) {
    attributes(l[[z]])$label
  })
  colInfo$labelValues <- sapply(colnames(l), function(z) {
    attr <- attributes(l[[z]])$labels
    toupper(paste(attr, names(attr), sep = "=", collapse = "^"))
  })
  colInfo$format <- NULL
  colInfo$Start <- c(1, 1 + cumsum(colInfo$Width))[1:nrow(colInfo)]
  colInfo$End <- cumsum(colInfo$Width)
  
  # missing and labels
  colInfo$labelled <- logical(nrow(colInfo))
  colInfo$missing <- ""
  missing_rules <- c(
    9, 99, 999, 9999, 99999, 999999, 99999999,
    8, 98, 998, 9998, 99998, 999998, 99999998,
    7, 97, 997, 9997, 99997, 999997, 99999997,
    96, 996, 9996, 99996, 999996, 99999996,
    95, 995, 9995, 99995, 999995, 99999995
  )
  for (ri in 1:nrow(colInfo)) {
    lv <- colInfo$labelValues[ri]
    keysTemp <- strsplit(unlist(strsplit(lv, "^", fixed = TRUE)), "=")
    keys <- sapply(keysTemp, function(k) {
      k[1]
    })
    keys <- keys[keys != ""]
    missing <- intersect(missing_rules, keys)
    colInfo$labelled[ri] <- length(missing) < length(keys)
    if (length(missing) != 0) {
      colInfo$missing[ri] <- paste0(missing, collapse = ";")
    }
  }
  # --- End getting missing values and labels
  
  # Get Type (which PV)
  colInfo$Type <- sapply(colInfo$variableName, function(zzz) {
    if (grepl("pv[1-9]", zzz, ignore.case = TRUE)) {
      return(ifelse(substring(zzz, 4, 4) == 0, substring(zzz, 5), substring(zzz, 4))) # for PV10
    } else {
      return("")
    }
  })
  
  # Get pvWt and weights
  colInfo$pvWt <- ""
  colInfo$pvWt <- mapply(function(v, t) {
    if (!grepl("^pv", v, ignore.case = TRUE)) {
      return("")
    } else {
      gsub(paste("pv", t, sep = "|"), "", v, ignore.case = TRUE)
    }
  }, colInfo$variableName, colInfo$Type)
  colInfo$pvWt[grepl("W_.*[0-9]$", colInfo$variableName, ignore.case = TRUE)] <- gsub("[^0-9]", "", colInfo$variableName[grepl("W_.*[0-9]$", colInfo$variableName, ignore.case = TRUE)])
  colInfo$pvWt[is.na(colInfo$pvWt)] <- ""
  
  # exclude cases that end with the 'sum' keyword (e.g., 'w_fstuwt_sch_sum' as that's not a usable weight)
  colInfo$weights <- grepl("^w_.*[^0-9]$", colInfo$variableName, ignore.case = TRUE) & !grepl("sum$", colInfo$variableName, ignore.case = TRUE)
  ## characters will have an N/A for their decimal value
  colInfo$dataType <- ifelse(colInfo$Decimal %in% 1:32 | (colInfo$Width > 9 & colInfo$Decimal %in% 0), rep("numeric", nrow(colInfo)),
                             ifelse(colInfo$Decimal %in% 0, rep("integer", nrow(colInfo)),
                                    rep("character", nrow(colInfo))
                             )
  )
  return(colInfo)
}

validateFileFormat_PISA_2015 <- function(df, fileFormat) {
  idx <- which(tolower(fileFormat$dataType) == "integer", arr.ind = TRUE) # only scan integers to see if they are numeric (i.e., have a decimal period)
  tempDF <- df[ , idx]
  
  res <- unlist(lapply(tempDF, valid.integer))
  
  if (!all(res, na.rm = TRUE)) {
    fileFormat$dataType[tolower(fileFormat$variableName) %in% tolower(colnames(tempDF)[!res])] <- "numeric"
  }
  
  return(fileFormat) # return updated fileFormat
}

# TRUE if the vector is a valid integer (no decimal, and less than a length of 9 (indicating large numeric))
valid.integer <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  test1 <- n %% 1 == 0 # test for decimals
  test2 <- nchar(x) <= 8 # test for out of bounds
  
  test1[is.na(test1)] <- TRUE
  test2[is.na(test2)] <- TRUE
  
  return(all(test1) && all(test2))
}

# builds the NAEP dataList object
buildPISA_dataList <- function(LaF, FF) {
  dataList <- list()
  
  # build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(
    lafObject = LaF,
    fileFormat = FF,
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

# get country data from any year
getMasterDTList <- function(filepath, x, countryDict) {
  fname <- file.path(filepath, x$datFile)
  fname <- fixPath(fname)
  # PISA 2003 uses "COUNTRY" others use "CNT"
  v1 <- x$dict$Start[tolower(x$dict$variableName) %in% c("cnt")]
  v2 <- x$dict$End[tolower(x$dict$variableName) %in% c("cnt")]
  if ("cnt" %in% colnames(countryDict)) {
    # not PISA 2003
    if (v1 == 1) {
      # if the file starts with cnt_index, just grab that
      laf0 <- laf_open_fwf(fname, column_types = c("character"), column_widths = c(v2), column_names = "cnt_index")
    } else {
      # otherwise have to start with the garbage before cnt_index and throw it out
      laf0 <- laf_open_fwf(fname, column_types = c("character", "character"), column_widths = c(v1 - 1, v2 - v1 + 1), column_names = c("notused", "cnt_index"))
    }
    text <- laf0[ , "cnt_index", drop = FALSE]
    LaF::close(laf0)
  } else {
    # PISA 2003 uses "COUNTRY" on countryDict (a numeric), and doesn't have "CNT" so use "but also has "CNT", so grab both
    v3 <- x$dict$Start[tolower(x$dict$variableName) %in% c("country")]
    v4 <- x$dict$End[tolower(x$dict$variableName) %in% c("country")]
    # LaF can handle a width of 0
    laf0 <- laf_open_fwf(fname, column_types = c("character", "character"), column_widths = c(3, 3), column_names = c("cnt_index", "cnt"))
    text <- laf0[ , c("cnt_index", "cnt"), drop = FALSE]
    LaF::close(laf0)
  }
  return(text)
}

# correctly identifies the data types based on the source data file (being read-in as characters)
# this is needed since we cannot rely on formats specified in the SPSS syntax file correctly
validateFileFormat_PISA <- function(dataFilePath, fileFormat) {
  # test that the ending position matches the number of characters in the first row
  # special case for 2000 reading FWF data file as it's specified out of bounds
  fCon <- file(dataFilePath, encoding = "cp1252")
  on.exit(close(fCon))
  testLen <- nchar(readLines(fCon, n = 1))
  
  if (fileFormat[nrow(fileFormat), "End"] > testLen) {
    # calc the diff of the offage and adjust the End position and the Width positions
    d <- fileFormat[nrow(fileFormat), "End"] - testLen
    fileFormat[nrow(fileFormat), "End"] <- testLen
    fileFormat[nrow(fileFormat), "Width"] <- fileFormat[nrow(fileFormat), "Width"] - d
  }
  
  sourceData_LaFObj <- laf_open_fwf(
    filename = dataFilePath,
    column_types = fileFormat$dataType,
    column_widths = fileFormat$Width,
    column_names = fileFormat$variableName
  )
  on.exit(close(sourceData_LaFObj))
  
  sourceData <- sourceData_LaFObj[] # get the full dataset for validation analysis
  
  colsToScan <- which(tolower(fileFormat$dataType) %in% c("integer", "numeric"), arr.ind = TRUE) # only scan numeric or integer columns
  
  # ======
  for (coli in colsToScan) {
    # test specifically if a numeric value has a decimal or is an integer by analyzing raw column data
    colData <- sourceData[[coli]]
    isDecimal <- any(grepl(".", colData, fixed = TRUE), na.rm = TRUE)
    
    if (isDecimal) {
      # update all the format and decimal values for these numerics regardless of what the SPSS formatting says
      fileFormat$dataType[coli] <- "numeric"
    } else { # integer or character
      if (fileFormat$Width[coli] < 8) {
        fileFormat$dataType[coli] <- "integer"
        fileFormat$Decimal[coli] <- 0
      } else { # numeric since it's a large number
        fileFormat$dataType[coli] <- "numeric"
      }
    } # end else if(isDecimal)
  } # end for(coli in 1:nrow(fileFormat))
  
  return(fileFormat)
}

processPISA2018_FIN <- function(filepath, verbose, countries, year) {
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  userOp2 <- options(scipen = 999) # ensure no scientific notation
  on.exit(options(userOp2), add = TRUE)
  if (suppressWarnings(memory.limit()) < 16000) {
    memory.limit(16000) # COG takes a lot of memory to read in, increase memory limit to allow it to be read in.
  }
  
  # grab all applicable files
  # QQQ --> Student Questionaire - plausible values for reading/math and financial literacy
  # COG --> Cognitive Items - Has the item level cognitive response items
  # TTM --> Has the item level timing/process data
  # TIM --> Has the overall timing/process data
  if (year == "2022"){
    fileList <- list.files(filepath, pattern = "CY08MSP_FLT_(QQQ|COG|TTM|TIM)\\.sav$", ignore.case = TRUE)
  } else if (year == "2018"){
    fileList <- list.files(filepath, pattern = "CY07_MSU_FLT_(QQQ|COG|TTM|TIM)\\.sav$", ignore.case = TRUE)
  } else {
    warning("year needs to be either 2018 or 2022!")
  }
  
  # Read in data
  mainFile <- grep("qqq", fileList, ignore.case = TRUE)
  if (length(mainFile) == 0) {
    stop("Missing Student Questionaire SPSS (.sav) data file. Since this is the main student file, it's required that this data file must be downloaded.")
  }
  full_fnames <- c("flt_qqq", "flt_cog", "flt_ttm", "flt_tim")
  savFileList <- sapply(full_fnames, function(f) {
    t <- grep(f, fileList, ignore.case = TRUE, value = TRUE)
    if (length(t) > 1) {
      return(t[1])
    }
    if (length(t) == 0) {
      return(NULL)
    }
    return(t)
  })
  savFileList <- unlist(savFileList)
  if (verbose) {
    cat("Importing SAV data into R\n")
  }
  all_countries <- ""
  # parse dictionary files
  studentFFlist <- lapply(savFileList, function(f) {
    if (verbose) {
      cat(paste0("Processing ", sQuote(f), "\n"))
    }
    
    t <- read_sav(file.path(filepath, f), user_na = TRUE, n_max = 1) # only read 1 line; validate datatypes later
    dct <- readDict2015(t, year)
    rm(t)
    return(dct)
  })
  
  # resave files
  lapply(savFileList, function(f) {
    if (verbose) {
      cat(paste0("Importing ", sQuote(f), "\n"))
    }
    t <- read_sav(file.path(filepath, f), user_na = TRUE, col_select = "CNT")
    country <- factor(t$CNT)
    rm(t)
    if (f == savFileList[[1]]) {
      all_countries <<- unique(country)
    }
    cntrys <- countries
    # wild card means everying
    if (any(cntrys == "*")) {
      cntrys <- all_countries
    }
    for (ci in cntrys) {
      f2 <- paste0(gsub("\\.sav$", "", f, ignore.case = TRUE), "_", ci, ".rds")
      if (verbose) {
        cat(paste0("  saving tmp ", sQuote(f2), "\n"))
      }
      wci <- which(country == ci)
      if (length(wci) > 0) {
        # just read in rows that are in the range of this country's record
        tempi <- read_sav(file.path(filepath, f), user_na = TRUE, skip = min(wci) - 1, n_max = max(wci) - min(wci) + 1)
        # filter to just this country
        tempi <- tempi[tempi$CNT == ci, ]
        
        saveRDS(tempi, file = file.path(filepath, f2))
        rm(tempi)
      }
    }
    return(NULL)
  })
  
  idlinkage <- list(
    flt_qqq = "", flt_cog = tolower("CNTSTUID"),
    flt_ttm = tolower("CNTSTUID"), flt_tim = tolower("CNTSTUID")
  )
  
  idlinkage <- idlinkage[names(idlinkage) %in% names(savFileList)]
  # Merge file formats
  mainLabelsFile <- studentFFlist[[1]]
  if (length(studentFFlist) < 2) {
    warning("There is only one dataset. No need to merge")
    ff <- mainLabelsFile
  } else {
    oldnames <- toupper(mainLabelsFile$variableName)
    by <- toupper(idlinkage[2:length(idlinkage)])
    if (length(by) != length(studentFFlist) - 1) {
      stop(paste0("length of by list is compatible with length of merge files."))
    }
    # If by is a list of variables
    for (i in 2:length(studentFFlist)) {
      newLabelsFile <- studentFFlist[[i]]
      newnames <- toupper(newLabelsFile$variableName)
      index_of_by <- which(newnames %in% by[i - 1])
      junkvars <- setdiff(intersect(oldnames, newnames), by[i - 1])
      # some duplicate vars are actually not junk
      vars_to_exclude_from_junk <- grep("ver_dat|senwt", junkvars, ignore.case = TRUE, value = TRUE)
      junkvars <- junkvars[!junkvars %in% vars_to_exclude_from_junk]
      index_of_junk <- which(newnames %in% junkvars)
      # Rewrite labelsfile
      index_of_removed <- c(index_of_by, index_of_junk)
      newLabelsFile <- newLabelsFile[-index_of_removed, ]
      if (length(vars_to_exclude_from_junk) > 0) {
        newLabelsFile$variableName <- ifelse(newLabelsFile$variableName %in% vars_to_exclude_from_junk,
                                             paste0(newLabelsFile$variableName, ".", names(studentFFlist)[i]),
                                             newLabelsFile$variableName
        )
      }
      newLabelsFile$Start[1] <- mainLabelsFile$End[nrow(mainLabelsFile)] + 1
      for (r in 2:nrow(newLabelsFile)) {
        newLabelsFile$Start[r] <- newLabelsFile$Start[r - 1] + newLabelsFile$Width[r - 1]
      }
      newLabelsFile$End <- newLabelsFile$Start + newLabelsFile$Width - 1
      mainLabelsFile <- rbind(mainLabelsFile, newLabelsFile)
      oldnames <- toupper(mainLabelsFile$variableName)
    }
    ff <- mainLabelsFile
  }
  ### Merging
  # process all countries.
  if (any(countries == "*")) {
    countries <- all_countries
  }
  countries <- intersect(toupper(countries), toupper(all_countries))
  
  for (cntry in countries) {
    if (verbose) {
      cat(paste0("Processing data for country code ", dQuote(cntry), "\n"))
    }
    f2 <- paste0(gsub("\\.sav$", "", savFileList[[1]], ignore.case = TRUE), "_", cntry, ".rds")
    if (!file.exists(file.path(filepath, f2))) {
      stop("Cache file ", sQuote(file.path(filepath, f2)), " missing.")
    }
    mm <- readRDS(file.path(filepath, f2))
    mm <- UnclassCols(mm)
    # "repeat." is the fixed name of "repeat", change both to this clearer name
    colnames(mm) <- toupper(colnames(mm))
    colnames(mm) <- gsub("^REPEAT\\.$", "REPEATGRADE", colnames(mm), ignore.case = TRUE)
    colnames(mm) <- gsub("^REPEAT$", "REPEATGRADE", colnames(mm), ignore.case = TRUE)
    colnames(mm) <- gsub("\\.$", "", colnames(mm))
    for (li in 2:length(savFileList)) {
      f2 <- paste0(gsub("\\.sav$", "", savFileList[li], ignore.case = TRUE), "_", cntry, ".rds")
      if (!file.exists(file.path(filepath, f2))) {
        if (verbose) {
          cat(paste(cntry, "does not have", names(savFileList)[li], "data.\n"))
        }
        next
      }
      m2 <- readRDS(file.path(filepath, f2))
      m2 <- UnclassCols(m2)
      # "repeat." is the fixed name of "repeat", change both to this clearer name
      colnames(m2) <- gsub("^REPEAT\\.$", "REPEATGRADE", colnames(m2), ignore.case = TRUE)
      colnames(m2) <- gsub("^REPEAT$", "REPEATGRADE", colnames(m2), ignore.case = TRUE)
      colnames(m2) <- gsub("\\.$", "", colnames(m2))
      ## Merging data
      mm <- merge(mm, m2, by = toupper(idlinkage[[li]]), suffixes = c("", ".junk"), all.x = TRUE, all.y = FALSE)
      rm(m2)
      var_duplicated_valid <- grep("(senwt|ver_dat)\\.junk", colnames(mm), value = TRUE, ignore.case = TRUE)
      colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)] <- gsub("\\.junk", paste0(".", names(savFileList)[li]), colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)])
      mm <- mm[ , grep("\\.junk", colnames(mm), invert = TRUE, value = TRUE, ignore.case = TRUE)]
    } # end li
    mm <- cbind("cnt" = cntry, mm)
    colnames(mm) <- tolower(colnames(mm))
    missingcolumns <- setdiff(tolower(ff$variableName), colnames(mm))
    if (length(missingcolumns) > 0) {
      naMa <- matrix(NA, nrow = nrow(mm), ncol = length(missingcolumns))
      colnames(naMa) <- missingcolumns
      mm <- cbind(mm, naMa)
    }
    mm <- mm[ , tolower(ff$variableName)]
    ## Exporting dat and ff txt file
    for (i in 1:ncol(mm)) {
      if (is.numeric(mm[[i]])) {
        mm[[i]] <- as.character(mm[[i]])
        mm[[i]] <- ifelse(is.na(mm[[i]]), "", format(mm[[i]], scientific = FALSE))
      }
    }
    
    if (year == 2022) {
      outf <- file.path(filepath, paste0("M_DAT_CY8_FIN_", cntry, ".txt"))
    }
    
    if (year == 2018) {
      outf <- file.path(filepath, paste0("M_DAT_CY7_FIN_", cntry, ".txt"))
    }
    
    if (file.exists(outf)) {
      unlink(outf)
    }
    
    # validate the file format against the full dataset (for each country) before it's cached in the .meta file
    ff <- validateFileFormat_PISA_2015(mm, ff)
    
    write.table(mm, file = outf, col.names = FALSE, row.names = FALSE, sep = ",", na = "")
  }
  ff$variableName <- gsub("^REPEAT\\.$", "REPEATGRADE", ff$variableName, ignore.case = TRUE)
  ff$variableName <- gsub("^REPEAT$", "REPEATGRADE", ff$variableName, ignore.case = TRUE)
  
  # Produce country dictionary
  countryLabels <- unlist(strsplit(ff$labelValues[toupper(ff$variableName) == toupper("cnt")], "\\^"))
  countryDict <- do.call("rbind", strsplit(countryLabels, "="))
  countryDict <- data.frame(countryDict, stringsAsFactors = FALSE)
  colnames(countryDict) <- c("cnt", "country.name")
  countryDict$available <- (tolower(countryDict$cnt) %in% tolower(all_countries))
  cacheFile <- list(
    countryDict = countryDict, dict = ff,
    list_files = names(savFileList),
    ver = packageVersion("EdSurvey"),
    cacheFileVer = 6, ts = Sys.time()
  )
  saveRDS(cacheFile, file.path(filepath, "FIN_all-countries.meta"))
  return(cacheFile)
}

