#' @title Connect to TIMSS Data
#'
#' @description Opens a connection to a TIMSS data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character vector to the full directory path(s) to the TIMSS
#'             extracted SPSS (.sav) set of data
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}
#'                  or other online sources. Consult the \emph{TIMSS User Guide}
#'                  documentation to help determine what countries
#'                  are included within a specific testing year of TIMSS and
#'                  for country code definitions.
#'                  To select all countries available, use a wildcard value of \strong{\code{*}}.
#' @param gradeLvl a character value to indicate the specific grade level you wish to return
#'                 \itemize{
#'                   \item{\strong{4} = fourth grade (the default if not specified)}
#'                   \item{\strong{8} = eighth grade}
#'                   \item{\strong{4B} = fourth grade bridge study (TIMSS 2019 only)}
#'                   \item{\strong{8B} = eight grade bridge study (TIMSS 2019 only)}
#'                 }
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the \code{readTIMSS} function by using existing read-in data already processed.
#'
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' @details
#' Reads in the unzipped files downloaded from the TIMSS international
#' database(s) using the \href{https://www.iea.nl/data-tools/repository}{IEA Study Data Repository}.
#' Data files require the SPSS data file (.sav) format using the default
#' filenames.
#'
#' A TIMSS \code{edsurvey.data.frame} includes three distinct data levels:
#' \itemize{
#'   \item student
#'   \item school
#'   \item teacher
#' }
#'
#' When the \code{getData} function is called using a TIMSS \code{edsurvey.data.frame},
#' the requested data variables are inspected, and it handles any necessary data merges automatically.
#' The \code{school} data always will be returned merged to the \code{student}
#' data, even if only \code{school} variables are requested.
#' If \code{teacher} variables are requested by the \code{getData} call, it
#' will cause \code{teacher} data to be merged.
#' Many \code{students} can be linked to many \code{teachers}, which varies
#' widely between countries.
#'
#' Please note that calling the \code{dim} function for a TIMSS
#' \code{edsurvey.data.frame} will result in the row count as if the
#' \code{teacher} dataset was merged.
#' This row count will be considered the \code{full data N} of the
#' \code{edsurvey.data.frame}, even if no \code{teacher} data were included in an analysis.
#' The column count returned by \code{dim} will be the count of unique column
#' variables across all three data levels.
#'
#' Beginning with TIMSS 2015, a \code{numeracy} dataset was designed to assess
#' mathematics at the end of the primary school cycle
#' for countries where most children are still developing fundamental mathematics skills.
#' The \code{numeracy} dataset is handled automatically for the user and is
#' included within the fourth-grade dataset \code{gradeLvl=4}.
#' Most \code{numeracy} countries have a \code{4th grade} dataset in addition
#' to their \code{numeracy} dataset, but some do not.
#' For countries that have both a \code{numeracy} and a \code{4th grade} dataset,
#' the two datasets are combined into one \code{edsurvey.data.frame} for that country.
#' Data variables missing from either dataset are kept, with \code{NA} values
#' inserted for the dataset records where that variable did not exist.
#' Data variables common to both datasets are kept as a single data variable,
#' with records retaining their original values from the source dataset.
#' Consult the \emph{TIMSS User Guide} for further information.
#' 
#' For the TIMSS 2019 study, a bridge study was conducted to help compute adjustment factors
#' between the electronic test format and the paper/pencil format.  The bridge study is
#' considered separate from the normal TIMSS 2019 study. The \code{gradeLvl} parameter now
#' includes a \code{"4B"} option for the Grade 4 bridge study, and the \code{"8B"} option
#' for the Grade 8 bridge study files.
#' 
#'
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or an \code{edsurvey.data.frame.list} if multiple countries specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{getData}}, and \code{\link{downloadTIMSS}}
#' @author Tom Fink
#'
#' @example \man\examples\readTIMSS.R
#'
#' @importFrom haven read_sav
#' @importFrom utils write.csv
#' @import tibble
#' @export
readTIMSS <- function(path,
                      countries,
                      gradeLvl = c("4", "8", "4b", "8b"),
                      forceReread = FALSE,
                      verbose = TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  #validate the folder path(s) the user specified to make sure they exist
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  path <- ifelse(grepl("[.][a-zA-Z]{1,4}$", path, perl=TRUE, ignore.case=TRUE), dirname(path), path)

  if(!all(dir.exists(path))){
    stop(paste0("The argument ", sQuote("path"), " cannot be located ", pasteItems(dQuote(path[!dir.exists(path)])),"."))
  }

  includeNumeracy <- TRUE #include numeracy as default
  countries <- tolower(unique(countries))

  if(missing(gradeLvl)){
    warning(paste0("The argument ", sQuote("gradeLvl"), " is not specified.  Defaulting to Grade 4."))
    gradeLvl <- 4 #default value
  }
  
  gradeLvl <- tolower(gradeLvl)
  gradeLvl <- match.arg(gradeLvl)
  
  if(length(gradeLvl)!=1){
    stop(paste0("The argument ", sQuote("gradeLvl"), " must be a single value."))
  }
  
  gradeLvl <- tolower(gradeLvl)
  if(!gradeLvl %in% c("4", "8", "4b", "8b")) {
    stop(paste0("The argument ", sQuote("gradeLvl"), " must be a value of 4, 8, 4B, or 8B."))
  }

  gradeL <- ifelse(gradeLvl %in% c("4", "4b"), "a", "b") # a=4th grade; b=8th grade
  
  if(!is.logical(forceReread)){
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if(!is.logical(verbose)){
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }


  if(unlist(countries)[1]=="*"){ #user has requested data for all countries::grab all country codes within all the paths specified
    countries <- unique(tolower(substring(list.files(path,
                                                     pattern=paste0("^", gradeL, ".....(",
                                                     paste(getTIMSSYearCodes(gradeLvl), collapse = "|"),")\\.sav$"), full.names=FALSE, ignore.case = TRUE),4,6)))
  }


  #gather datafile listing::be sure we only pickup TIMSS years based on the yearcodes
  filenames <- list.files(path,
                          pattern=paste0("^", gradeL, "..", "(",paste(countries, collapse="|"), ")(",
                                         paste(getTIMSSYearCodes(gradeLvl), collapse = "|"), ")","\\.sav$"), full.names=TRUE, ignore.case = TRUE)
  if(length(filenames) == 0) {
    stop(paste0("Could not find any TIMSS datafiles for ", sQuote("gradeLvl") , " of ", gradeLvl,
                " for countries ", paste(countries, collapse=", "),
                " in the following folder(s) ", paste(path, collapse=", "), "."))
  }

  fSubPart <- tolower(substring(basename(filenames), 1, 8)) #includes a/b (4 or 8th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames),7,8))))

  #ensure we grab the year code of the base year if only the numeracy code is found for our loop (country only has literacy data)
  yrNum <- unique(convertTIMSSYearCode(fileYrs))
  fileYrs <- getTIMSSYearCodes(gradeLvl)[names(getTIMSSYearCodes(gradeLvl)) %in% yrNum]

  procCountryData <- list()
  iProcCountry <- 0 #index counter for adding countries to the list

  for(yrCode in fileYrs[!(substring(fileYrs,1,1)=="n")]){#loop through all the year codes first::but not numeracy year codes
    for(cntry in countries){ #then loop through all the countries

      TIMSSfiles <- list()#empty list

      if(gradeLvl %in% c("4", "4b")){
        TIMSSfiles <- c("acg", #school background
                        "asa", #student achievement
                        "asg", #student background
                        "ash", #student home background (Special file::might not always be present)
                        "asr", #within-country scoring reliability
                        "ast", #student-teacher linkage
                        "atg") #teacher background
      } else { #assumes "8" or "8b"
        TIMSSfiles <- c("bcg", #School Background
                        "bsa", #Student Achievement
                        "bsg", #Student Background
                        "bsr", #Within-Country Scoring Reliability
                        "bst", #Student-Teacher Linkage
                        "btm", #Teacher Background-Mathematics
                        "bts") #Teacher Background-Science
      }
      
      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnamesNumeracy <- NULL
      yrCodeNumeracy <- unique(fileYrs[substring(fileYrs,1,1)=="n"]) #returns all numeracy codes
      yrCodeNumeracy <- yrCodeNumeracy[convertTIMSSYearCode(yrCodeNumeracy)==convertTIMSSYearCode(yrCode)] #get the numeracy code that only matches our perticular year in the loop
      
      fnames <- sapply(TIMSSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f,cntry, yrCode))] #added check here to only grab files for our specific TIMSS grade level, country, and year
      }, simplify=FALSE)

      fnamesNumeracy <- sapply(TIMSSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f,cntry, yrCodeNumeracy))] #added check here to only grab files for our specific TIMSS grade level, country, and year
      }, simplify=FALSE)

      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g))==0
      }, simplify=TRUE)

      hasMissingNumeracy <- sapply(fnamesNumeracy, function(g) {
        sum(nchar(g))==0
      }, simplify=TRUE)

      if(gradeLvl %in% c("4", "4b")){
        hasMissing["ash"] <- FALSE #special files that might be missing from data::no need to raise error
        hasMissing["asr"] <- FALSE
        hasMissingNumeracy["ash"] <- FALSE
        hasMissingNumeracy["asr"] <- FALSE
      } else { #assumes grade 8 or 8b
        hasMissing["bsr"] <- FALSE
      }

      hasExcess <- sapply(fnames, function(h) {
        length(h)>1
      }, simplify=TRUE)

      hasExcessNumeracy <- sapply(fnamesNumeracy, function(h) {
        length(h)>1
      }, simplify=TRUE)

      hasData <- sum(nchar(unlist(fnames)))>0
      hasDataNumeracy <- FALSE
      if(gradeLvl==4){
        hasDataNumeracy <- sum(nchar(unlist(fnamesNumeracy)))>0
      }

      #includeNumeracy is TRUE as default, but retaining this as future reference
      if(includeNumeracy==FALSE){
        hasDataNumeracy <- FALSE
        fnamesNumeracy <- NULL
        yrCodeNumeracy <- NULL
      }

      #test for any missing files other than the 'ash' or 'asr' file::also check for any duplicate or multiple files
      if (sum(hasMissing)>0 && hasData==TRUE && hasDataNumeracy==FALSE) {
        stop(paste0("Missing TIMSS datafile(s) for country (", cntry, ") ", paste(TIMSSfiles[hasMissing], collapse=", "), "."))
      }
      if (sum(hasExcess)>0 && hasData==TRUE && hasDataNumeracy==FALSE){
        stop(paste0("Excess/duplicate TIMSS datafile(s) for country (", cntry, ") ", paste(TIMSSfiles[hasExcess], collapse=", "), "."))
      }
      if (sum(hasMissingNumeracy)>0 && hasData==FALSE && hasDataNumeracy==TRUE) {
        stop(paste0("Missing TIMSS numeracy datafile(s) for country (", cntry, "): ", paste(TIMSSfiles[hasMissingNumeracy], collapse=", "), "."))
      }
      if (sum(hasExcessNumeracy)>0 && hasData==FALSE && hasDataNumeracy==TRUE){
        stop(paste0("Excess/duplicate TIMSS numeracy datafile(s) for country (", cntry, "): ", paste(TIMSSfiles[hasExcessNumeracy], collapse=", "), "."))
      }
      if ((sum(hasMissing)>0 || sum(hasMissingNumeracy)>0) && hasData==TRUE && hasDataNumeracy==TRUE) {
        stop(paste0("Missing TIMSS datafile(s) for country (", cntry, ") 4th grade files ", paste(TIMSSfiles[hasMissing], collapse=", "), " numeracy files ", paste(TIMSSfiles[hasMissingNumeracy], collapse=", "), "."))
      }
      if ((sum(hasExcess)>0 || sum(hasExcessNumeracy)>0) && hasData==TRUE && hasDataNumeracy==TRUE){
        stop(paste0("Excess/duplicate TIMSS datafile(s) for country (", cntry, ") 4th grade files ", paste(TIMSSfiles[hasExcess], collapse=", "), " numeracy files ", paste(TIMSSfiles[hasExcessNumeracy], collapse=", "), "."))
      }


      #test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (hasData==FALSE && hasDataNumeracy==FALSE) {
         next
      }

      iProcCountry <- iProcCountry + 1 #update the processed country index value after we confirm that there is data to process
      processedData <- list()

      if(gradeLvl %in% c("4", "4b")){
        if(hasData==TRUE){ #process our 4th grade data::run event if we have both sets
          processArgs <- list(dataFolderPath=unique(dirname(unlist(fnames))), #specify only the directory in which the files exist
                              countryCode=cntry,
                              fnames=fnames,
                              fileYrs=yrCode,
                              forceReread=forceReread,
                              verbose=verbose)

          retryProc <- tryCatch({processedData <- do.call("processTIMSSGr4", processArgs, quote = TRUE)
                                  FALSE
                                }, error = function(e){
                                  TRUE #flag to retry
                                }, warning = function(w){
                                  TRUE #flat to retry
                                })

          if (retryProc){
            processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
            processedData <- tryCatch(do.call("processTIMSSGr4", processArgs, quote = TRUE),
                                      error = function(e){
                                        stop(paste0("Unable to process TIMSS 4th grade data for country code ", sQuote(cntry),
                                                      " having year code ", sQuote(yrCode) ," at folder path(s) ", paste(sQuote(path), collapse = " & "),
                                                      ". Possible file corruption with source data.",
                                                      " Error message: ", e))
                                      })
          }

        }
        if(hasDataNumeracy==TRUE){ #process our numeracy data only
          processArgs <- list(dataFolderPath=unique(dirname(unlist(fnamesNumeracy))), #specify folders where data resides
                              countryCode=cntry,
                              fnames=fnamesNumeracy,
                              fileYrs=yrCodeNumeracy,
                              forceReread=forceReread,
                              verbose=verbose)

          retryProc <- tryCatch({processedData <- do.call("processTIMSSGr4", processArgs, quote = TRUE)
                                  FALSE
                                }, error = function(e){
                                  TRUE #flag to retry
                                }, warning = function(w){
                                  TRUE #flag to retry
                                })

          if (retryProc){
            processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
            processedData <- tryCatch(do.call("processTIMSSGr4", processArgs, quote = TRUE),
                                      error = function(e){
                                        stop(paste0("Unable to process TIMSS numeracy data for country code ", sQuote(cntry),
                                                    " having year code ", sQuote(yrCodeNumeracy) ," at folder path(s) ", paste(sQuote(path), collapse = " & "),
                                                    ". Possible file corruption with source data.",
                                                    " Error message: ", e))
                                      })
          }
        }
        if(hasDataNumeracy==TRUE && hasData==TRUE){#process both 4th and numeracy, then merge together
          processArgs <- list(dataFolderPath=unique(c(dirname(unlist(fnames)), dirname(unlist(fnamesNumeracy)))), #specify folders where data resides
                              countryCode=cntry,
                              fnames=fnames,
                              fnamesNumeracy = fnamesNumeracy,
                              fileYrs=paste0(yrCode, yrCodeNumeracy), #yearcode will be a combination of both year codes
                              forceReread=forceReread,
                              verbose=verbose)

          retryProc <- tryCatch({processedData <- do.call("processTIMSS4AndNumeracy", processArgs, quote = TRUE)
                                  FALSE
                                },error = function(e){
                                  TRUE #flag to retry
                                }, warning = function(w){
                                  TRUE #flag to retry
                                })

          if (retryProc){
            processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
            processedData <- tryCatch(do.call("processTIMSS4AndNumeracy", processArgs, quote = TRUE),
                                      error = function(e){
                                        stop(paste0("Unable to process TIMSS combined 4th grade and numeracy data for country code ", sQuote(cntry),
                                                    " having year code ", sQuote(paste0(yrCode, yrCodeNumeracy)) ," at folder path(s) ", paste(sQuote(path), collapse = " & "),
                                                    ". Possible file corruption with source data.",
                                                    " Error message: ", e))
                                      })
          }
        }
      }else { #8th grade or 8th grade bridge data
        processArgs <- list(dataFolderPath=unique(dirname(unlist(fnames))), #specify folders where data resides
                            countryCode=cntry,
                            fnames=fnames,
                            fileYrs=yrCode,
                            forceReread=forceReread,
                            verbose=verbose)

        retryProc <- tryCatch({processedData <- do.call("processTIMSSGr8", processArgs, quote = TRUE)
                                FALSE
                              }, error = function(e){
                                TRUE #flag to retry
                              }, warning = function(w){
                                TRUE #flag to retry
                              })

        if (retryProc){
          processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
          processedData <- tryCatch(do.call("processTIMSSGr8", processArgs, quote = TRUE),
                                    error = function(e){
                                      stop(paste0("Unable to process TIMSS 8th grade data for country code ", sQuote(cntry),
                                                  " having year code ", sQuote(yrCode) ," at folder path(s) ", paste(sQuote(path), collapse = " & "),
                                                  ". Possible file corruption with source data.",
                                                  " Error message: ", e))
                                    })
        }
      }#end if(gradeLvl == 4)

        processedData$userConditions <- list()
        processedData$defaultConditions <- NULL
        processedData$data <- processedData$dataList$student
        processedData$dataSch <- processedData$dataList$school
        processedData$dataTch <- processedData$dataList$teacher

        processedData$dataListMeta <- processedData$dataListMeta

        testJKprefix <- c("JK", "JK.TCHWGT", "JK.MATWGT", "JK.SCIWGT") #have any jk prefix values here that are applicable for this dataset
        weights <- NULL #default value

        for(i in 1:length(testJKprefix)){
          ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix[i] ,")","[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values

          if(length(ujkz)>0){
            if(testJKprefix[i]=="JK"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="jk", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgt"
              weights <- c(weights,tmpWgt)
            }
            if(testJKprefix[i]=="JK.TCHWGT"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="jk.tchwgt", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "tchwgt"
              weights <- c(weights,tmpWgt)
            }
            if(testJKprefix[i]=="JK.MATWGT"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="jk.matwgt", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "matwgt"
              weights <- c(weights,tmpWgt)
            }
            if(testJKprefix[i]=="JK.SCIWGT"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="jk.sciwgt", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "sciwgt"
              weights <- c(weights,tmpWgt)
            }
          }
        }
        #retained totwgt as default.  getData call will warn if 'totwgt' is requested when teacher data is merged
        attr(weights, "default") <- "totwgt"

        processedData$weights <-  weights
        processedData$pvvars <- buildPVVARS_TIMSS(processedData$dataListFF$student, defaultPV = "mmat")
        processedData$subject <- c("Mathematics", "Science")
        processedData$year <- convertTIMSSYearCode(yrCode)
        processedData$assessmentCode <- "International"
        processedData$dataType <- "Student Data"
        processedData$gradeLevel <- ifelse(gradeLvl=="4", "Grade 4",
                                           ifelse(gradeLvl=="8","Grade 8",
                                           ifelse(gradeLvl=="4b", "Grade 4 - Bridge Study",
                                           ifelse(gradeLvl=="8b", "Grade 8 - Bridge Study", ""))))

        #Check if these are consistant between all of the years of data
        processedData$achievementLevels <- c("625", "550", "475", "400")
        names(processedData$achievementLevels) <- c("Advanced International Benchmark", "High International Benchmark", "Intermediate International Benchmark", "Low International Benchmark")

        processedData$omittedLevels <- c('Multiple', NA,
                                         'OMITTED', 'OMITTED OR INVALID', 'OMITTED (BLANK ONLY)', 'BLANK(OMITTED)',
                                         'BLANK(MISSING)', 'BLANK(MISS)', 'MIS.','MISS.',
                                         'N. REA.', 'N.REA.', 'N. ADM.','N. ADMIN.', 'NOT ADMIN', 'NOT APPL','NOT ADMINISTERED',
                                         'NOT REACHED', 'NOT ADMIN.', 'NOT APPLICABLE', 'LOGICALLY NOT APPLICABLE',
                                         'MISSING', '(Missing)')


        processedData$survey <- ifelse(gradeLvl %in% c("4b", "8b"), "TIMSS - Bridge Study", "TIMSS")
        processedData$country <- getTIMSSCountryName(cntry)

        procCountryData[[iProcCountry]] <- edsurvey.data.frame(userConditions = processedData$userConditions,
                                                               defaultConditions = processedData$defaultConditions,
                                                               dataList = buildTIMSS_dataList(processedData$dataList$student,
                                                                                              processedData$dataListFF$student,
                                                                                              processedData$dataList$school,
                                                                                              processedData$dataListFF$school,
                                                                                              processedData$dataList$teacher,
                                                                                              processedData$dataListFF$teacher),
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
                                                               jkSumMultiplier = 0.5, #defined by the method of JK weight replication used (JK2)
                                                               reqDecimalConversion = FALSE,
                                                               dim0=processedData$dim0) 

        processedData <- NULL
    }#end for(cntry in countries)
  }#end for(fileYr in fileYrs)

  if (iProcCountry > 1) {
    return(edsurvey.data.frame.list(procCountryData)) #do not apply country labels::the edsurvey.data.frame.list does a better job of detecting the differences
  } else {
    # just one country
    return(procCountryData[[1]])
  }
}

#@param yrCode a character value used in the TIMSS filenaming structure to identify the specific year (e.g. m1, m2, m6)
#@return a numeric 4 digit year value
convertTIMSSYearCode <- function(yrCode){

  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% "m1"] <- 1995
  yrTest[yrTest %in% "m2"] <- 1999
  yrTest[yrTest %in% "m3"] <- 2003
  yrTest[yrTest %in% "m4"] <- 2007
  yrTest[yrTest %in% "m5"] <- 2011
  yrTest[yrTest %in% c("m6", "n1")] <- 2015
  yrTest[yrTest %in% "m7"] <- 2019
  yrTest[yrTest %in% "b7"] <- 2019

  return(yrTest)
}

getTIMSSYearCodes <- function(gradeLvl){
  #retrieve the TIMMS years based on their filenaming structure

  yrVals <- NULL

  if(gradeLvl=="4"){
    yrVals = c("m1","m2","m3", "m4", "m5", "m6", "n1", "m7") #n1 is for numeracy data::only applicable to 4th grade
    names(yrVals) = c(1995, 1999, 2003, 2007, 2011, 2015, 2015, 2019)
  } else if(gradeLvl=="8"){
    yrVals = c("m1","m2","m3", "m4", "m5", "m6", "m7")
    names(yrVals) = c(1995, 1999, 2003, 2007, 2011, 2015, 2019)
  } else if(gradeLvl=="4b"){
    yrVals = c("b7")
    names(yrVals) = c(2019)
  } else if(gradeLvl=="8b"){
    yrVals = c("b7")
    names(yrVals) = c(2019)
  }
  
  return(yrVals)
}

#builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_TIMSS <- function(fileFormat, defaultPV = "mmat"){
  
  pvFields <- subset(fileFormat, nchar(fileFormat$Type)>0) #type is identified in writeTibbleToFWFReturnFileFormat function
  constructs <- unique(pvFields$Type)
  
  #drop the international benchmark contructs as they are not true plausible values, only discrete numerics
  constructs <- constructs[!grepl("^(s|m)ibm$", constructs, ignore.case = TRUE)]
  
  pvvars <- vector("list", length(constructs))
  names(pvvars) <- constructs

  for(i in names(pvvars)){
    varList <- tolower(sort(pvFields$variableName[pvFields$Type == i]))
    pvvars[[i]] <- list(varnames=varList)
  }

  #test if defaultPV in the list and make it default::otherwise set it to the first pvvar in the list
  if (defaultPV %in% names(pvvars)){
    attr(pvvars, "default") <- defaultPV
  }else{
    attr(pvvars, "default") <- names(pvvars)[1]
  }

  return (pvvars)
}


#@param dataFolderPath a character value of the initial folder path provided to the 'readTIMMS' call to find the TIMSS .sav SPSS files
#@param countryCode a character value of the 3-digit country code we want to process
#@param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
#@param fileYrs are the specific filename year code convention (e.g., m1, m2, m3)
#@param forceReread to force processing even if cache metadata is present
#@param verbose to know if we want verbose output or not
processTIMSSGr4 <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {

  yearCode <- unlist(fileYrs)[1]

  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^a", "(",paste(countryCode), ")",
                            yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)

  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^a..", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)

  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE){
    runProcessing <- TRUE #determine if we need to process data or not
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

      if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "TIMSS")){
        runProcessing <- TRUE
      }else{
        #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
        studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="asg"], cacheFile$dataListFF$student)
        schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="acg"], cacheFile$dataListFF$school)
        teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="atg"], cacheFile$dataListFF$teacher)

        dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF)
        dataListFF <- cacheFile$dataListFF
        dataListMeta <- cacheFile$dataListMeta

        dim0 <- cacheFile$dim0
        
        runProcessing <- FALSE
      }
  }#end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  if(runProcessing==TRUE){

    if(verbose==TRUE){
      cat(paste0("Processing data for country ", dQuote(countryCode),".\n"))
    }
    
    #delete the .meta file (if exists) before processing in case of error/issue
    if(length(metaCacheFP)>0 && file.exists(metaCacheFP)){
      file.remove(metaCacheFP)
    }

    #SCHOOL LEVEL===================================================
    acg <- unlist(fnames["acg"])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["acg"])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(acg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP)
    
    #free up memory
    schoolDF1 <- NULL
    #===============================================================

    #STUDENT LEVEL==================================================
    #ensure we only have one file to work with for each type otherwise record duplication errors may occur
    asa <- unlist(fnames["asa"])[1]
    asg <- unlist(fnames["asg"])[1]
    ash <- unlist(fnames["ash"])[1]
    asr <- unlist(fnames["asr"])[1]

    stuDF1 <- read_sav(asa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case=TRUE, value=TRUE)

    stuDF2 <- read_sav(asg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)
    colnames(stuDF2) <- toupper(colnames(stuDF2))

    ids2 <- grep("^ID", names(stuDF2), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

    mm <- mergeTibble(stuDF1,
                      stuDF2,
                      by=ids12,
                      all.x=FALSE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]

    if(nrow(stuDF1) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }
    if(nrow(stuDF2) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }
    
    
    #test to ensure the ash filepath is not NA (0==FALSE (Have file) | 1==TRUE (No File))
    if(min(is.na(ash)) == 0) {
      stuDF3 <- read_sav(ash, user_na = TRUE)
      stuDF3 <- UnclassCols(stuDF3)
      colnames(stuDF3) <- toupper(colnames(stuDF3))

      ids3 <- grep("^ID", names(stuDF3), ignore.case=TRUE, value=TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF3,
                        by=idsmm3,
                        all.x=TRUE,
                        all.y=FALSE,
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nrow(stuDF1) != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
    } else {
      idsmm3 <- ids12
    }
    
    #free up memory
    stuDF1 <- NULL
    stuDF2 <- NULL
    stuDF3 <- NULL
    
    if(min(is.na(asr)) == 0){
      stuDF4 <- read_sav(asr, user_na = TRUE)
      stuDF4 <- UnclassCols(stuDF4)
      colnames(stuDF4) <- toupper(colnames(stuDF4))
      ids4 <- grep("^ID", names(stuDF4), ignore.case=TRUE, value=TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      idsmm4 <- idsmm4[!(idsmm4 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF4,
                        by=idsmm4,
                        all.x=TRUE,
                        all.y=FALSE,
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      
      #free up memory
      stuDF4 <- NULL
    }

    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames["asg"])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)
    #===============================================================

    #Student-Teacher Linkage and Teacher Background=================
    ast <- unlist(fnames["ast"])[1]
    atg <- unlist(fnames["atg"])[1]

    stuTeachDF <- read_sav(ast, user_na = TRUE)
    stuTeachDF <- UnclassCols(stuTeachDF)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))

    teachDF <- read_sav(atg, user_na = TRUE)
    teachDF <- UnclassCols(teachDF)
    colnames(teachDF) <- toupper(colnames(teachDF))

    ids1 <- grep("^ID", names(stuTeachDF), ignore.case=TRUE, value=TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

    mm <- mergeTibble(stuTeachDF,
                      teachDF,
                      by=ids12,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]

    if(nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("ast"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }

    teachFP <- gsub(".sav$", "\\.txt", unlist(fnames["atg"])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    
    #free up memory
    stuTeachDF <- NULL
    #===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)

    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF)
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    #===============================================================
    
    #save the cachefile to be read-in for the next call
    #save the nrow0 to speed up getting dim0 when constructing edsurvey.data.frame
    
    nrow0 <- nrow(mm)
    ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, ffTeach$variableName)))
    dim0 <- c(nrow0, ncol0)
    
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=5,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dim0=dim0)

    saveRDS(cacheFile, file.path(dataFolderPath,paste0("a", countryCode, yearCode,".meta")))

    #===============================================================
  } else {
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),".\n"))
    }
  }#end if(runProcessing==TRUE)

  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dim0=dim0))

}

#@param dataFolderPath a character value of the initial folder path provided to the 'readTIMMS' call to find the TIMSS .sav SPSS files
#@param countryCode a character value of the 3-digit country code we want to process
#@param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
#@param fileYrs are the specific filename year code convention (e.g., m1, m2, m3)
#@param forceReread to force processing even if cache metadata is present
#@param verbose to know if we want verbose output or not
processTIMSSGr8 <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {

  yearCode <- unlist(fileYrs)[1]

  metaCacheFP <- list.files(dataFolderPath,
                                         pattern=paste0("^b", "(",paste(countryCode), ")",
                                                        yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)
  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^b..", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)

  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE){ #ensure we have a full set of cache files
    runProcessing <- TRUE
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "TIMSS")){
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bsg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bcg"], cacheFile$dataListFF$school)
      teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bst"], cacheFile$dataListFF$teacher)

      dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF
      
      dim0 <- cacheFile$dim0

      runProcessing <- FALSE
    }
  } #end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  if(runProcessing==TRUE){

    if(verbose==TRUE){
      cat(paste0("Processing data for country ", dQuote(countryCode),".\n"))
    }
    
    #delete the .meta file (if exists) before processing in case of error/issue
    if(length(metaCacheFP)>0 && file.exists(metaCacheFP)){
      file.remove(metaCacheFP)
    }

    #SCHOOL LEVEL===================================================
    bcg <- unlist(fnames["bcg"])[1]

    schoolDF1 <- read_sav(file=bcg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)

    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["bcg"])[1], ignore.case = TRUE)
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1,  schoolFP)

    #cleanup
    schoolDF1 <- NULL
    #===============================================================

    #STUDENT LEVEL==================================================
    bsa <- unlist(fnames["bsa"])[1]
    bsg <- unlist(fnames["bsg"])[1]
    bsr <- unlist(fnames["bsr"])[1]

    stuDF1 <- read_sav(bsa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)

    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case=TRUE, value=TRUE)

    stuDF2 <- read_sav(bsg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)

    colnames(stuDF2) <- toupper(colnames(stuDF2))
    ids2 <- grep("^ID", names(stuDF2), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

    mm <- mergeTibble(stuDF1,
                      stuDF2,
                      by=ids12,
                      all.x=FALSE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]

    if(nrow(stuDF1) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("bsa"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    if(nrow(stuDF2) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("bsg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }

    #cleanup
    stuDF1 <- NULL
    stuDF2 <- NULL

    if(min(is.na(bsr)) == 0){

      stuDF3 <- read_sav(bsr, user_na = TRUE)
      stuDF3 <- UnclassCols(stuDF3)

      colnames(stuDF3) <- toupper(colnames(stuDF3))
      ids3 <- grep("^ID", names(stuDF3), ignore.case=TRUE, value=TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF3,
                        by=idsmm3,
                        all.x=TRUE,
                        all.y=FALSE,
                        suffixes=c("", ".junk"))

      mm <- mm[ , names(mm)[!grepl("\\.junk$",names(mm))]]

      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("bsr"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
    }

    #cleanup
    stuDF3 <- NULL

    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames["bsg"])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)
    #===============================================================

    #Student-Teacher Linkage to math and science teachers=============
    bst <- unlist(fnames["bst"])[1]
    btm <- unlist(fnames["btm"])[1]
    bts <- unlist(fnames["bts"])[1]

    stuTeachDF <- read_sav(bst, user_na = TRUE)
    stuTeachDF <- UnclassCols(stuTeachDF)

    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))
    ids1 <- grep("^ID", names(stuTeachDF), ignore.case=TRUE, value=TRUE)

    teachMathDF <- read_sav(btm, user_na = TRUE)
    teachMathDF <- UnclassCols(teachMathDF)

    colnames(teachMathDF) <- toupper(colnames(teachMathDF))
    ids2 <- grep("^ID", names(teachMathDF), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging
    
    #add a '.math' postfix to the variable name here for a math specific teacher variable as some varnames are duplicated between math/sci teacher files
    colnames(teachMathDF) <- sapply(toupper(trimws(names(teachMathDF))), function(tempName){
        if(!(tempName %in% toupper(trimws(names(stuTeachDF))))){
          return(paste0(tempName, ".MATH"))
        } else {
          return(paste0(tempName))
        }
    })
    
    mm <- mergeTibble(stuTeachDF,
                      teachMathDF,
                      by=ids12,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))

    mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

    if(nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("btm"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }

    #cleanup
    stuTeachDF <- NULL
    teachMathDF <- NULL

    teachSciDF <- read_sav(bts, user_na = TRUE)
    teachSciDF <- UnclassCols(teachSciDF)

    colnames(teachSciDF) <- toupper(colnames(teachSciDF))
    ids3 <- grep("^ID", names(teachSciDF), ignore.case=TRUE, value=TRUE)
    idsmm3 <- ids1[ids1 %in% ids3]
    idsmm3 <- idsmm3[!(idsmm3 %in% c("IDGRADE", "IDGRADER", "IDPUNCH"))] #omit these vars for merging

    #add a '.sci' postfix to the variable name here for a math specific teacher variable as some varnames are duplicated between math/sci teacher files
    names(teachSciDF) <- sapply(toupper(trimws(names(teachSciDF))), function(tempName){
      if(!(tempName %in% toupper(trimws(names(mm))))){
        return(paste0(tempName, ".SCI"))
      } else {
        return(paste0(tempName))
      }
    })

    nr <- nrow(mm)
    mm <- mergeTibble(mm,
                      teachSciDF,
                      by=idsmm3,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    if(nr != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("bts"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }

    stuTeachFP <- gsub(".sav$", "\\.txt", unlist(fnames["bst"])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, stuTeachFP)

    #cleanup
    teachSciDF <- NULL
    #===============================================================

    #gather the LaF connections to return
    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teachLAF <- getFWFLaFConnection(stuTeachFP, ffTeach)

    #build data list and file format list===========================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teachLAF)
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    #===============================================================

    #calc the dim0 to store in the .meta file for fast retrieval
    nrow0 <- nrow(mm)
    ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, ffTeach$variableName)))
    dim0 <- c(nrow0, ncol0)
    
    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=5,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dim0=dim0)

    saveRDS(cacheFile, file.path(unlist(dataFolderPath)[1],paste0("b", countryCode, yearCode,".meta")))
    #===============================================================
  } else {
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),".\n"))
    }
  }

  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dim0 = dim0))

}


#when a tibble is merged it comes back as a data.frame instead of a tibble, this converts the data.frame back to tibble
mergeTibble <- function(a, b, by,  ..., suffixes=c("", ".junk")) {

  suffixes[1] <- ""

  ab <- merge(a, b, by=by, ..., suffixes=suffixes)

  cols <- names(ab)
  abt <- as_tibble(ab)

  for(i in 1:length(cols)) {
    coli <- cols[i]
    abcoli <- abt[[coli]]
    if(coli %in% names(a) | coli %in% names(b)) {
      if(coli %in% names(a)) {
        ocoli <- a[[coli]]
      } else {
        ocoli <- b[[coli]]
      }
      newAtrs <- attributes(ocoli)
      oldAnames <- names(attributes(abcoli))
      transname <- names(newAtrs)
      transname <- transname[!transname %in% oldAnames]
      for(tri in 1:length(transname)) {
        if((!is.null(transname[tri])) && (!is.na(transname[tri])) && (length(transname[tri])>0)){
          attr(abcoli, transname[tri]) <- newAtrs[[transname[tri]]]
        }
      }
      abt[[coli]] <- abcoli
    }
  }
  abt
}

#writes a tibble object to a fixed-width-format (fwf) datafile, compiles the FileFormat detail of the fwf into a data.frame of needed info
# contributor: Jeppe Bundsgaard: updates for ICILS 2018
writeTibbleToFWFReturnFileFormat <- function(spssDF, outF, jkType=c("JK2", "JK1")) {
  if(!inherits(spssDF, "tbl_df")) {
    stop("spssDF must be a tibble")
  }

  fileFormat <- getSPSSFileFormat(spssDF) #from readUTILS.R file
  spssDF <- data.frame(spssDF) #convert to data.frame to mitigate any tibble issues
  
  #ensure lower case for matching
  fileFormat$variableName <- tolower(fileFormat$variableName)
  colnames(spssDF) <- tolower(colnames(spssDF))
  jkType = jkType[1] #JK2 will be default unless otherwise specified

  #define any weight vars here
  testWgt <- c("^TOTWGT[S]{0,1}$", "^TCHWGT$", "^MATWGT$", "^SCIWGT$", "^TOTWGTT$", "^TOTWGTCH$", "^PHYWGT$")
  testJKRep <- c("^JKREP[S]{0,1}$", "^JKREP$", "^JKREP$", "^JKREP$", "^JKREPT$", "^JKREP$", "^JKREP$")
  testJKZone <- c("^JKZONE[S]{0,1}$", "^JKZONE$", "^JKZONE$", "^JKZONE$", "JKZONET$", "^JKZONE$", "^JKZONE$")
  testJKprefix <- c("JK", "JK.TCHWGT", "JK.MATWGT", "JK.SCIWGT", "JK.TOTWGTT", "JK.TOTWGTCH", "JK.PHYWGT")

  jkvars <- c() #holder for all JK vars added

  #loop through enough times to search for totwgt, tchwgt, matwgt, or sciwgt and build their JK weights
  for(iWgt in 1:length(testWgt)){
    wgtc <- grep(testWgt[iWgt], fileFormat$variableName, ignore.case = TRUE, value=TRUE)
    jkrepc <- grep(testJKRep[iWgt], fileFormat$variableName, ignore.case = TRUE, value=TRUE)
    jkzonec <- grep(testJKZone[iWgt], fileFormat$variableName, ignore.case = TRUE, value=TRUE)

    if(length(wgtc)==1 && length(jkrepc)==1 && length(jkzonec)==1) {
      weight <- data.frame(spssDF[ , wgtc, drop=FALSE])
      jkrep <- data.frame(spssDF[ , jkrepc, drop=FALSE])
      jkzone <- data.frame(spssDF[ , jkzonec, drop=FALSE])
      ujkz <- sort(unique(jkzone[,1]))

      #JK2 is the default, this applies to TIMSS, TIMSSAdv, and PIRLS
      #JK2 and JK1 differ by how the replicate weights are constructed
      if (jkType=="JK2"){
        for(i in ujkz) {
          #first group - calc
          coli <- paste0(testJKprefix[iWgt],-1+i*2)
          jkvars <- c(jkvars, coli)
          jkw <- weight
          jkw[jkzone == i & jkrep == 0] <- 0
          jkw[jkzone == i & jkrep == 1] <- 2 * jkw[jkzone == i & jkrep == 1]
          jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
          
          #add it to the fileFormat and the data
          spssDF <- cbind(spssDF, jkw[,1])
          ffAppend <- subset(fileFormat, fileFormat$variableName==wgtc)
          ffAppend$variableName <- coli
          ffAppend$Labels <- paste0(ffAppend$Labels, " - ", coli)
          fileFormat <- rbind(fileFormat, ffAppend)

          # and the second group
          coli <- paste0(testJKprefix[iWgt],i*2)
          jkvars <- c(jkvars, coli)
          jkw <- weight
          jkw[jkzone == i & jkrep == 0] <- 2 * jkw[jkzone == i & jkrep == 0]
          jkw[jkzone == i & jkrep == 1] <- 0
          jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
          
          #add it to the fileFormat and data
          spssDF <- cbind(spssDF, jkw[,1])
          ffAppend <- subset(fileFormat, fileFormat$variableName==wgtc)
          ffAppend$variableName <- coli
          ffAppend$Labels <- paste0(ffAppend$Labels, " - ", coli)
          fileFormat <- rbind(fileFormat, ffAppend)
        }
      }else if(jkType=="JK1"){
        
        ujkz <- 1:75 #IEA JK1 datasets (ICCS/CivED and ICILS) always have 75 jk replicate weights regardless of actual zones

        for(i in ujkz) {
          coli <- paste0(testJKprefix[iWgt], i)
          jkvars <- c(jkvars, coli)
          jkw <- weight
          jkw[jkzone == i & jkrep == 0] <- 0
          jkw[jkzone == i & jkrep == 1] <- 2 * jkw[jkzone == i & jkrep == 1]
          jkw[is.na(jkw[,1]),1] <- 0 #turn any NAs to a zero value
          
          #add it to the fileFormat and data
          spssDF <- cbind(spssDF, jkw[,1])
          ffAppend <- subset(fileFormat, fileFormat$variableName==wgtc)
          ffAppend$variableName <- coli
          ffAppend$Labels <- paste0(ffAppend$Labels, " - ", coli)
          fileFormat <- rbind(fileFormat, ffAppend)
        }
      }
    }
  }

  #test for any specialty *dash* (em-dash, en-dash) characters in the value labels that may be confusing for users and replace them with a regular dash character
  fileFormat$labelValues <- gsub("\u2013", "-", fileFormat$labelValues) #replace en-dash (\u2013) with regular dash (\u002d)
  fileFormat$labelValues <- gsub("\u2014", "-", fileFormat$labelValues) #replace em-dash (\u2014) with regular dash (\u002d)
  fileFormat$labelValues <- gsub("\\", "/", fileFormat$labelValues, fixed=TRUE) #switch \\ to /

  ## for replicate weights it is the jackknife replicate weight number
  ## for plausible value variables it is the index within the construct
  fileFormat$pvWT <- sapply(fileFormat$variableName, function(zz){
    if(grepl("[ABMPabmp][Ss][MSPRmspr]...[0][1-5]", zz, ignore.case = TRUE)){
      return(substring(zz,8,8)) #has two-digits, remove the leading 0::PVs are 1-5 for the measures
    }
    else if(grepl("^PV[1-5]CI[LV]$", zz, ignore.case = TRUE)){ #Specific For ICILS (PV1CIL) single PV measure and ICCS (PV1CIV) single PV measure, and for ICILS 2018 (PV1CT)
      return(substring(zz,3,3))
    }
    else {
      if(grepl(paste0("^","(",paste(testJKprefix, collapse="|") ,")","[0-9]*"), zz, ignore.case=TRUE)) {
        return(sub(paste0("^", "(", paste(testJKprefix, collapse="|"),")"), "", zz))
      }
      else if(grepl("^[ST]RWGT[0-9]*", zz, ignore.case=TRUE)){ #specific for ICILS Student/Teacher JK weights; student=SRWGT##, taecher=TRWGT##
        return(substring(zz,6,nchar(zz)))
      }
      else {
        return ("")
      }
    }
  })

  ## for plausible value variables it is the name of the construct it regards
  #Gr4: Overall - MAT = Mathematics; SCI = Science
  #Gr4: Content - NUM = Number; LIF = Life Science; GEO = Geometric Shapes and Measurement; PHY = Physical Science; DAT = Data Display; EAR = Earth Science
  #GR4: Cognitive - KNO = Knowing; APP = Applying; REA = Reasoning
  #GR4: International Benchmark - IBM = International Benchmark
  #Gr8: Overall - MAT = Mathematics; SCI = Science
  #Gr8: Content - NUM = Number; BIO = Biology; ALG = Algebra; CHE = Chemistry; GEO = Geometry; PHY = Physics; DAT = Data and Chance; EAR = Earth Science
  #GR8: Cognitive - KNO = Knowing; APP = Applying; REA = Reasoning
  #GR8: International Benchmark - IBM = International Benchmark

  #first char is either a or b indicating grade level
  #second char is 's' for student
  #third char is 'm' or 's' for math or science
  #char 4-6 is the construct (see above)
  #char 7-8 is the numeric 01-05 designation of the pausible value
  fileFormat$Type <- sapply(fileFormat$variableName, function(zzz){
    if(grepl("[ABMPabmp][Ss][MSPREmspre]...[0][1-5]", zzz, ignore.case = TRUE)==TRUE){
      return(substring(tolower(zzz),3,6))
    }
    else if(grepl("^PV[1-5](CI[LV]|CT)$", zzz, ignore.case = TRUE)==TRUE){ #specific for ICILS (PV1CIL, PV1CT) single PV measure and ICCS (PV1CIV) single PV measure
      return(substring(tolower(zzz),4,6))
    }
    else {
      return ("")
    }
  })
  #

  #be sure to mark all of the weight vars as TRUE in the fileFormat
  for(i in 1:length(testWgt)){
    wgtVar <- grepl(testWgt[i], fileFormat$variableName, ignore.case = TRUE)
    fileFormat$weights[wgtVar==TRUE] <- TRUE
  }
  
  #recalc start/stop positions with our newly added JK replicates
  fileFormat$Start <- c(1,1 + cumsum(fileFormat$Width))[1:nrow(fileFormat)]
  fileFormat$End <- cumsum(fileFormat$Width)
  
  #lastly write out the file and return the updated fileFormat, these functions are in the readUTILS.R file
  fileFormat <- validateFWF_FileFormat(fileFormat)
  fileFormat <- writeDF_FWF(spssDF, fileFormat, outF, verbose = FALSE)
  return(fileFormat)
}

#returns an LaF to the fwf file using the fileformat specs
getFWFLaFConnection <- function(fwfFilePath, fwfFileFormat){
  laf <- LaF::laf_open_fwf(fwfFilePath, column_types = fwfFileFormat$dataType, column_widths = fwfFileFormat$Width, column_names = tolower(fwfFileFormat$variableName))
}


exportTIMSSToCSV <- function(folderPath, exportPath, cntryCodes, gradeLvl, ...){

  sdfList <- readTIMSS(folderPath, cntryCodes, gradeLvl, ...)

  if (inherits(sdfList, "edsurvey.data.frame.list")) {
    for(i in 1:length(sdfList$datalist)){

      sdf  <- sdfList$datalist[[i]]
      cntry <- sdf$country

      cat(paste(cntry, "working.\n"))
      data <- getData(sdf, colnames(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)


      write.csv(data, file=file.path(exportPath, paste0(cntry, ".csv")), na="", row.names = FALSE)
      cat(paste(cntry, "completed.\n"))
    }
  } else if (inherits(sdfList, "edsurvey.data.frame")) {

    sdf <- sdfList
    cntry <- sdf$country

    cat(paste(cntry, "working.\n"))
    data <- getData(sdf, colnames(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)

    write.csv(data, file=file.path(exportPath, paste0(cntry, ".csv")), na="", row.names = FALSE)
    cat(paste(cntry, "completed.\n"))
  }

}

#@param dataFolderPath a character value of the initial folder path provided to the 'readTIMMS' call to find the TIMSS .sav SPSS files
#@param countryCode a character value of the 3-digit country code we want to process
#@param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
#@param fileYrs are the specific filename year code convention (e.g., m1, m2, m3)
#@param forceReread to force processing even if cache metadata is present
#@param verbose to know if we want verbose output or not
processTIMSS4AndNumeracy <- function(dataFolderPath, countryCode, fnames, fnamesNumeracy, fileYrs, forceReread, verbose){

  yearCode <- unlist(fileYrs)[1] #the yearcode should be sent joined together such as (m6n1 for 2015)

  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^a", "(",paste(countryCode), ")",
                                           yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)

  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^a..", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)

  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE){
    runProcessing <- TRUE #determine if we need to process data or not
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])

    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "TIMSS")){
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="asg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="acg"], cacheFile$dataListFF$school)
      teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="atg"], cacheFile$dataListFF$teacher)

      dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF

      dim0 <- cacheFile$dim0

      runProcessing <- FALSE
    }
  }#end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)

  if(runProcessing==TRUE){

    if(verbose==TRUE){
      cat(paste0("Processing data for country ", dQuote(countryCode),".\n"))
    }
    
    #delete the .meta file (if exists) before processing in case of error/issue
    if(length(metaCacheFP)>0 && file.exists(metaCacheFP)){
      file.remove(metaCacheFP)
    }

    #SCHOOL LEVEL===================================================
    acg <- unlist(fnames["acg"])[1]
    acgN <- unlist(fnamesNumeracy["acg"])[1]

    schoolFP <- file.path(dataFolderPath,paste0("acg", countryCode, yearCode, ".txt"))

    schoolDF1 <- read_sav(acg, user_na = TRUE)
    schoolDF1 <- UnclassCols(schoolDF1)
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))

    schoolDF1Num <- read_sav(acgN, user_na = TRUE)
    schoolDF1Num <- UnclassCols(schoolDF1Num)

    colnames(schoolDF1Num) <- toupper(colnames(schoolDF1Num))

    schoolDFx <- rBindTibble(schoolDF1, schoolDF1Num)

    if(anyDuplicated(schoolDFx)>0){
      schoolDFx <- dropTibbleDupes(schoolDFx) #removes any duplicates as it appears the school level files are identical for both 4 and numeracy datasets
    }

    if(nrow(schoolDF1)!=nrow(schoolDFx)){
      #test that the school level is consistent between 4th grade and numeracy fields
    }else{ #merge was good
      schoolDF1 <- schoolDFx
      schoolDFx <- NULL
    }

    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP)

    schoolDF1 <- NULL
    schoolDF1Num <- NULL
    schoolDFx <- NULL
    #===============================================================

    #STUDENT LEVEL==================================================
    #ensure we only have one file to work with for each type otherwise record duplication errors may occur
    asa <- unlist(fnames["asa"])[1]
    asg <- unlist(fnames["asg"])[1]
    ash <- unlist(fnames["ash"])[1]
    asr <- unlist(fnames["asr"])[1]

    asaN <- unlist(fnamesNumeracy["asa"])[1]
    asgN <- unlist(fnamesNumeracy["asg"])[1]
    ashN <- unlist(fnamesNumeracy["ash"])[1]
    asrN <- unlist(fnamesNumeracy["asr"])[1]

    #ASA MERGE================================
    stuDF1 <- read_sav(asa, user_na = TRUE)
    stuDF1 <- UnclassCols(stuDF1)
    colnames(stuDF1) <- toupper(colnames(stuDF1))

    stuDF1Num <- read_sav(asaN, user_na = TRUE)
    stuDF1Num <- UnclassCols(stuDF1Num)
    colnames(stuDF1Num) <- toupper(colnames(stuDF1Num))

    stuDFx <- rBindTibble(stuDF1, stuDF1Num)

    #in very rare situation there may be duplicate records between 4th and numeracy
    if(anyDuplicated(stuDFx)>0){
      stuDFx <- dropTibbleDupes(stuDFx)
    }

    stuDF1 <- stuDFx
    stuDF1Num <- NULL
    stuDFx <- NULL
    #=========================================

    #ASG MERGE================================
    stuDF2 <- read_sav(asg, user_na = TRUE)
    stuDF2 <- UnclassCols(stuDF2)
    colnames(stuDF2) <- toupper(colnames(stuDF2))

    stuDF2Num <- read_sav(asgN, user_na = TRUE)
    stuDF2Num <- UnclassCols(stuDF2Num)
    colnames(stuDF2Num) <- toupper(colnames(stuDF2Num))

    stuDFx <- rBindTibble(stuDF2, stuDF2Num)

    #in very rare situation there may be duplicate records between 4th and numeracy
    if(anyDuplicated(stuDFx)>0){
      stuDFx <- dropTibbleDupes(stuDFx)
    }

    stuDF2 <- stuDFx
    stuDF2Num <- NULL
    stuDFx <- NULL
    #=========================================

    #Merge ASA and ASG combined files together
    ids1 <- grep("^ID", names(stuDF1), ignore.case=TRUE, value=TRUE)
    ids2 <- grep("^ID", names(stuDF2), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]

    mm <- mergeTibble(stuDF1,
                      stuDF2,
                      by=ids12,
                      all.x=FALSE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]

    if(nrow(stuDF1) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }
    if(nrow(stuDF2) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }
    #=========================================


    if(min(is.na(ash)) == 0 || min(is.na(ashN))==0) {#test if we have the ash file
      if(min(is.na(ash))==0){#test 4th grade file
        stuDF3 <- read_sav(ash, user_na = TRUE)
        stuDF3 <- UnclassCols(stuDF3)
        colnames(stuDF3) <- toupper(colnames(stuDF3))
      }
      if(min(is.na(ashN))==0){#test 4th grade file
        stuDF3N <- read_sav(ashN, user_na = TRUE)
        stuDF3N <- UnclassCols(stuDF3N)
        colnames(stuDF3N) <- toupper(colnames(stuDF3N))
      }

      stuDFx <- rBindTibble(stuDF3, stuDF3N)

      #in very rare situation there may be duplicate records between 4th and numeracy
      if(anyDuplicated(stuDFx)>0){
        stuDFx <- dropTibbleDupes(stuDFx)
      }

      stuDF3 <- stuDFx #cleanup
      stuDF3N <- NULL
      stuDFx <- NULL

      ids3 <- grep("^ID", names(stuDF3), ignore.case=TRUE, value=TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF3,
                        by=idsmm3,
                        all.x=TRUE,
                        all.y=FALSE,
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nrow(stuDF1) != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
    } else {
      idsmm3 <- ids12
    }

    if(min(is.na(asr)) == 0 ||min(is.na(asrN)) == 0 ){ #test if either set has an asr file
      if(min(is.na(asr)) == 0 ){#test 4th grade asr file
        stuDF4 <- read_sav(asr, user_na = TRUE)
        stuDF4 <- UnclassCols(stuDF4)
        colnames(stuDF4) <- toupper(colnames(stuDF4))
      }
      if(min(is.na(asrN)) == 0 ){#test 4th grade asr file
        stuDF4N <- read_sav(asrN, user_na = TRUE)
        stuDF4N <- UnclassCols(stuDF4N)
        colnames(stuDF4N) <- toupper(colnames(stuDF4N))
      }

      stuDFx <- rBindTibble(stuDF4, stuDF4N)

      if(anyDuplicated(stuDFx)>0){
        stuDFx <- dropTibbleDupes(stuDFx)
      }

      stuDF4 <- stuDFx #cleanup
      stuDF4N <- NULL
      stuDFx <- NULL

      ids4 <- grep("^ID", names(stuDF4), ignore.case=TRUE, value=TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF4,
                        by=idsmm4,
                        all.x=TRUE,
                        all.y=FALSE,
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance."))
      }
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    }

    stuFP <- file.path(dataFolderPath,paste0("asg", countryCode, yearCode, ".txt"))
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)
    #===============================================================

    #Student-Teacher Linkage and Teacher Background=================
    ast <- unlist(fnames["ast"])[1]
    atg <- unlist(fnames["atg"])[1]
    astN <- unlist(fnamesNumeracy["ast"])[1]
    atgN <- unlist(fnamesNumeracy["atg"])[1]

    stuTeachDF <- read_sav(ast, user_na = TRUE)
    stuTeachDF <- UnclassCols(stuTeachDF)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))

    stuTeachDFN <- read_sav(astN, user_na = TRUE)
    stuTeachDFN <- UnclassCols(stuTeachDFN)
    colnames(stuTeachDFN) <- toupper(colnames(stuTeachDFN))

    stuTeachDFx <- rBindTibble(stuTeachDF, stuTeachDFN)

    #in very rare situation there may be duplicate records between 4th and numeracy
    if(anyDuplicated(stuTeachDFx)>0){
      stuTeachDFx <- dropTibbleDupes(stuTeachDFx)
    }

    stuTeachDF <- stuTeachDFx #cleanup
    stuTeachDFN <- NULL
    stuTeachDFx <- NULL

    teachDF <- read_sav(atg, user_na = TRUE)
    teachDF <- UnclassCols(teachDF)
    colnames(teachDF) <- toupper(colnames(teachDF))

    #DO NOT MERGE THE TEACHER BACKROUND DATA::The 4th grade file includes all teacher data and the Numeracy file is just a small subset of those teachers

    ids1 <- grep("^ID", names(stuTeachDF), ignore.case=TRUE, value=TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]

    mm <- mergeTibble(stuTeachDF,
                      teachDF,
                      by=ids12,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[ , names(mm)[!grepl("\\.junk$", names(mm))]]

    if(nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("atg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance."))
    }

    teachFP <- file.path(dataFolderPath, paste0("atg", countryCode, yearCode, ".txt"))
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    #===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)

    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    #===============================================================

    #calculate the dim0 to store in the .meta file for fast retreival
    nrow0 <- nrow(mm)
    ncol0 <- length(unique(c(ffsch$variableName, ffstu$variableName, ffTeach$variableName)))
    dim0 <- c(nrow0, ncol0)
    
    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=5,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dim0=dim0)

    saveRDS(cacheFile, file.path(dataFolderPath,paste0("a", countryCode, yearCode,".meta")))

    #===============================================================
  } else {
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),".\n"))
    }
  }#end if(runProcessing==TRUE)

  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dim0=dim0))
}

#get the full country name to aide the user, so they won't have to track them down.
#cntryCode should be the 3 character country code vector defined in the data filename scheme (e.g., usa = United States, swe = Sweden)
#if a match is not found, this funtion will return a character value indicating it is unknown '(unknown) CountryCode: xxx'
getTIMSSCountryName <- function(countryCode){

  cntryCodeDF <- data.frame(
      cntryCode = c("aad", "aba", "adu", "alb", "are", "arm", "aus", "aut", "aze",
                    "bfl", "bfr", "bgr", "bhr", "bih", "bsq", "bwa",
                    "cab", "can", "cbc", "che", "chl", "col", "cot", "cqu", "cyp", "cze",
                    "deu", "dnk", "dza",
                    "egy", "ema", "eng", "esp", "est",
                    "fin", "fra",
                    "geo", "gha", "grc",
                    "hkg", "hnd", "hrv", "hun",
                    "idn", "irl", "irn", "isl", "isr", "ita",
                    "jor", "jpn",
                    "kaz", "kor", "kwt",
                    "lbn", "ltu", "lva",
                    "mar", "mda", "mkd", "mlt", "mne", "mng", "mys",
                    "nir", "nld", "no4", "no8", "nor", "nzl",
                    "omn",
                    "pak", "phl", "pol", "prt", "pse",
                    "qat",
                    "rmo", "rom", "rus",
                    "sau", "scg", "sco", "sgp", "slv", "srb", "svk", "svn", "swe", "syr",
                    "tha", "tun", "tur", "twn",
                    "uin", "ukr", "uma", "umn", "usa",
                    "xkx",
                    "ye6", "yem",
                    "zaf", "zgt", "zwc"),
      cntryName = c("Abu Dhabi, UAE", "Buenos Aires, Argentina", "Dubai, UAE", "Albania", "United Arab Emirates", "Armenia", "Australia", "Austria", "Azerbaijan",
                    "Belgium (Flemish)", "Belgium (French)", "Bulgaria", "Bahrain", "Bosnia and Herzegovina", "Basque Country, Spain", "Botswana",
                    "Alberta, Canada", "Canada", "British Columbia, Canada", "Switzerland", "Chile", "Colombia", "Ontario, Canada", "Quebec, Canada", "Cyprus", "Czech Republic",
                    "Germany", "Denmark", "Algeria",
                    "Egypt", "Madrid, Spain", "England", "Spain", "Estonia",
                    "Finland", "France",
                    "Georgia", "Ghana", "Greece",
                    "Hong Kong SAR", "Honduras", "Croatia", "Hungary",
                    "Indonesia", "Ireland", "Iran, Islamic Rep. of", "Iceland", "Israel", "Italy",
                    "Jordan", "Japan",
                    "Kazakhstan", "Korea, Rep. of", "Kuwait",
                    "Lebanon", "Lithuania", "Latvia",
                    "Morocco", "Moldova, Republic of", "Macedonia, Rep. of", "Malta", "Montenegro", "Mongolia", "Malaysia",
                    "Northern Ireland", "Netherlands", "Norway (4th grade)", "Norway (8th grade)", "Norway", "New Zealand",
                    "Oman",
                    "Pakistan", "Philippines", "Poland", "Portugal", "Palestinian Nat'l Auth.",
                    "Qatar",
                    "Moscow, Russian Federation", "Romania", "Russian Federation",
                    "Saudi Arabia", "Serbia", "Scotland", "Singapore", "El Salvador", "Serbia", "Slovak Republic", "Slovenia", "Sweden", "Syrian Arab Republic",
                    "Thailand", "Tunisia", "Turkey", "Chinese Taipei",
                    "Indiana, US", "Ukraine", "Massachusetts, US", "Minnesota, US", "United States",
                    "Kosovo",
                    "Yemen (6th)", "Yemen",
                    "South Africa", "South Africa (Gauteng)", "South Africa (Western Cape Province)"),
                    stringsAsFactors = FALSE) #be sure to not create any factors::factors not needed at all

  lookupNames <- vector(mode = "character", length = length(countryCode))

  for(i in 1:length(countryCode)){
    testName <- cntryCodeDF[cntryCodeDF$cntryCode==countryCode[i], "cntryName"]

    if(length(testName)==0){ #test if no value found
      testName <- paste("(unknown) CountryCode:", countryCode[i])
    }

    lookupNames[i] <- testName
  }

  return(lookupNames)
}

# recreates a similar function to rbind.fill of plyr package, where it creates a
# merged tibble from the two tibbles must ensure all column attributes are retained
rBindTibble <- function(tbl1, tbl2){

  tblA <- tbl1 #create copys of the tibbles
  tblB <- tbl2

  #create empty row tibble with full columns
  xTbl <- cbind(tblA[FALSE, ], tblB[FALSE, !(names(tblB) %in% names(tblA))])

  #bind any new rows to both sets as we need equal columns.  values are filled with NA values for these columns
  tblA <- cbind(tblA, xTbl[1:nrow(tblA), !(names(xTbl) %in% names(tblA))])
  tblB <- cbind(tblB, xTbl[1:nrow(tblB), !(names(xTbl) %in% names(tblB))])

  #append our rows together to the combined tibble
  xTbl <- rbind(xTbl, tblA)
  xTbl <- rbind(xTbl, tblB)

  cols <- names(xTbl) #reapply the attributes from the source tibbles
  for(i in 1:length(cols)) {
    coli <- cols[i]
    abcoli <- xTbl[[coli]]
    if(coli %in% names(tbl1) || coli %in% names(tbl2)) {
      if(coli %in% names(tbl1)) {
        ocoli <- tbl1[[coli]]
      } else {
        ocoli <- tbl2[[coli]]
      }
      newAtrs <- attributes(ocoli)
      oldAnames <- names(attributes(abcoli))
      transname <- names(newAtrs)
      transname <- transname[!transname %in% oldAnames]
      for(tri in 1:length(transname)) {
        if((!is.null(transname[tri])) && (!is.na(transname[tri])) && (length(transname[tri])>0)){
          attr(abcoli, transname[tri]) <- newAtrs[[transname[tri]]]
        }
      }
      xTbl[[coli]] <- abcoli
    }
  }
  return(xTbl)
}

#builds the TIMSS dataList object
buildTIMSS_dataList <- function(stuLaf, stuFF, schLaf, schFF, tchLaf, tchFF){

  dataList <- list()

  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(lafObject = stuLaf,
                                        fileFormat = stuFF,
                                        levelLabel = "Student",
                                        forceMerge = TRUE,
                                        parentMergeLevels = NULL,
                                        parentMergeVars = NULL,
                                        mergeVars = NULL,
                                        ignoreVars = NULL,
                                        isDimLevel = FALSE)

  dataList[["School"]] <- dataListItem(lafObject = schLaf,
                                       fileFormat = schFF,
                                       levelLabel = "School",
                                       forceMerge = FALSE,
                                       parentMergeLevels = c("Student", "Student"),
                                       parentMergeVars = c("idcntry", "idschool"),
                                       mergeVars = c("idcntry", "idschool"),
                                       ignoreVars = names(schLaf)[names(schLaf) %in% names(stuLaf)], #student file variables will take precedence over school variables of same name
                                       isDimLevel = FALSE)

  dataList[["Teacher"]] <- dataListItem(lafObject = tchLaf,
                                        fileFormat = tchFF,
                                        levelLabel = "Teacher",
                                        forceMerge = FALSE,
                                        parentMergeLevels = c("Student", "Student"),
                                        parentMergeVars = c("idcntry", "idstud"),
                                        mergeVars = c("idcntry", "idstud"),
                                        ignoreVars = names(tchLaf)[names(tchLaf) %in% names(stuLaf)], #student file variables will take precedence over teacher variables of same name
                                        isDimLevel = TRUE)

  return(dataList)
}
