#' Format AM dct File for Use with DirectEstimation
#' @description
#' Takes an \code{AM dct} file and formats it for use with the \code{mml} method
#' as \code{paramTab}.
#' 
#' @param dct a file location from which to read the \code{dct} file
#' @param mml a logical for if the paramTab is being used in \code{mml.sdf}
#' 
#' @return a \code{data.frame} in a format suitable for use with \code{mml} as
#' a \code{paramTab}.
#' 
#' @author Sun-Joo Lee, Eric Buehler, and Paul Bailey
#' @export
getAllItems <- function(x, npv, construct, omittedLevels=FALSE,...) {
  cl <- match.call()
  survey <- getAttributes(x, "survey")
  if(survey %in% "NAEP") {
    cl[[1]] <- quote(getAllItems_NAEP)
    return(eval(cl))
  }
  if(survey %in% "TIMSS") {
    cl[[1]] <- quote(getAllItems_TIMSS)
    return(eval(cl))
  }
  stop(paste0(dQuote("getAllItems")," does not support ", survey))
}

# a function for returning all items associated with a NAEP or TIMSS construct.
getAllItems_TIMSS <- function(sdf, construct, omittedLevels){
  survey <- getAttributes(sdf, "survey")
  ### TIMSS data ###
  ### building paramTabs
  # filter IRT params to year, subject, grade level
  theYear <- as.numeric(getAttributes(sdf, "year"))
  theSubject <- construct
  if(length(theSubject) > 1) {
    stop("the response must be a single test score.")
  }
  # test this is length 1
  theSubject <- theSubject[theSubject %in% c(names(getAttributes(sdf, "pvvars")))]
  if(length(theSubject) < 1) {
    stop(paste0("Cannot find TIMSS subject ", dQuote(construct)))
  }
  TIMSSsubject <- ifelse("m" %in% tolower(substr(theSubject, 1, 1)), "mmat")
  if(TIMSSsubject %in% "mmat") {
    subjectFilter <- 'MAT'
    # no subscale if this is mmat
    if(theSubject %in% "mmat") {
      TIMSSsubscale <- NA
    } else {
      TIMSSsubscale <- theSubject
    }
  }
  if(TIMSSsubject %in% "ssci") {
    subjectFilter <- 'SCI'
    # no subscale if this is ssci
    if(theSubject %in% "ssci") {
      TIMSSsubscale <- NA
    } else {
      TIMSSsubscale <- theSubject
    }
  }
  theLevel <- getAttributes(sdf, "gradeLevel")
  theLevel <- as.numeric(unlist(regmatches(theLevel, gregexpr("[[:digit:]]+", theLevel))))
  
  # get the parameters
  timssDir <- dirname(sdf$dataList$School$lafObject@filename)
  timssItems <- getTimssItems(timssDir, theYear, theLevel, subjectFilter) 
  
  return(timssItems)
}


getTimssItems <- function(timssDir, theYear, theLevel, subjectFilter) {
  
  # check for valid year 
  if(!theYear %in% c(2011, 2015, 2019)){
    stop(paste0("Grabbing question items is currently not available for ", theYear,"."))
  }

  # build download string and file name dynamically 
  # decade 
  theDecade <- gsub('\\d\\d(\\d\\d)',"\\1", theYear)
  
  # the grade 
  if(theYear == 2011){
    theGrade <- ""
  } else {
    theGrade <- paste0("_G",theLevel)
  }
  
  # the year 
  if(theYear %in% c(2015, 2019)){
    database <- "international-database"
    }
  
  # link
  download_link1 <- paste0("https://timssandpirls.bc.edu/timss",theYear, "/", database, "/downloads/T", 
                           theDecade, theGrade ,"_ItemInformation.zip")
  # file 
  file <- paste0("T", theDecade, theGrade ,"_ItemInformation.xlsx") 
  
  # check for file 
  dfFile <- paste0(timssDir, "/", file)
  if (!file.exists(dfFile)) {
    zips <- paste0(". Download them here: ", download_link1)
    stop(paste0("Make sure ", df1Name, ", ", df2Name, ", and ", df3Name, " are downloaded in ", timssDir, zips,". ", "Try running: downloadTIMSS('",timssDir, "', '",theYear,"')"))
  }
  
  # read items names 
  items <- suppressMessages(read_excel(dfFile, sheet = subjectFilter))
  return(tolower(items))
}

getAllItems_NAEP <- function(sdf, construct, omittedLevels){

  ### building paramTabs
  # filter IRT params to year, subject, grade level
  theYear <- getAttributes(sdf, "year")
  theSubject <-  getAttributes(sdf, "subject") 
  theLevel <- as.integer(regmatches(getAttributes(sdf, "gradeLevel"), regexpr('[[:digit:]]', getAttributes(sdf, "gradeLevel")))) 
  theRegion <- getAttributes(sdf, "assessmentCode")
  fr2Path <- getAttributes(sdf, "fr2Path")
  
  # use NAEPirtparams package and filter to needed params
  params <- NAEPirtparams::parameters
  paramsFilter <- params[params$level==theLevel & params$year==theYear & params$subject==theSubject, ]
  paramsFilter$NAEPid <- tolower(paramsFilter$NAEPid)
  
  if ('accom' %in% paramsFilter$accommodations){
    # Everything no-accom for now
    # warning('Accommodation option not available yet, using IRT parameters for tests with no accommodations by default.') ###refine later###
    paramsFilter <- paramsFilter[!paramsFilter$accommodations=='accom',]
  } else {
    paramsFilter <- paramsFilter[!paramsFilter$accommodations=='accom',]
  }
  
  if ('State' == theRegion){
    paramsFilter <- paramsFilter[paramsFilter$assessmentCode=='State',]
  } else {
    # This is National or not indicated
    # warning('Using IRT parameters for National tests by default.')
    paramsFilter <- paramsFilter[!paramsFilter$assessmentCode=='State',]
  }
  
  # check that there are no duplicate items
  if (!nrow(paramsFilter) == length(unique(paramsFilter$NAEPid))) {
    stop("You have duplicate items in your parameters table.") ###refine later###
  }
  
  # check that you have parameters
  paramItems <- paramsFilter$NAEPid
  if (length(paramItems) == 0){
    stop("Your assessment's IRT parameters do not exist. Please provide a DCT file") ###refine later###
  }
  
  # get deleted and/or adjusted items
  adjust <- NAEPirtparams::adjustments
  adjustData <- adjust[adjust$level==theLevel & adjust$subject==theSubject & adjust$year==theYear, ] 
  if ('accom' %in% adjustData$accommodations){
    # Everything no-accom for now
    warning('Accommodation option not available yet, using IRT parameters for tests with no accommodations by default.') ###refine later###
    adjustData <- adjustData[!adjustData$accommodations=='accom', ]
  } else {
    adjustData <- adjustData[!adjustData$accommodations=='accom', ]
  }
  deletedItems <- tolower(adjustData[adjustData$adjustment=='Deleted', 'NAEPid']) 
  deletedItems <- deletedItems[deletedItems %in% colnames(sdf)]
  adjustedData <- adjustData[adjustData$adjustment=='Collapsed', c('NAEPid','from','to')]
  adjustedData$NAEPid <- tolower(adjustedData$NAEPid)
  if (length(deletedItems) > 0) {
    warning(paste0(paste0(deletedItems, collapse = ', '), ' was/were deleted and will not be included in the analysis')) ###refine later
    paramsFilter <- paramsFilter[!paramsFilter$NAEPid %in% deletedItems, ]  
  }
  
  # check that items in parameters table exist in data
  itemsUse <- paramsFilter$NAEPid[paramsFilter$NAEPid %in% colnames(sdf)]
  itemsNotInData <- setdiff(paramsFilter$NAEPid, itemsUse)
  if (length(itemsNotInData) > 0) {
    warning(paste0('These items were in the assessment, but not in your data: ', pasteItems(itemsNotInData, final = 'and '))) ###refine later###
    paramsUse <- paramsFilter[paramsFilter$NAEPid %in% itemsUse, ]
  } else {
    paramsUse <- paramsFilter
  }
  return(itemsUse)
}
