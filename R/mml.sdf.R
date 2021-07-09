#' @title EdSurvey Direct Estimation 
#' @description Prepare IRT parameters and score items and then estimate a linear model with direct estimation.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   model.
#' @param data       an \code{edsurvey.data.frame} for the National Assessment of Educational Progress (NAEP) 
#'                  and the Trends in International Mathematics and Science Study (TIMSS).
#' @param weightVar  a character indicating the weight variable to use.
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, it  uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      the levels of all factor variables that are specified
#'                      in an \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param composite  logical; for a NAEP composite, setting to \code{FALSE} fits the model to all items at once,
#'                   in a single construct, whereas setting to \code{TRUE} fits the model as a NAEP composite
#'                   (i.e., a weighted average of the subscales). This argument is not applicable for TIMSS. 
#' @param dctPath a connection that points to the location of a NAEP dct file. A dct file can be used to input custom item response theory (IRT)
#'                parameters and subscale/subtest weights for NAEP assessments compared with those provided in the \code{NAEPirtparams} package. Otherwise,
#'                the argument defaults to NULL and IRT parameters and subscale weights from \code{NAEPirtparams} are used.
#'                IRT parameters for TIMSS cannot be supplied through a \code{dctPath} and are downloaded by using the \code{\link{downloadTIMSS}} function. 
#' @param verbose logical; indicates whether a detailed printout should display during execution, only for NAEP data.
#' @param multiCore allows the \code{foreach} package to be used. You should
#'                  have already set up
#' \ifelse{latex}{the \code{registerDoParallel} function in the \code{doParallel} package}{\code{\link[doParallel]{registerDoParallel}}}.
#' @param numberOfCores the number of cores to be used when using \code{multiCore}. Defaults to 75\% of available cores. Users 
#'                      can check available cores with \code{detectCores()}. 
#' @param minNode numeric; minimum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param maxNode numeric; maximum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param Q integer; number of integration points per student used when integrating over the levels of the latent outcome construct. 
#' @param scoreDict a \code{data.frame} that includes guidelines for scoring the provided NAEP data. 
#'                  Here, \emph{scoring} refers to turning item responses into scores on each item.
#'                  To see the default scoring guidelines, call the function \code{defaultNAEPScoreCard()}, or see the Examples section.
#'                  See Details for more information on possible scores.
#' @param idVar a variable that is used to explicitly define the name of the student identifier 
#'              variable to be used from \code{data}. Defaults to \code{NULL}, and \code{sid} is used 
#'              as the student identifier. 
#'
#' @details 
#' Typically, models are fit with NAEP data using plausible values to integrate out the uncertainty in the measurement of individual
#' student outcomes. When direct estimation is used, the measurement error is integrated out explicitly using \code{Q} quadrature points.
#' See documentation for \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}} in the \code{Dire} package.
#'
#' The \code{scoreDict} helps turn response categories that are not simple item responses, such as \code{Not Reached} and \code{Multiple},
#' to something coded as inputs for the \code{mml} function in \code{Dire}. How \code{mml} treats these values depends on the test.
#' For NAEP, for a dichotomous item, 8 is scored as the same proportion correct as the guessing parameter for that item, 0 is 
#' an incorrect response, an NA does not change the student's score, and 1 is correct. TIMSS does not require a \code{scoreDict}.
#'
#' @return 
#' An \code{edSurveyMML} object, which is the outcome from \code{mml.sdf}, with the following elements:
#'    \item{mml}{an object containing information from the \code{mml} procedure. 
#'    \code{?mml} can be used for further information.}
#'    \item{scoreDict}{the scoring used in the \code{mml} procedure}.
#'    \item{itemMapping}{the item mapping used in the \code{mml} procedure}. 
#' @references
#' Cohen, J., & Jiang, T. (1999). Comparison of partially measured latent traits across nominal subgroups.
#'    \emph{Journal of the American Statistical Association}, \emph{94}(448), 1035--1044. https://doi.org/10.2307/2669917
#'
#' @import NAEPirtparams
#' @importFrom Dire mml    
#' @importFrom stats na.omit
#' @importFrom utils flush.console
#' @example /man/examples/mml.sdf.R
#' @export
mml.sdf <- function(formula,
                    data,
                    weightVar = NULL,
                    omittedLevels = TRUE,
                    composite = TRUE,
                    dctPath = NULL,
                    verbose = FALSE,
                    multiCore = FALSE,
                    numberOfCores = NULL, 
                    minNode = -4, 
                    maxNode = 4, 
                    Q = 34,
                    scoreDict = defaultNAEPScoreCard(),
                    idVar = NULL) {
  # check for parrallel if multiCore True 
  if(any(grepl("_linking", all.vars(formula), fixed=TRUE))) {
    stop("mml.sdf does not support linking error.")
  }
  if(multiCore == TRUE){
    # check doParallel 
    if(requireNamespace("doParallel")){
      # set numberOfCores default if not provided 
      if(is.null(numberOfCores)){
        numberOfCores <- parallel::detectCores() * .75
      }
    } else {
      multiCore <- FALSE
      message(paste0("Unable to find package doParallel, setting multiCore to FALSE. Require the ", dQuote("doParallel"), " package to use multiCore option."))
    }
    # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18119
    # R on OS X bug that prevents parallel
    if (grepl("Darwin", Sys.info()[1]) &&
        getRversion() <= "4.1.0") {
      multiCore <- FALSE
      message("Upgrade to R 4.1.1 to use multiCore on Mac OS.")
    }
  }
  sdf <- data
  # check for no response
  if(is.null(getResponse(formula))) {
    stop("Please specify response variable. Equation should be of the form: y ~ x")
  }
  ### check for transformations in equations: sqrt(mmat), exp(mmat), mmat^0.5, mmat^(1/3), log(mmat), 
  if(all(grepl("\\^\\d+|log\\(.*\\)|\\^\\(.*\\)|exp\\(.*\\)|sqrt\\(.*\\)", formula[2:3]))){
    stop("Please transform variable outside of equations.")
  } 

  # survey checks 
  if (getAttributes(sdf, "survey") == 'TIMSS') {
    checkDataClass(data, c("edsurvey.data.frame"))
    ### TIMSS data ###
    ### building paramTabs
    # filter IRT params to year, subject, grade level
    theYear <- as.numeric(getAttributes(sdf, "year"))
    theSubject <- all.vars(formula[[2]])
    if(length(theSubject) > 1) {
      stop("the response must be a single test score.")
    }
    # test this is length 1
    theSubject <- theSubject[theSubject %in% c(names(getAttributes(sdf, "pvvars")))]
    if(length(theSubject) < 1) {
      stop(paste0("Cannot find TIMSS subject ", dQuote(all.vars(formula[[2]]))))
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
    
    if (theYear %in% c(2011, 2015, 2019)) {
      # get the parameters
      timssDir <- dirname(sdf$dataList$School$lafObject@filename)
      allParams <- timssParam(timssDir, theYear, theLevel, subjectFilter) 
    } else {
      stop("Item parameters are currently only available for 2011 and 2015.")
    }
    
    # get params and transformations
    params <- allParams$params
    transformations <- allParams$transformations
    
    # check that parameter items exist in data
    itemsUse <- params$TIMSSid[params$TIMSSid %in% colnames(sdf)]
    itemsNotInData <- setdiff(params$TIMSSid, itemsUse)
    if (length(itemsNotInData) > 0) {
      warning(paste0('These items were in the assessment, but not in your data: ', pasteItems(itemsNotInData, final = 'and '))) ###refine later###
      paramsUse <- params[params$TIMSSid %in% itemsUse, ]
    } else {
      paramsUse <- params
    }
    
    # get paramTabs
    paramTabs <- timssParamTabs(paramsUse)
    polyParamTab <- paramTabs$polyParamTab
    dichotParamTab <- paramTabs$dichotParamTab
    
    polyParamTab$test <- theSubject
    polyParamTab$scorePoints <- 2 #max points for CR is 2 for TIMSS
    dichotParamTab$test <- theSubject
    
    ### building testDat
    # filter transformation constants
    if (nrow(transformations) == 0) {
      stop ("Transformation scales do not exist for your assessment.") ###refine later###
    }
    
    #for testDat, average the location and scal
    testDat <- as.data.frame(
      list(
        test = theSubject,
        location = mean(transformations$location),
        scale = mean(transformations$scale)
      )
    )
    
    ### create stuItems 
    # get dependent vars, weight, and items from data
    indepVars <- labels(terms(formula))
    # TIMSS items are set to missing, must use omittedLevels=FALSE
    edf <- getData(data=sdf, varnames=c(polyParamTab$ItemID, dichotParamTab$ItemID, indepVars, weightVar, getStratumVar(sdf), getPSUVar(sdf), idVar), omittedLevels = FALSE) 
    
    # check completeness
    incomplete <- !complete.cases(edf[,c(indepVars, weightVar)])
    if(any(incomplete)) {
      warning("Removing ", sum(incomplete), " rows with NAs from analysis.")
      edf <- edf[!incomplete,]
    }
    # check that there is some data to work with
    if(nrow(edf) <= 0) {
      stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                  sQuote("getData"), "."))
    }

    # scoring 
    edf <- scoreTIMSS(edf, polyParamTab, dichotParamTab)
    
    # creat stuItems
    if(is.null(idVar)) {
      idVar <- "sid"
      edf$sid <- 1:nrow(edf)
    }

    stuItems <- melt(edf[,c(polyParamTab$ItemID,dichotParamTab$ItemID, idVar)], id.vars=idVar, measure.vars=c(polyParamTab$ItemID,dichotParamTab$ItemID))
    colnames(stuItems) <- c(idVar, 'key', 'score')
    
    ### create stuDat ###
    stuDat <- edf[edf[,idVar] %in% unique(stuItems[,idVar]), c(idVar, indepVars, weightVar, getStratumVar(sdf), getPSUVar(sdf))]
    
    ### warnings
    if (!missing(dctPath)) {
      warning('dctPath is not used for TIMSS; ignoring argument.')
    }
    if(!missing(composite) && composite) {
      warning('Composite is not supported in TIMSS.')
    }
    composite <- FALSE 
    itemMapping <- c(`For NAEP only` = NULL) 
    scoreDict <- c(`For NAEP only` = NULL) 
    
  } else if (getAttributes(sdf, "survey") == 'NAEP') {
    checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
    ### NAEP data ###
    ### Option A: using NAEPirtparams instead of dct
    if (is.null(dctPath)) {
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
        warning('Accommodation option not available yet, using IRT parameters for tests with no accommodations by default.') ###refine later###
        paramsFilter <- paramsFilter[!paramsFilter$accommodations=='accom',]
      } else {
        paramsFilter <- paramsFilter[!paramsFilter$accommodations=='accom',]
      }
      
      if ('State' == theRegion){
        paramsFilter <- paramsFilter[paramsFilter$assessmentCode=='State',]
      } else {
        # This is National or not indicated
        warning('Using IRT parameters for National tests by default.')
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
      
      # get paramTabs
      paramTabs <- naepParamTabs(paramsUse)
      polyParamTab <- paramTabs$polyParamTab
      dichotParamTab <- paramTabs$dichotParamTab
      
      ### building testDat
      # filter transformation constants
      transf <- NAEPirtparams::transformations
      transFilter <- transf[transf$level==theLevel & transf$year==theYear & transf$subject==theSubject, ]
      
      if ('accom' %in% transFilter$accommodations){
        # Everything no-accom for now
        transFilter <- transFilter[!transFilter$accommodations=='accom',]
      } else {
        transFilter <- transFilter[!transFilter$accommodations=='accom',]
      }
      
      if ('State' == theRegion){
        transFilter <- transFilter[transFilter$assessmentCode=='State',]
      } else {
        # This is National or not indicated
        transFilter <- transFilter[!transFilter$assessmentCode=='State',]
      }
      
      if (nrow(transFilter) != length(unique(transFilter$subtest))) {
        stop ("You have duplicate transformation constants in your table.") ###refine later###
      }
      
      # filter to testDat
      testDat <- transFilter[,c('subtest','location','scale', 'subtestWeight')]
      testDat <- testDat[!is.na(testDat$subtestWeight),]
      
    } else {
      ### Option B: using dct file provided
      allTables <- parseNAEPdct(dctPath) 
      dichotParamTab <- allTables$dichotParamTab
      polyParamTab <- allTables$polyParamTab
      testDat <- allTables$testDat
      adjustedData <- data.frame()
      fr2Path <- getAttributes(sdf, "fr2Path")
    }
    
    ### continue building paramTabs
    # get score card (giving item id, answer, points for each answer)
    sCard <- getNAEPScoreCard(fr2Path, polyParamTab$ItemID, dichotParamTab$ItemID, adjustedData, scoreDict)
    items <- unique(sCard$key)
    cols <- colnames(sCard)
    colnames(sCard) <- c('key', 'From', 'To')
    itemMapping <- c() 
    if (verbose) {
      for (itm in items) {
        mapping <- list(with(sCard[sCard$key==itm,], table(From, To, useNA = 'ifany')))
        names(mapping) <- itm
        itemMapping <- c(itemMapping, mapping)
        cat(paste0('item: ', itm, '\n'))
        print(with(sCard[sCard$key==itm,], table(From, To, useNA = 'ifany')))
        cat('\n')
        cat('\n')
        flush.console()
      }
    } else {
      for (itm in items) {
        mapping <- list(with(sCard[sCard$key==itm,], table(From, To, useNA = 'ifany')))
        names(mapping) <- itm
        itemMapping <- c(itemMapping, mapping)
      }
    }
    colnames(sCard) <- cols
    
    # get max vals per item and merge to paramTabs
    otherLevs <- c("Multiple", "Not Reached", "Missing", "Omitted", "Illegible", "Non-Rateable", "Off Task")
    keyMaxVals <- aggregate(score ~ key, sCard[!tolower(sCard$answer) %in% tolower(otherLevs), ], max) #max value for each item, non-answer levels
    colnames(keyMaxVals) <- c('ItemID', 'score')
    polyParamTab <- merge(polyParamTab, keyMaxVals, by='ItemID', all.x=T)
    colnames(polyParamTab)[colnames(polyParamTab)=='score'] <- 'scorePoints'
    dichotParamTab <- merge(dichotParamTab, keyMaxVals, by='ItemID', all.x=T)
    colnames(dichotParamTab)[colnames(dichotParamTab)=='score'] <- 'scorePoints'
    
    # check that there are enough d's for polyParamTab max scorePoints
    polyParamTab$numD <- apply(polyParamTab[,c('d1','d2','d3','d4','d5')], 1, function(x) 5 - sum(is.na(x)))
    polyParamTab[is.na(polyParamTab$scorePoints),'scorePoints'] <- 0
    if (any(polyParamTab$scorePoints != polyParamTab$numD)) {
      badIDs <- polyParamTab[polyParamTab$scorePoints!=polyParamTab$numD,'ItemID']
      polyParamTab <- polyParamTab[polyParamTab$scorePoints==polyParamTab$numD,]
      warning(paste0(paste0(badIDs, collapse = ', '), ' did not have enough parameters for the max score and will be taken out of analysis.')) ###refine later
    }
    polyParamTab$numD <- NULL
    
    ### create stuItems ###
    if (!is.null(dctPath)) {
      # if a dct file was provided, check that all the items in the dct are also in the data
      allParams <- c(polyParamTab$ItemID, dichotParamTab$ItemID)
      inData <- allParams[allParams %in% colnames(sdf)]
      notinData <- allParams[!allParams %in% colnames(sdf)]
      if (length(notinData)>0) {
        # filter items to only those in data
        polyParamTab <- polyParamTab[polyParamTab$ItemID%in%inData,]
        dichotParamTab <- dichotParamTab[dichotParamTab$ItemID%in%inData,]
        warning(paste0('The following items in your dct file were not in the data and will be excluded from analysis: ', paste(notinData, collapse = ',')))
      }
    }
    
    # get dependent vars, weight, and items from data
    indepVars <- labels(terms(formula))
    edf <- getData(data=sdf, varnames=c(polyParamTab$ItemID, dichotParamTab$ItemID, indepVars, weightVar, getStratumVar(sdf), getPSUVar(sdf), idVar), omittedLevels = FALSE) 
    
    # check for incomplete cases on the formula variables and weights
    incomplete <- !complete.cases(edf[,c(indepVars, weightVar)])
    if(any(incomplete)) {
      warning("Removing ", sum(incomplete), " rows with NAs from analysis.")
      edf <- edf[!incomplete,]
    }
    # check that there is some data to work with
    if(nrow(edf) <= 0) {
      stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                  sQuote("getData"), "."))
    }
    
    # add id number
    if(is.null(idVar)) {
      idVar <- "sid"
      edf$sid <- 1:nrow(edf)
    }

    # reshape and score items
    stuItemsWide <- edf[ , c(idVar, c(polyParamTab$ItemID, dichotParamTab$ItemID))]
    stuItemsWide <- data.table(stuItemsWide)
    stuItemsLong <- melt(stuItemsWide, id.vars=idVar, measure.vars=c(polyParamTab$ItemID, dichotParamTab$ItemID))
    colnames(stuItemsLong) <- c('id', 'key', 'answer')
    stuItemsLong <- stuItemsLong[!is.na(answer)]
    stuItems <- merge(stuItemsLong, sCard, by=c('key', 'answer'), all.x=TRUE)  # score through merge
    stuItems <- as.data.frame(stuItems[ , c("id", "key", "score")])
    colnames(stuItems)[1] <- idVar
    
    ### create stuDat ###
    stuDat <- edf[edf[,idVar] %in% unique(stuItems[,idVar]), c(idVar, indepVars, weightVar,stratavar=getStratumVar(sdf), psuvar=getPSUVar(sdf))]
    
    ### continue building testDat and paramTabs
    if((round(sum(testDat$subtestWeight), 1)==1) & (nrow(testDat)>1)){
      # if weights add up to 1, this is a composite test
      testDat$test <- 'composite'
      if (nrow(dichotParamTab) > 0) {
        dichotParamTab$test <- 'composite'
      }
      if (nrow(polyParamTab) > 0) {
        polyParamTab$test <- 'composite'
      }
    } else {
      # this is not a composite test
      testDat$test <- testDat$subtest
      dichotParamTab$test <- testDat$subtest
      polyParamTab$test <- testDat$subtest
      testDat$subtest <- NULL
      dichotParamTab$subtest <- NULL
      polyParamTab$subtest <- NULL
    }
    
    if (nrow(polyParamTab) == 0) {
      polyParamTab = NULL
    }
    if (nrow(dichotParamTab) == 0) {
      dichotParamTab = NULL
    }
  } else {
    # Other assessments that are not TIMSS or NAEP
    stop("This assessment is currently unsupported.")
  }
  
  # setting up cluster for multi-core 
  startedCluster <- FALSE
  if(multiCore){
    # check if cluster is already running 
    if(nrow(showConnections()) == 0) {
      cores <- round(numberOfCores, 0) # use 75 percent of cores 
      cl <- parallel::makeCluster(cores)
      startedCluster <- TRUE
      doParallel::registerDoParallel(cl, cores=cores)
    }
  }
  cat("Pre-processing Completed.\nStarting MML Procedure.")
  mmlObj <- mml(formula = formula,
                stuItems = stuItems,
                stuDat = stuDat,
                idVar = idVar,
                dichotParamTab = dichotParamTab,
                polyParamTab = polyParamTab,
                testScale = testDat,
                Q = Q,
                composite = composite,
                minNode = minNode,
                maxNode = maxNode,
                strataVar = getStratumVar(sdf),
                PSUVar = getPSUVar(sdf),
                weightVar = weightVar,
                fast = TRUE,
                multiCore = multiCore)
  # stop cluster, if we started it
  if(multiCore & startedCluster){
    parallel::stopCluster(cl)
  }
  # get call
  call <- match.call()
  # return 
  return(structure(list("Call" = call, "mml" = mmlObj, "scoreDict" = scoreDict, "itemMapping" = itemMapping),
                   class="edSurveyMML"))
}

#' @importFrom stats terms.formula
getResponse <- function(form) {
  tf <- terms.formula(form)
  respIndex <- attr(tf, "response")
  if(respIndex == 0) {
    return(NULL)
  }
  return(attr(tf,"variables")[[1+attr(tf, "response")]])
}


