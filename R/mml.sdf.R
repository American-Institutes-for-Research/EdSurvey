#' @title EdSurvey Direct Estimation
#' @description Prepare IRT parameters and score items and then estimate a linear model with direct estimation.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   model.
#' @param data       an \code{edsurvey.data.frame} for the National Assessment of Educational Progress (NAEP) 
#'                  and the Trends in International Mathematics and Science Study (TIMSS). 
#'                  The attributes \code{dichotParamTab}, \code{polyParamTab}, \code{testData},
#'                  \code{scoreCard} (for NAEP), and \code{scoreDict} (for TIMSS) must not be \code{NULL}.
#'                  Use the function \code{setNAEPScoreCard} or \code{setTIMSSScoreDict} to set attributes.
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
#' @param verbose logical; indicates whether a detailed printout should display during execution, only for NAEP data.
#' @param multiCore allows the \code{foreach} package to be used. You should
#'                  have already set up
#' \ifelse{latex}{the \code{registerDoParallel} function in the \code{doParallel} package}{\code{\link[doParallel]{registerDoParallel}}}.
#' @param numberOfCores the number of cores to be used when using \code{multiCore}. Defaults to 75\% of available cores. Users 
#'                      can check available cores with \code{detectCores()}. 
#' @param minNode numeric; minimum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param maxNode numeric; maximum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param Q integer; number of integration points per student used when integrating over the levels of the latent outcome construct. 
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
#' An \code{mml.sdf} object, which is the outcome from \code{mml.sdf}, with the following elements:
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
#' @aliases defaultNAEPScoreCard
#' @example /man/examples/mml.sdf.R
#' @export
mml.sdf <- function(formula,
                    data,
                    weightVar = NULL,
                    omittedLevels = TRUE,
                    composite = TRUE,
                    verbose = 0,
                    multiCore = FALSE,
                    numberOfCores = NULL, 
                    minNode = -4, 
                    maxNode = 4, 
                    Q = 34,
                    idVar = NULL) {
  stopifnot(inherits(verbose, c("numeric", "integer", "logical")))
  verbose <- as.numeric(verbose)
  # if the weight var is not set, use the default
  if(is.null(weightVar)) {
    weightVar <- attributes(getAttributes(data, "weights"))$default
  } else {
    weightVar <- weightVar
  } # End of if/else: is.null(weightVar)
  # check for parrallel if multiCore True 
  if(any(grepl("_linking", all.vars(formula), fixed=TRUE))) {
    stop("mml.sdf does not support linking error.")
  }
  # if 1) there is no idVar, 2) ROWID is on the data, 3) ROWID is unique, make it the idVar
  if(missing(idVar) && "ROWID" %in% colnames(data) && length(unique(data[["ROWID"]])) == length(data[["ROWID"]])) {
    idVar <- "ROWID"
  } else {
    if(verbose > 0) {
      message("no idVar set. This is necessary to draw plausible values.")
    }
  }
  if(multiCore == TRUE){
    if(verbose>0) {
      message("Starting parallel processing.")
    }

    # check doParallel 
    if(requireNamespace("doParallel")){
      # set numberOfCores default if not provided 
      if(is.null(numberOfCores)){
        numberOfCores <- parallel::detectCores() * .75
      }
      # check that they aren't using too many cores 
      if(numberOfCores > parallel::detectCores()){
        defaultCores <- parallel::detectCores() * .75
        warning(paste0(sQuote(numberOfCores), " is greater than number of avaliable cores,",
                       sQuote(parallel::detectCores())," setting number of cores to default of ",
                       sQuote(defaultCores)))
        numberOfCores <- defaultCores
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
      message("Upgrade to R 4.1.1 or higher to use multiCore on Mac OS.")
    }
  }
  if(verbose>0) {
    message("Gathering item information.")
  }

  # check for no response
  if(is.null(getResponse(formula))) {
    stop("Please specify response variable. Equation should be of the form: y ~ x")
  }
  ### check for transformations in equations: sqrt(mmat), exp(mmat), mmat^0.5, mmat^(1/3), log(mmat), 
  if(all(grepl("\\^\\d+|log\\(.*\\)|\\^\\(.*\\)|exp\\(.*\\)|sqrt\\(.*\\)", formula[2:3]))){
    stop("Please transform variable outside of equations.")
  } 

  ### check data class 
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  
  ### check for Strata and Psu Var 
  checkPsuStrata(data)
  strataVar <- getStratumVar(data)
  psuVar <- getPSUVar(data)
  
  ### check idVar
  idVar <- checkIdVar(data, idVar) 
  # if there was no ID, add one
  if(FALSE %in% idVar) {
    data$sid <- 1:nrow(data)
    idVar <- "sid"
  }
  # be sure tbl data is correctly converted to factor
  if(inherits(data, "light.edsurvey.data.frame")) {
    data <- lightUnclassCols(data)
  }
  # check survey 
  survey <- getAttributes(data, "survey")
  
  # survey checks 
  if (survey == 'TIMSS') {
    ### TIMSS data ###
    theYear <- as.numeric(getAttributes(data, "year"))
    theLevel <- getAttributes(data, "gradeLevel")
    theLevel <- as.numeric(unlist(regmatches(theLevel, gregexpr("[[:digit:]]+", theLevel))))
    theSubject <- all.vars(formula[[2]])
    
    # year check
    if (!theYear %in% c(2011, 2015, 2019)) {
      stop("Item parameters are currently only available for 2011, 2015, and 2019.")
    } 
    
    # subject check
    if(length(theSubject) > 1) {
      stop("the response must be a single test score.")
    }
    # test this is length 1
    theSubject <- theSubject[theSubject %in% c(names(getAttributes(data, "pvvars")))]
    if(length(theSubject) < 1) {
      stop(paste0("Cannot find TIMSS subject ", dQuote(all.vars(formula[[2]]))))
    }
    
    # get paramTabs
    polyParamTab <- getAttributes(data, "polyParamTab")
    dichotParamTab <- getAttributes(data, "dichotParamTab")
    testDat <- getAttributes(data, "testData")
    scoreDict <- getAttributes(data, "scoreDict")
    sCard <- getAttributes(data, "scoreCard") # this is null for timss
    
    if (nrow(polyParamTab)==0 | nrow(dichotParamTab)==0 | nrow(testDat)==0) {
      stop("IRT parameter values do not exist. Use the function setTIMSSScoreDict to continue.")
    }
    
    # check for paramTab items not in data
    items <- c(polyParamTab$ItemID, dichotParamTab$ItemID)
    itemsUse <- items[items %in% colnames(data)]
    itemsNotInData <- setdiff(items, itemsUse)
    if (length(itemsNotInData) > 0) {
      polyParamTab <- polyParamTab[polyParamTab$ItemID %in% itemsUse, ]
      dichotParamTab <- dichotParamTab[dichotParamTab$ItemID %in% itemsUse, ]
      warning(paste0('These items were in the assessment, but not in your data: ', pasteItems(itemsNotInData, final = 'and '))) 
    } 
    if (length(itemsUse) < 1){
      stop(paste0(dQuote(data), ' does not contain parameter items for ', sQuote(subject)))
    }
    
    # check for data items not in paramTab -- this is hard to do
    
    
    ### create stuItems 
    # get dependent vars, weight, and items from data
    indepVars <- labels(terms(formula))
    # TIMSS items are set to missing, must use omittedLevels=FALSE
    getDataArgs <- list(data=data, varnames=c(polyParamTab$ItemID, dichotParamTab$ItemID, indepVars, weightVar, strataVar, psuVar, idVar), omittedLevels = FALSE) 
    edf <- quietGetData(data, getDataArgs)
    # mml uses character id variables, so recast here
    edf[[idVar]] <- as.character(edf[[idVar]])
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
    edf <- scoreTIMSS(edf, polyParamTab, dichotParamTab, scoreDict)
    
    # creat stuItems
    stuItems <- as.data.frame(melt(as.data.table(edf[ , c(polyParamTab$ItemID,dichotParamTab$ItemID, idVar)]),
                                   id.vars=idVar,
                                   measure.vars=c(polyParamTab$ItemID,dichotParamTab$ItemID)))
    colnames(stuItems) <- c(idVar, 'key', 'score')
    
    ### create stuDat ###
    stuDat <- edf[edf[,idVar] %in% unique(stuItems[,idVar]), c(idVar, indepVars, weightVar, strataVar, psuVar)]
    
    ### warnings
    if(!missing(composite) && composite) {
      warning('Composite is not supported in TIMSS.')
    }
    composite <- FALSE 
    itemMapping <- c(`For NAEP only` = NULL) 
    #scoreDict <- c(`For NAEP only` = NULL) 
    
  } else if (survey == 'NAEP') {
    ### NAEP data ###
    checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
    
    # check scoreCard to see if we can continue
    sCard <- getAttributes(data, "scoreCard")
    polyParamTab <- getAttributes(data, "polyParamTab") 
    dichotParamTab <- getAttributes(data, "dichotParamTab")
    testDat <- getAttributes(data, "testData")
    if (nrow(sCard) == 0) {
      stop("A scoreCard does not exist for your data. Use a DCT file with the function setNAEPScoreCard to continue.")
    }

    # check for paramTab items not in data
    items <- c(polyParamTab$ItemID, dichotParamTab$ItemID)
    itemsUse <- items[items %in% colnames(data)]
    itemsNotInData <- setdiff(items, itemsUse)
    if (length(itemsNotInData) > 0) {
      polyParamTab <- polyParamTab[polyParamTab$ItemID %in% itemsUse, ]
      dichotParamTab <- dichotParamTab[dichotParamTab$ItemID %in% itemsUse, ]
      warning(paste0('These items were in the assessment, but not in your data: ', pasteItems(itemsNotInData, final = 'and '))) 
    } 
    if (length(itemsUse) < 1){
      stop(paste0(dQuote(data), ' does not contain parameter items.'))
    }
    
    # check for data items not in paramTab -- this is hard to do
    
    ### continue building paramTabs
    items <- unique(sCard$key)
    cols <- colnames(sCard)
    colnames(sCard) <- c('key', 'From', 'To')
    itemMapping <- c() 
    for (itm in items) {
      mapping <- list(with(sCard[sCard$key==itm,], table(From, To, useNA = 'ifany')))
      names(mapping) <- itm
      itemMapping <- c(itemMapping, mapping)
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
    
    # get dependent vars, weight, and items from data
    indepVars <- labels(terms(formula))
    getDataArgs <- list(data=data, varnames=c(polyParamTab$ItemID, dichotParamTab$ItemID, indepVars, weightVar, strataVar, psuVar, idVar), omittedLevels = FALSE) 
    edf <- quietGetData(data, getDataArgs)
    edf[[idVar]] <- as.character(edf[[idVar]])
    
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
    
    # reshape and score items
    stuItemsWide <- edf[ , c(idVar, c(polyParamTab$ItemID, dichotParamTab$ItemID))]
    stuItemsWide <- data.table(stuItemsWide)
    # may issue a warning about item classes. Not useful in this context.
    suppressWarnings(stuItemsLong <- melt(stuItemsWide, id.vars=idVar, measure.vars=c(polyParamTab$ItemID, dichotParamTab$ItemID)))
    colnames(stuItemsLong) <- c('id', 'key', 'answer')
    stuItemsLong <- stuItemsLong[!is.na(answer)]
    stuItems <- merge(stuItemsLong, sCard, by=c('key', 'answer'), all.x=TRUE)  # score through merge
    stuItems <- as.data.frame(stuItems[ , c("id", "key", "score")])
    colnames(stuItems)[1] <- idVar
    
    ### create stuDat ###
    stuDat <- edf[edf[,idVar] %in% unique(stuItems[,idVar]), c(idVar, indepVars, weightVar,stratavar=strataVar, psuvar=psuVar)]
    
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
  } # end if/else for NAEP or TIMSS

  
  # setting up cluster for multi-core 
  if(multiCore){
    # check if cluster is already running 
    if(nrow(showConnections()) == 0) {
      cores <- round(numberOfCores, 0) # use 75 percent of cores 
      cl <- parallel::makeCluster(cores)
  	  if(verbose >= 1) {
  	    message(paste0("Starting cluster with ", cl, " cores"))
      }
      doParallel::registerDoParallel(cl, cores=cores)
      # stop cluster before any exit
      on.exit( parallel::stopCluster(cl) )
    }
  } # end if(multiCore)
  

  if(verbose>0) {
    message("Starting MML Procedure.")
  }

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
                strataVar = strataVar,
                PSUVar = psuVar,
                weightVar = weightVar,
                fast = TRUE,
                multiCore = multiCore)

  # get call
  call <- match.call()
  # main mml.sdf class 
  obj <- structure(list("Call" = call, "mml" = mmlObj, "survey" = survey, "getDataArgs" = getDataArgs, "sCard"=sCard, "idVar" = idVar), 
                   class=c("mml.sdf"))
  
  # append composite class 
  if(survey == "NAEP") {
    obj$scoreDict <- getAttributes(data, "scoreDict")
    obj$itemMapping <-  itemMapping 
    class(obj) <- c("mml.NAEP", class(obj))
  } else {
    obj$scoreDict <- getAttributes(data, "scoreDict")
    obj$itemMapping <-  itemMapping 
    class(obj) <- c("mml.TIMSS", class(obj))
  }
  # return
  return(obj)
}

quietGetData <- function(data, args) {
  withCallingHandlers({
    edf <- do.call(getData, args)
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Updating labels on"))
      invokeRestart("muffleWarning")
  }) 
}

#' Purpose: 
#' Give error if Strata and PSU var not present on light edsurvey dataframe.
#' We may want to change these to warnings later.
#' But leaving as errors right now. Otherwise we'll need to change getData() call to not use 
#' getStatumVar() and getPSUVar() functions. 
checkPsuStrata <- function(data) {
  strataVar <- getStratumVar(data)
  psuVar <- getPSUVar(data)
  if(inherits(data,'light.edsurvey.data.frame')){
    if(!strataVar %in% colnames(data)) {
      stop(paste0("Missing strataVar, ", sQuote(strataVar), ". Please include",
              sQuote(strataVar), "in your getData() call."))
    } 
    if(!psuVar %in% colnames(data)) {
      stop(paste0("Missing strataVar, ", sQuote(strataVar), ". Please include",
                     sQuote(strataVar), "in your getData() call."))
    }
  }
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

#' Purpose: 
#' Give warning if idVar is null on on light edsurvey dataframe.
#' Without an idVar the users won't be able to merge PVs back to a light.edsurvey.data.frame.  
checkIdVar <- function(data, idVar){ 
  if(inherits(data,'light.edsurvey.data.frame')){
    if(is.null(idVar)){
      if(!"ROWID" %in% colnames(data)) {
        warning("Student level results can't be merged without a valid ", sQuote("idVar"), ". An arbitrary ID, ", sQuote("SID"), ", will be set.")
        idVar <- FALSE
      } else {
        idVar <- "ROWID"
      }
    }
  } else {
    if(inherits(data,'edsurvey.data.frame')){
      if(is.null(idVar)){
        if(!"ROWID" %in% colnames(data$cache)){
          warning("Student level results can't be merged without a valid ", sQuote("idVar"), ". An arbitrary ID, ", sQuote("SID"), ", will be set.")
          idVar <- FALSE
        } else {
          idVar <- "ROWID"
        }
      } else {
        if(!idVar %in% colnames(data)) {
          stop(paste0("Cannot find idVar ", dQuote(idVar), " on data"))
        }
      }
    }
  }
  return(idVar)
}
