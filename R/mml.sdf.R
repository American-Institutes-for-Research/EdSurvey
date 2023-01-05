#' @title EdSurvey Direct Estimation
#' @description Prepare IRT parameters and score items and then estimate a linear model with direct estimation.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   model.
#' @param data       an \code{edsurvey.data.frame} for the National Assessment of Educational Progress (NAEP) 
#'                  and the Trends in International Mathematics and Science Study (TIMSS). 
#'                  The attributes \code{dichotParamTab}, \code{polyParamTab}, \code{testData},
#'                  \code{scoreCard} (for NAEP), and \code{scoreDict} (for TIMSS) must not be \code{NULL}.
#'                  Use the function \code{setNAEPScoreCard} or \code{setAttributes} to set attributes.
#' @param weightVar  a character indicating the weight variable to use.
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, it  uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param omittedLevels a logical value. When set to the value of \code{TRUE}, drops
#'                      the levels of all factor variables that are specified
#'                      in an \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#'                      To draw plausible values for the full dataset, the user must set this to \code{FALSE}.
#' @param composite  logical; for a NAEP composite, setting to \code{FALSE} fits the model to all items at once,
#'                   in a single construct, whereas setting to \code{TRUE} fits the model as a NAEP composite
#'                   (i.e., a weighted average of the subscales). This argument is not applicable for TIMSS which is always
#'                   fit as an overall (non-composite). 
#' @param verbose logical; indicates whether a detailed printout should display during execution, only for NAEP data.
#' @param multiCore allows the \code{foreach} package to be used. This function will setup and take down the cluster.
#' @param numberOfCores the number of cores to be used when using \code{multiCore}. Defaults to 75\% of available cores. Users 
#'                      can check available cores with \code{detectCores()}. 
#' @param minNode numeric; minimum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param maxNode numeric; maximum integration point in direct estimation; see \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}.
#' @param Q integer; number of integration points per student used when integrating over the levels of the latent outcome construct. 
#' @param idVar a variable that is used to explicitly define the name of the student identifier 
#'              variable to be used from \code{data}. Defaults to \code{NULL}, and \code{sid} is used 
#'              as the student identifier. 
#' @param returnMmlCall logical; when \code{TRUE}, do not process the mml call but instead return it for the user to edit before calling
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
#' @aliases defaultNAEPScoreCard defaultTIMSSScoreDict
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
                    idVar = NULL,
                    returnMmlCall = FALSE) {
  stopifnot(inherits(verbose, c("numeric", "integer", "logical")))
  ### check data class 
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  
  has_IRTAttributes(data, errorCheck = TRUE) #ensure IRT attributes are set, throw error if not
  
  verbose <- as.numeric(verbose)
  # if the weight var is not set, use the default
  weightVar <- checkWeightVar(data,weightVar)
  # check for parrallel if multiCore True 
  if(any(grepl("_linking", all.vars(formula), fixed=TRUE))) {
    stop("mml.sdf does not support linking error.")
  }
  
  idVar <- fixIdVar(data, idVar, verbose)

  multiCoreSetup <- setupMulticore(multiCore, numberOfCores, verbose)

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
  
  ### check for Strata and Psu Var 
  checkPsuStrata(data)
  strataVar <- getStratumVar(data)
  psuVar <- getPSUVar(data)
  
  ### check idVar
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
  if(!survey %in% c("TIMSS", "NAEP")) {
    # Other assessments that are not TIMSS or NAEP
    stop("This assessment is unsupported by mml.sdf.")
  }

  # check the survey 
  theYear <- as.numeric(getAttributes(data, "year"))
  theSubject <- all.vars(formula[[2]])
  if(length(theSubject) != 1) {
    stop("There must be exactly one outcome for mml.sdf. Found:", pasteItems(dQuote(theSubject)))
  }
  # year check
  if (survey == 'TIMSS' && !theYear %in% c(2011, 2015, 2019)) {
    stop("Item parameters are currently only available for 2011, 2015, and 2019.")
  } 
  # test this is length 1
  theSubject <- theSubject[theSubject %in% names(getAttributes(data, "pvvars"))]
  if(length(theSubject) < 1) {
    stop(paste0("Cannot find subject ", dQuote(all.vars(formula[[2]]))))
  }

  scoreInfo <- getScoreInfo(data, survey, theSubject)
  # check for paramTab items not in data
  scoreInfo <- checkParamTabAgainstItems(data, scoreInfo)

  # ### create stuItems, stuDat
  # # get dependent vars, weight, and items from data
  indepVars <- all.vars(formula)
  indepVars <- indepVars[!indepVars %in% theSubject]
  getDataArgs <- list(data=data, varnames=c(scoreInfo$itemsUse, theSubject, indepVars, weightVar, strataVar, psuVar, idVar),
                      omittedLevels = omittedLevels) 
  edf <- quietGetData(data, getDataArgs)
  # mml uses character id variables, so recast here
  edf[[idVar]] <- as.character(edf[[idVar]])

  # check completeness
  edf <- filterOutIncompleteZeroWeight(edf, indepVars, weightVar, verbose)

  scoreCall <- getScoreCall(data)
  # make the score call enviornment
  scoreCallEnv <- list2env(scoreInfo)
  assign("edf", edf, envir = scoreCallEnv) # add edf to the environment
  edf <- eval(scoreCall, envir=scoreCallEnv)
  scoreFunction <- scoreCall[[1]]

  # return scored data
  pvs <- edf[ , c(scoreInfo$itemsUse, idVar)]
  pvs$id <- pvs[,idVar]
  pvs[,idVar] <- NULL
  # robust to light or full data frame
  data <- mergePVGeneral(data, pvs, idVar)

  # make long stuItems from the wide edf

  stuItems <- as.data.frame(melt(as.data.table(edf[ , c(scoreInfo$itemsUse, idVar)]),
                                 id.vars=idVar,
                                 measure.vars=c(scoreInfo$itemsUse)))
  # creat stuItems
  colnames(stuItems) <- c(idVar, 'key', 'score')
  # create stuDat 
  stuDat <- edf[edf[ , idVar] %in% unique(stuItems[ , idVar]), c(idVar, indepVars, weightVar, strataVar, psuVar)]

  # no composite in TIMSS
  if(composite && survey %in% "TIMSS") {
    mc <- missing(composite)
    # the next line sets missing(composite) to FALSE, so that must be tested above it
    composite <- FALSE
    if(!mc) {
      stop('Composite is not supported in TIMSS.')
    }
  }

  
  # setting up cluster for multi-core 
  startMulticore(multiCoreSetup, verbose=verbose)

  if(verbose>0) {
    message("Starting MML Procedure.")
  }

  waldDenomBaseDof <- waldDof(edf, getStratumVar(data), getPSUVar(data))

  clObj <- list(formula = formula,
                stuItems = stuItems,
                stuDat = stuDat,
                idVar = idVar,
                dichotParamTab = scoreInfo$dichotParamTab,
                polyParamTab = scoreInfo$polyParamTab,
                testScale = scoreInfo$testDat,
                Q = Q,
                composite = composite,
                minNode = minNode,
                maxNode = maxNode,
                strataVar = strataVar,
                PSUVar = psuVar,
                weightVar = weightVar,
                fast = TRUE,
                multiCore = multiCoreSetup$multiCore)
  if(returnMmlCall) {
    obj <- structure(list("Call"        = clObj,
                          "survey"      = survey,
                          "getDataArgs" = getDataArgs,
                          "scoreDict"   = scoreInfo$scoreDict,
                          "idVar"       = idVar,
                          "scoreFunction" = scoreFunction,
                          "waldDenomBaseDof" = waldDenomBaseDof),
                     class="mml.sdf.precall")
    return(obj)
  }
  mmlObj <- do.call(mml, clObj)

  # get call
  call <- match.call()
  # main mml.sdf class 
  obj <- structure(list("Call"        = call,
                        "mml"         = mmlObj,
                        "survey"      = survey,
                        "getDataArgs" = getDataArgs,
                        "scoreDict"   = scoreInfo$scoreDict,
                        "idVar"       = idVar,
                        "scoreFunction" = scoreFunction,
                        "waldDenomBaseDof" = waldDenomBaseDof,
                        "data"        = data), 
                   class=c("mml.sdf"))
  # append composite class 
  obj$scoreDict <- scoreInfo$scoreDict
  obj$theSubject <- theSubject
  if(survey == "NAEP") {
    class(obj) <- c("mml.NAEP", class(obj))
  } else {
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

#' Give error if Strata and PSU var not present on light edsurvey data frame.
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} to check for PSU and Strata variables
#' @keywords internal
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

#' Give warning if idVar is null on on light edsurvey dataframe.
#' @param data an \code{edsurvey.data.frame}, \code{light.edsurvey.data.frame}, or \code{data.frame} to look for the \code{idVar} on
#' @param idVar the variable to look for
#' Without an idVar the users won't be able to merge PVs back to the data after running an mml.  If there is no idVar, a default is chosen.
#' @keywords internal
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

filterParamTabToSubject <- function(paramTab, survey, subject) {
  # identify which TIMSS subtest to use, if necessary
  if(survey == "TIMSS") {
    # default, so there is a subtest
    paramTab$subtest <- paramTab$content_subtest
    if(subject %in% paramTab$cognitive_subtest) {
      paramTab$subtest <- paramTab$cognitive_subtest
    }
    paramTab$content_subtest <- NULL
    paramTab$cognitive_subtest <- NULL
  }
  if(! "test" %in% colnames(paramTab)) {
    stop(paste0("paramTab does not must contain a column named ", dQuote("test")))
  }
  if(! "subtest" %in% colnames(paramTab)) {
    stop(paste0("paramTab does not must contain a column named ", dQuote("subtest")))
  }
  paramTab <- paramTab[paramTab$test %in% subject | paramTab$subtest %in% subject, ]
  return(paramTab)
}

getScoreCall <- function(data) {
  scoreFunction <- getAttributes(data, "scoreFunction")
  if(is.null(scoreFunction)) {
    stop("attribute scoreFunction must be set on the data.")
  }
  if(inherits(scoreFunction, "function")) {
    scoreCall <- list(scoreFunction, quote(edf), quote(polyParamTab), quote(dichotParamTab), quote(scoreDict))
    mode(scoreCall) <- "call"
  } else {
    scoreCall <- call(scoreFunction, quote(edf), quote(polyParamTab), quote(dichotParamTab), quote(scoreDict))
  }
  return(scoreCall)
}

getScoreInfo <- function(data, survey, theSubject) {
  polyParamTab <- getAttributes(data, "polyParamTab") 
  polyParamTab <- filterParamTabToSubject(polyParamTab, survey, theSubject)
  dichotParamTab <- getAttributes(data, "dichotParamTab")
  dichotParamTab <- filterParamTabToSubject(dichotParamTab, survey, theSubject)
  testDat <- getAttributes(data, "testData")
  scoreDict <- getAttributes(data, "scoreDict")

  if(!theSubject %in% c(testDat$subtest, testDat$test)) {
    stop(paste0("Cannot find ", dQuote(theSubject), " in the testData columns ", dQuote("subtest"), " or ", dQuote("test")))
  }
  if(!theSubject %in% c(dichotParamTab$subtest, dichotParamTab$test, polyParamTab$subtest, polyParamTab$test)) {
    stop(paste0("Cannot find ", dQuote(theSubject), " in either the dichotParamTab nor polyParamTab columns ", dQuote("subtest"), " or ", dQuote("test")))
  }
  return(list(polyParamTab=polyParamTab, dichotParamTab=dichotParamTab, testDat=testDat, scoreDict=scoreDict))
}

checkParamTabAgainstItems <- function(data, scoreInfo) {
  ppt <- scoreInfo$polyParamTab
  dpt <- scoreInfo$dichotParamTab
  items <- c(ppt$ItemID, dpt$ItemID)
  itemsUse <- items[items %in% colnames(data)]
  itemsNotInData <- setdiff(items, itemsUse)

  if (length(itemsNotInData) > 0) {
    ppt <- ppt[ppt$ItemID %in% itemsUse, ]
    dpt <- dpt[dpt$ItemID %in% itemsUse, ]
    warning(paste0('These items were in the assessment, but not in your data: ', pasteItems(dQuote(itemsNotInData), final = 'and '))) 
  }
  if (length(itemsUse) < 1){
    stop(paste0(dQuote("data"), ' does not contain any of the required items: ', pasteItems(dQuote(itemsNotInData), final = 'and ')))
  }

  # check poly param tab has score points that agree with parameters
  dCols <- grep("d[0-9]", colnames(ppt), value=TRUE)
  apparentScorePoints <- apply(ppt[ , dCols, drop=FALSE], 1, function(x) {length(dCols) - sum(is.na(x))} )
  if(!"socrePoints" %in% colnames(ppt)) {
    ppt$scorePoints <- apparentScorePoints
  }
  ppt[is.na(ppt$scorePoints),'scorePoints'] <- 0
  if (any(ppt$scorePoints != apparentScorePoints)) {
    badIDs <- ppt[ppt$scorePoints != apparentScorePoints, 'ItemID']
    ppt <- ppt[ppt$scorePoints == apparentScorePoints, ]
    warning(paste0(pasteItems(badIDs), ' did not have enough parameters for the max score and will not be used.'))
  }
  # null these out instead of passing a zero length param tab
  if (nrow(ppt) == 0) {
    ppt <- NULL
  }
  if (nrow(dpt) == 0) {
    dpt <- NULL
  }
  scoreInfo$polyParamTab <- ppt
  scoreInfo$dichotParamTab <- dpt
  scoreInfo$itemsUse <- itemsUse
  return(scoreInfo)
}

filterOutIncompleteZeroWeight <- function(edf, indepVars, weightVar, verbose) {
  incomplete <- !complete.cases(edf[ , c(indepVars, weightVar)])
  if(any(incomplete)) {
    if(verbose) {
      message("Removing ", sum(incomplete), " rows with NAs from analysis.")
    }
    edf <- edf[!incomplete, , drop=FALSE]
  }
  # remove non-positive (full sample) weights
  if(!is.null(weightVar)) {
    if(any(edf[ , weightVar] <= 0)) {
      posWeights <- edf[ , weightVar] > 0
      if(verbose) {
        message("Removing ", sum(!posWeights), " rows with nonpositive weight from analysis.")
      }
      edf <- edf[posWeights, ]
    }
  }
  # check that there is some data to work with
  if(nrow(edf) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                sQuote("getData"), ", incomplete cases, and non-positive weights are removed."))
  }
  return(edf)
}

fixIdVar <- function(data, idVar, verbose) {
  # if 1) there is no idVar, 2) ROWID is on the data, 3) ROWID is unique, make it the idVar
  if(is.null(idVar)) {
    if( is.null(idVar) && "ROWID" %in% colnames(data) && length(unique(data[["ROWID"]])) == length(data[["ROWID"]])) {
      idVar <- "ROWID"
    } else {
      if(verbose > 0) {
        message("no idVar set. This is necessary to draw plausible values.")
      }
    }
  }
  idVar <- checkIdVar(data, idVar) 
  return(idVar)
}

#returns TRUE/FALSE if an edsurvey.data.frame or light.edsurvey.data.frame have appropriate IRT attributes set
#set errorCheck = TRUE to throw warning about what part of the irt attributes are missing
has_IRTAttributes <- function(esdf, errorCheck = FALSE){
  #validate inputs
  checkDataClass(esdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  stopifnot("errorCheck must be a logical value of length 1" = is.logical(errorCheck) && length(errorCheck) == 1)
  
  isNAEP <- grepl("NAEP", getAttributes(esdf, "survey", errorCheck = errorCheck), ignore.case = TRUE)
  
  #these might not be set after rebindAttributes is called, we will throw our own errors here
  dichotParam <- getAttributes(esdf, "dichotParamTab", errorCheck = FALSE)
  polyParam <- getAttributes(esdf, "polyParamTab", errorCheck = FALSE)
  testData <- getAttributes(esdf, "testData", errorCheck = FALSE)
  scoreCard <- getAttributes(esdf, "scoreCard", errorCheck = FALSE)
  scoreDict <- getAttributes(esdf, "scoreDict", errorCheck = FALSE)
  scoreFunction <- getAttributes(esdf, "scoreFunction", errorCheck = FALSE)

  eMsg <- paste0("Missing IRT Attributes for this edsurvey.data object. IRT attributes required for use with ", dQuote("mml.sdf"), " function. See ?mml.sdf help for details.")
  if(isNAEP){
    eMsg <- paste0(eMsg, " Try setting IRT attributes with the AM .dct file(s) using the ", dQuote("setNAEPScoreCard"), " function.")
  }

  hasItems <- (inherits(dichotParam, "data.frame") && inherits(polyParam, "data.frame"))
  hasTests <- inherits(testData, "data.frame")
  hasScore <- (inherits(scoreCard, "data.frame") || inherits(scoreDict, "data.frame"))
  hasFunction <- !is.null(scoreFunction)
  
  if(!all(c(hasItems, hasTests, hasScore, hasFunction))){
    if(errorCheck){
      stop(eMsg)
    }
    return(FALSE)
  }
  
  #ALL TESTS PASSED, RETURN TRUE
  return(TRUE)
}