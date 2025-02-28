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
#' @param dropOmittedLevels a logical value. When set to the value of \code{TRUE}, drops
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
#' @param optimizer passed to \ifelse{latex}{\code{mml}}{\code{\link[Dire]{mml}}}
#'
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}
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
#' An \code{mml.sdf} object, which is the fit for the \code{mml.sdf}, with the following elements:
#'    \item{Call}{the call used to generate this object}
#'    \item{mml}{an object containing information from the \code{mml} procedure.
#'               \code{?mml} can be used for further information.}
#'    \item{survey}{the name of the survey}
#'    \item{getDataArgs}{the arguments used to call getData}
#'    \item{scoreDict}{a dictionary used when building the scoring function}
#'    \item{scoreFunction}{the function that scores the data, turning the data in the format it was provided to a numeric raw score}
#'    \item{idVar}{the name of the id variable used to uniquely identify a row}
#'    \item{waldDenomBaseDof}{the denominator degrees of freedom for Wald tests}
#'    \item{data}{the data object used in the call}
#'    \item{theSubject}{the name of the outcome variable}
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
                    dropOmittedLevels = TRUE,
                    composite = TRUE,
                    verbose = 0,
                    multiCore = FALSE,
                    numberOfCores = NULL,
                    minNode = -4,
                    maxNode = 4,
                    Q = 34,
                    idVar = NULL,
                    returnMmlCall = FALSE,
                    omittedLevels = deprecated(),
                    optimizer = c("EM", "QN")) {
  stopifnot(inherits(verbose, c("numeric", "integer", "logical")))
  ### check data class
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  optimizer <- match.arg(optimizer)

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "mml.sdf(omittedLevels)", "mml.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")

  has_IRTAttributes(data, errorCheck = TRUE) # ensure IRT attributes are set, throw error if not

  verbose <- as.numeric(verbose)
  # if the weight var is not set, use the default
  weightVar <- checkWeightVar(data, weightVar)
  # check for parrallel if multiCore True
  if (any(grepl("_linking", all.vars(formula), fixed = TRUE))) {
    stop("mml.sdf does not support linking error.")
  }

  idVar <- fixIdVar(data, idVar, verbose)

  if (verbose > 0) {
    message("Gathering item information.")
  }

  # check for no response
  if (is.null(getResponse(formula))) {
    stop("Please specify response variable. Equation should be of the form: y ~ x")
  }
  ### check for transformations in equations: sqrt(mmat), exp(mmat), mmat^0.5, mmat^(1/3), log(mmat),
  if (all(grepl("\\^\\d+|log\\(.*\\)|\\^\\(.*\\)|exp\\(.*\\)|sqrt\\(.*\\)", formula[2:3]))) {
    stop("Please transform variable outside of equations.")
  }

  ### check for Strata and Psu Var
  checkPsuStrata(data)
  strataVar <- getStratumVar(data)
  psuVar <- getPSUVar(data)

  ### check idVar
  # if there was no ID, add one
  if (FALSE %in% idVar) {
    data$sid <- 1:nrow(data)
    idVar <- "sid"
  }
  # be sure tbl data is correctly converted to factor
  if (inherits(data, "light.edsurvey.data.frame")) {
    data <- lightUnclassCols(data)
  }
  # check survey
  survey <- getAttributes(data, "survey")
  if (!survey %in% c("TIMSS", "NAEP")) {
    # Other assessments that are not TIMSS or NAEP
    stop("This assessment is unsupported by mml.sdf.")
  }

  # check the survey
  theYear <- as.numeric(getAttributes(data, "year"))
  theSubject <- all.vars(formula[[2]])
  if (length(theSubject) != 1) {
    stop("There must be exactly one outcome for mml.sdf. Found:", pasteItems(dQuote(theSubject)))
  }
  # year check
  if (survey == "TIMSS" && !theYear %in% c(2011, 2015, 2019)) {
    stop("Item parameters are currently only available for 2011, 2015, and 2019.")
  }
  # test this is length 1
  theSubject <- theSubject[theSubject %in% names(getAttributes(data, "pvvars"))]
  if (length(theSubject) < 1) {
    stop(paste0("Cannot find subject ", dQuote(all.vars(formula[[2]]))))
  }

  scoreInfo <- getScoreInfo(data, survey, theSubject)
  # check for paramTab items not in data
  scoreInfo <- checkParamTabAgainstItems(data, scoreInfo)
  # ### create stuItems, stuDat
  # # get dependent vars, weight, and items from data
  indepVars <- all.vars(formula)
  indepVars <- indepVars[!indepVars %in% theSubject]
  if(verbose > 0) {
    message("Preparing data.")
  }
  getDataArgs <- list(
    data = data, varnames = c(scoreInfo$itemsUse, theSubject, indepVars, weightVar, strataVar, psuVar, idVar),
    dropOmittedLevels = dropOmittedLevels
  )
  edf <- quietGetData(data, getDataArgs)
  # mml uses character id variables, so recast here
  edf[[idVar]] <- as.character(edf[[idVar]])

  # check completeness
  edf <- filterOutIncompleteZeroWeight(edf, indepVars, weightVar, verbose)
  if(verbose > 0) {
    message("Scoring data.")
  }
  scoreDict <- data$scoreDict
  scoreFunction <- scoreInfo$scoreFunction
  edf <- scoreFunction(edf, scoreInfo$polyParamTab, scoreInfo$dichotParamTab, scoreInfo$scoreDict)

  # return scored data
  pvs <- edf[ , c(scoreInfo$itemsUse, idVar)]
  # robust to light or full data frame
  data <- merge(data, pvs, by=idVar)

  stuDat <- edf[, c(idVar, indepVars, weightVar, strataVar, psuVar, scoreInfo$itemsUse)]
  if (nrow(stuDat) == 0) {
    stop("No students with valid test data also have valid covariates.")
  }
  # no composite in TIMSS
  if (composite && survey %in% "TIMSS") {
    mc <- missing(composite)
    if (mc) {
      composite <- FALSE
    } else {
      # the next line sets missing(composite) to FALSE, so that must be tested above it
      testDat <- getAttributes(data, "testData", errorCheck = FALSE)
      subTestDat <- testDat[testDat$test %in% theSubject, ]
      if (nrow(subTestDat) == 0 || any(is.na(subTestDat$weights))) {
        stop("Composite is not recomended for TIMSS.")
      }
    }
    # the user seems to know what they are doing, they added weights. So allow it
  }

  waldDenomBaseDof <- waldDof(edf, getStratumVar(data), getPSUVar(data))
  multiCoreSetup <- setupMulticore(multiCore, numberOfCores, verbose)

  clObj <- list(
    formula = formula,
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
    verbose= max(0,verbose -1),
    multiCore = multiCoreSetup$multiCore,
    optimizer = optimizer
  )
  if (returnMmlCall) {
    return(clObj)
  }
  if (verbose > 0) {
    message("Starting MML Procedure.")
  }
  # setting up cluster for multi-core
  startMulticore(multiCoreSetup, verbose = verbose)
  mmlObj <- do.call(mml, clObj)

  # get call
  call <- match.call()
  mmlObj$call <- call
  # main mml.sdf class
  obj <- structure(
    list(
      "Call" = call,
      "mml" = mmlObj,
      "survey" = survey,
      "getDataArgs" = getDataArgs,
      "scoreDict" = scoreInfo$scoreDict,
      "idVar" = idVar,
      "scoreFunction" = scoreFunction,
      "waldDenomBaseDof" = waldDenomBaseDof,
      "data" = data
    ),
    class = c("mml.sdf")
  )
  # append composite class
  obj$scoreDict <- scoreInfo$scoreDict
  obj$theSubject <- theSubject
  if (survey == "NAEP") {
    class(obj) <- c("mml.NAEP", class(obj))
  } else {
    class(obj) <- c("mml.TIMSS", class(obj))
  }
  # return
  return(obj)
}

quietGetData <- function(data, args) {
  withCallingHandlers(
    {
      edf <- do.call(getData, args)
    },
    warning = function(w) {
      if (startsWith(conditionMessage(w), "Updating labels on")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Give error if Strata and PSU var not present on light edsurvey data frame.
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} to check for PSU and Strata variables
#' @keywords internal
checkPsuStrata <- function(data) {
  strataVar <- getStratumVar(data)
  psuVar <- getPSUVar(data)
  if (inherits(data, "light.edsurvey.data.frame")) {
    if (!strataVar %in% colnames(data)) {
      stop(paste0(
        "Missing strataVar, ", sQuote(strataVar), ". Please include",
        sQuote(strataVar), "in your getData() call."
      ))
    }
    if (!psuVar %in% colnames(data)) {
      stop(paste0(
        "Missing psuVar, ", sQuote(psuVar), ". Please include",
        sQuote(psuVar), "in your getData() call."
      ))
    }
  }
}

#' @importFrom stats terms.formula
getResponse <- function(form) {
  tf <- terms.formula(form)
  respIndex <- attr(tf, "response")
  if (respIndex == 0) {
    return(NULL)
  }
  return(attr(tf, "variables")[[1 + attr(tf, "response")]])
}

#' Give warning if idVar is null on on light edsurvey dataframe.
#' @param data an \code{edsurvey.data.frame}, \code{light.edsurvey.data.frame}, or \code{data.frame} to look for the \code{idVar} on
#' @param idVar the variable to look for
#' Without an idVar the users won't be able to merge PVs back to the data after running an mml.  If there is no idVar, a default is chosen.
#' @keywords internal
checkIdVar <- function(data, idVar) {
  if (inherits(data, "light.edsurvey.data.frame")) {
    if (is.null(idVar)) {
      if (!"ROWID" %in% colnames(data)) {
        warning("Student level results can't be merged without a valid ", sQuote("idVar"), ". An arbitrary ID, ", sQuote("SID"), ", will be set.")
        idVar <- FALSE
      } else {
        idVar <- "ROWID"
      }
    }
  } else {
    if (inherits(data, "edsurvey.data.frame")) {
      if (is.null(idVar)) {
        if (!"ROWID" %in% colnames(data$cache)) {
          warning("Student level results can't be merged without a valid ", sQuote("idVar"), ". An arbitrary ID, ", sQuote("SID"), ", will be set.")
          idVar <- FALSE
        } else {
          idVar <- "ROWID"
        }
      } else {
        if (!idVar %in% colnames(data)) {
          stop(paste0("Cannot find idVar ", dQuote(idVar), " on data"))
        }
      }
    }
  }
  return(idVar)
}

filterParamTabToSubject <- function(paramTab, survey, subject) {
  # identify which TIMSS subtest to use, if necessary
  if (survey == "TIMSS") {
    # default, so there is a subtest
    paramTab$subtest <- paramTab$content_subtest
    if (subject %in% paramTab$cognitive_subtest) {
      paramTab$subtest <- paramTab$cognitive_subtest
    }
    paramTab$content_subtest <- NULL
    paramTab$cognitive_subtest <- NULL
  }
  if (!"test" %in% colnames(paramTab)) {
    stop(paste0("paramTab does not must contain a column named ", dQuote("test")))
  }
  if (!"subtest" %in% colnames(paramTab)) {
    stop(paste0("paramTab does not must contain a column named ", dQuote("subtest")))
  }
  paramTab <- paramTab[paramTab$test %in% subject | paramTab$subtest %in% subject, ]
  return(paramTab)
}

getScoreInfo <- function(data, survey, theSubject) {
  polyParamTab <- getAttributes(data, "polyParamTab", errorCheck = FALSE)
  polyParamTab <- filterParamTabToSubject(polyParamTab, survey, theSubject)
  dichotParamTab <- getAttributes(data, "dichotParamTab", errorCheck = FALSE)
  dichotParamTab <- filterParamTabToSubject(dichotParamTab, survey, theSubject)
  testDat <- getAttributes(data, "testData", errorCheck = FALSE)
  scoreDict <- getAttributes(data, "scoreDict", errorCheck = FALSE)
  if (!theSubject %in% c(testDat$subtest, testDat$test)) {
    stop(paste0("Cannot find ", dQuote(theSubject), " in the testData columns ", dQuote("subtest"), " or ", dQuote("test")))
  }
  if (!theSubject %in% c(dichotParamTab$subtest, dichotParamTab$test, polyParamTab$subtest, polyParamTab$test)) {
    stop(paste0("Cannot find ", dQuote(theSubject), " in either the dichotParamTab nor polyParamTab columns ", dQuote("subtest"), " or ", dQuote("test")))
  }
  scoreFunction <- getAttributes(data, "scoreFunction", errorCheck = TRUE)
  return(list(polyParamTab = polyParamTab, dichotParamTab = dichotParamTab, testDat = testDat, scoreDict = scoreDict, scoreFunction = scoreFunction))
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
    warning(paste0("These items were in the assessment, but not in your data: ", pasteItems(dQuote(itemsNotInData), final = "and ")))
  }
  if (length(itemsUse) < 1) {
    stop(paste0(dQuote("data"), " does not contain any of the required items: ", pasteItems(dQuote(itemsNotInData), final = "and ")))
  }

  # check poly param tab has score points that agree with parameters
  if(!"d0" %in% colnames(ppt)) {
    ppt$d0 <- 0
  }
  dCols <- grep("d[0-9]", colnames(ppt), value = TRUE)
  apparentScorePoints <- -1 + apply(ppt[ , dCols, drop = FALSE], 1, function(x) {
    length(dCols) - sum(is.na(x))
  })
  if (!"scorePoints" %in% colnames(ppt)) {
    ppt$scorePoints <- apparentScorePoints
  }
  ppt[is.na(ppt$scorePoints), "scorePoints"] <- 0
  if (any(ppt$scorePoints != apparentScorePoints)) {
    badIDs <- ppt[ppt$scorePoints != apparentScorePoints, "ItemID"]
    ppt <- ppt[ppt$scorePoints == apparentScorePoints, ]
    warning(paste0(pasteItems(badIDs), " did not have enough parameters for the max score and will not be used."))
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

filterOutIncompleteZeroWeight <- function(edf, indepVars, weightVar, verbose = FALSE) {
  incomplete <- !complete.cases(edf[ , c(indepVars, weightVar)])
  if (any(incomplete)) {
    if (verbose) {
      message("Removing ", sum(incomplete), " rows with NAs from analysis.")
    }
    edf <- edf[!incomplete, , drop = FALSE]
  }
  # remove non-positive (full sample) weights
  if (!is.null(weightVar)) {
    if (any(edf[ , weightVar] <= 0)) {
      posWeights <- edf[ , weightVar] > 0
      if (verbose) {
        message("Removing ", sum(!posWeights), " rows with nonpositive weight from analysis.")
      }
      edf <- edf[posWeights, ]
    }
  }
  # check that there is some data to work with
  if (nrow(edf) <= 0) {
    stop(paste0(
      sQuote("data"), " must have more than 0 rows after a call to ",
      sQuote("getData"), ", incomplete cases, and non-positive weights are removed."
    ))
  }
  return(edf)
}

fixIdVar <- function(data, idVar, verbose) {
  # if 1) there is no idVar, 2) ROWID is on the data, 3) ROWID is unique, make it the idVar
  if (is.null(idVar)) {
    if (is.null(idVar) && "ROWID" %in% colnames(data) && length(unique(data[["ROWID"]])) == length(data[["ROWID"]])) {
      idVar <- "ROWID"
    } else {
      if (verbose > 0) {
        message("no idVar set. This is necessary to draw plausible values.")
      }
    }
  }
  idVar <- checkIdVar(data, idVar)
  return(idVar)
}

# returns TRUE/FALSE if an edsurvey.data.frame or light.edsurvey.data.frame have appropriate IRT attributes set
# set errorCheck = TRUE to throw warning about what part of the irt attributes are missing
#' @importFrom stats reformulate
has_IRTAttributes <- function(esdf, errorCheck = FALSE) {
  # validate inputs
  checkDataClass(esdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  stopifnot("errorCheck must be a logical value of length 1" = is.logical(errorCheck) && length(errorCheck) == 1)

  isNAEP <- grepl("NAEP", getAttributes(esdf, "survey", errorCheck = errorCheck), ignore.case = TRUE)

  # these might not be set after rebindAttributes is called, we will throw our own errors here
  dichotParam <- getAttributes(esdf, "dichotParamTab", errorCheck = FALSE)
  polyParam <- getAttributes(esdf, "polyParamTab", errorCheck = FALSE)
  testData <- getAttributes(esdf, "testData", errorCheck = FALSE)
  scoreCard <- getAttributes(esdf, "scoreCard", errorCheck = FALSE)
  scoreDict <- getAttributes(esdf, "scoreDict", errorCheck = FALSE)
  scoreFunction <- getAttributes(esdf, "scoreFunction", errorCheck = FALSE)

  eMsg <- paste0("Missing IRT Attributes for this edsurvey.data object. IRT attributes required for use with ", dQuote("mml.sdf"), " function. See ?mml.sdf help for details.")
  if (isNAEP) {
    eMsg <- paste0(eMsg, " Try setting IRT attributes with the AM .dct file(s) using the ", dQuote("setNAEPScoreCard"), " function.")
  }

  hasItems <- (inherits(dichotParam, "data.frame") && inherits(polyParam, "data.frame"))
  hasTests <- inherits(testData, "data.frame")
  hasScore <- (inherits(scoreCard, "data.frame") || inherits(scoreDict, "data.frame"))
  hasFunction <- !is.null(scoreFunction)

  if (!all(c(hasItems, hasTests, hasScore, hasFunction))) {
    if (errorCheck) {
      stop(eMsg)
    }
    return(FALSE)
  }

  # ALL TESTS PASSED, RETURN TRUE
  return(TRUE)
}

# this function removes covariates that have exactly one level from the right hand side.
# it also removed associated interactions. This is purely to speed up the SVD.
# dat a data frame
# all_covs is a vector with all the terms on the right hand side of an equation.
fix_all_covs <- function(data, formula) {
  # note that terms with e.g. x*z become "x", "z", "x:z" in term.labels, so we need to
  # search for ":" and never "*"
  ft <- terms(formula)
  all_covs <- attr(ft, "term.labels")
  a <- data
  # levels we are keeping
  covs_all <- all_covs[grepl("[:]", all_covs)]
  for(i in 1:length(all_covs)) {
    if(!grepl("[:]", all_covs[i])) {
      if(length(unique(a[,all_covs[i]])) > 1) {
        # keep this
        covs_all <- c(covs_all, all_covs[i])
      } else {
        # drop this (simply do not add it to covs_all)
        # also drop covariates that have this in it before a colon
        also_drop <- covs_all[grepl(paste0("^",all_covs[i],"[:]"), covs_all)]
        for(j in seq_along(also_drop)) {
          covs_all <- covs_all[covs_all != also_drop[j]]
        }
        # also drop covariates that have this in it after a colon
        also_drop <- covs_all[grepl(paste0("[:]",all_covs[i],"$"), covs_all)]
        for(j in seq_along(also_drop)) {
           covs_all <- covs_all[covs_all != also_drop[j]]
        }
      }
    }  
  }
  response <- all.vars(formula)[attr(ft, "response")]
  use_intercept <- attr(ft, "intercept")==1
  new_formula <- reformulate(covs_all, response, intercept=use_intercept)
  return(new_formula)
}

#' @title variable recoding function
#' @description Prep variables for calls to \code{\link{mml.sdf}}
#'
#' @param data a data frame to be edited
#' @param vars variables to modify
#' @param from recode from these levels
#' @param to recode to this level, or levels
#'
#' @details
#' If \code{to} is length 1, then all variables in \code{vars} are recoded from every \code{from}
#' to the level of \code{to}.
#'
#' When \code{to} is the same length as \code{from} then the ith level of \code{from} is recoded
#' to the ith level of \code{to}.
#' 
#' @return
#' the data with each variable in \code{vars} recoded from \code{missingFrom} to \code{missingTo}
#' @export
es_recode <- function(data, vars, from, to) {
  if(!all(vars %in% colnames(data))) {
    stop("Could not find these columns on data argument ", paste(dQuote(vars[!vars %in% colnames(data)]), collapse=", "))
  }
  if(length(to) %in% 1) {
    for(i in 1:length(vars)) {
      data[ , vars[i]] <- factor(ifelse(data[ , vars[i]] %in% from, to, as.character(data[ , vars[i]])))
    }
  } else{
    if(length(to) %in% length(from)) {
      for(i in 1:length(vars)) {
        data[ , vars[i]] <- as.character(data[ , vars[i]])
        for(j in 1:length(to)) {
          data[ , vars[i]] <- ifelse(data[ , vars[i]] %in% from[j], to[j], data[ , vars[i]])
        }
      data[ , vars[i]] <- factor(data[ , vars[i]])
      }
    } else {
      stop("Either the length of missingTo should be length 1 or it should be the same length as from")
    }
  }
}
