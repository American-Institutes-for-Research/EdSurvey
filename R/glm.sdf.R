#' @title EdSurvey Generalized Linear Models
#'
#' @description Fits a logit or probit that
#'              uses weights and variance estimates
#'              appropriate for the \code{edsurvey.data.frame},
#'              the \code{light.edsurvey.data.frame}, or the \code{edsurvey.data.frame.list}.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   linear model. See \ifelse{latex}{\code{glm}}{\code{\link[stats]{glm}}}.
#'                   For logit and probit, we recommend using the \code{I()} function
#'                   to define the level used for success. (See Examples.)
#' @param data       an \code{edsurvey.data.frame}
#' @param family     the \code{glm.sdf} function currently fits only the binomial
#'                   outcome models, such as logit and probit, although other link
#'                   functions are available for binomial models. See the \code{link}
#'                   argument in the help for
#'                   \ifelse{latex}{\code{family}}{\code{\link[stats]{family}}}.
#' @param jrrIMax    the \code{Vjrr} sampling variance term (see
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with
#'                   any positive number of plausible values and is estimated on
#'                   the lower
#'                   of the number of available plausible values and \code{jrrIMax}. When
#'                   \code{jrrIMax} is set to \code{Inf}, all plausible values will be used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more
#'                   accurate variance estimates.
#' @param weightVar  character indicating the weight variable to use (see Details).
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param relevels   a list; used to change the contrasts from the
#'                   default treatment contrasts to the treatment contrasts with a chosen omitted
#'                   group. The name of each element should be the variable name, and the value
#'                   should be the group to be omitted.
#' @param varMethod  a character set to \dQuote{jackknife} or \dQuote{Taylor} that indicates the variance
#'                   estimation method to be used. See Details.
#' @param dropOmittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      those levels of all factor variables that are specified
#'                      in \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses
#'                          the default conditions stored in an \code{edsurvey.data.frame}
#'                          to subset the data. Use \code{print} on an
#'                          \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'               \code{recode=} \code{list(}\code{var1=} \code{list(from=} \code{c("a",} \code{"b",} \code{"c"),} \code{to=}\code{"d"))}.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of
#'                          primary sampling units (PSUs)
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allow for
#'                           the computation
#'                           of covariances between estimates.
#'
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}
#'
#' @details
#' This function implements an estimator that correctly handles left-hand side
#' variables that are logical, allows for survey sampling weights, and estimates
#' variances using the jackknife replication or Taylor series.
#' The vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#' describes estimation of the reported statistics and how it depends on \code{varMethod}.
#'
#' The coefficients are estimated
#' using the sample weights according to the section
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Not Present}
#' or the section
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Present,}
#' depending on if there are assessment variables or variables with plausible values
#' in them.
#'
#' How the standard errors of the coefficients are estimated depends on the
#' presence of plausible values (assessment variables),
#' But once it is obtained, the \emph{t} statistic
#' is given by \deqn{t=\frac{\hat{\beta}}{\sqrt{\mathrm{var}(\hat{\beta})}}} where
#' \eqn{ \hat{\beta} } is the estimated coefficient and \eqn{\mathrm{var}(\hat{\beta})} is
#' its variance of that estimate.
#'
#' \code{logit.sdf} and \code{probit.sdf} are included for convenience only;
#' they give the same results as a call to \code{glm.sdf} with the binomial family
#' and the link function named in the function call (logit or probit).
#' By default, \code{glm} fits a logistic regression when \code{family} is not set,
#' so the two are expected to give the same results in that case.
#' Other types of generalized linear models are not supported.
#'
#' \subsection{Variance estimation of coefficients}{
#'   All variance estimation methods are shown in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#'   When the predicted
#'   value does not have plausible values and \code{varMethod} is set to
#'   \code{jackknife}, the variance of the coefficients
#'   is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Not Present, Using the Jackknife Method.}
#'
#'   When plausible values are present and \code{varMethod} is set to
#'   \code{jackknife}, the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Present, Using the Jackknife Method.}
#'
#'   When the predicted
#'   value does not have plausible values and \code{varMethod} is set to
#'   \code{Taylor}, the variance of the coefficients
#'   is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Not Present, Using the Taylor Series Method.}
#'
#'   When plausible values are present and \code{varMethod} is set to
#'   \code{Taylor}, the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Present, Using the Taylor Series Method.}
#' }
#'
#' @section Testing:
#' Of the common hypothesis tests for joint parameter testing, only the Wald
#' test is widely used with plausible values and sample weights. As such, it
#' replaces, if imperfectly, the Akaike Information Criteria (AIC), the
#' likelihood ratio test, chi-squared, and analysis of variance (ANOVA, including \emph{F}-tests).
#' See \code{\link{waldTest}} or
#' the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-WaldTest.pdf}{\emph{Methods and Overview of Using EdSurvey for Running Wald Tests}}.
#'
#' @aliases logit.sdf probit.sdf
#'
#' @return
#' An \code{edsurveyGlm} with the following elements:
#'    \item{call}{the function call}
#'    \item{formula}{the formula used to fit the model}
#'    \item{coef}{the estimates of the coefficients}
#'    \item{se}{the standard error estimates of the coefficients}
#'    \item{Vimp}{the estimated variance caused by uncertainty in the scores (plausible value variables)}
#'    \item{Vjrr}{the estimated variance from sampling}
#'    \item{M}{the number of plausible values}
#'    \item{nPSU}{the number of PSUs used in the calculation}
#'    \item{varm}{the variance estimates under the various plausible values}
#'    \item{coefm}{the values of the coefficients under the various plausible values}
#'    \item{coefmat}{the coefficient matrix (typically produced by the summary of a model)}
#'    \item{weight}{the name of the weight variable}
#'    \item{npv}{the number of plausible values}
#'    \item{njk}{the number of the jackknife replicates used}
#'    \item{varMethod}{always \code{jackknife}}
#'    \item{varEstInputs}{when \code{returnVarEstInputs} is \code{TRUE},
#'                        this element is returned. These are
#'                        used for calculating covariances with
#'                        \code{\link{varEstToCov}}.}
#'
#' @seealso \ifelse{latex}{\code{glm}}{\code{\link[stats]{glm}}}
#' @author Paul Bailey
#'
#' @example man/examples/glm.sdf.R
#' @importFrom stats binomial predict
#' @importFrom glm2 glm2 glm.fit2
#' @importFrom Matrix rankMatrix
#' @export
#' @usage
#' glm.sdf(formula, family = binomial(link = "logit"), data,
#'   weightVar = NULL, relevels = list(),
#'   varMethod=c("jackknife", "Taylor"), jrrIMax = 1,
#'   dropOmittedLevels = TRUE, defaultConditions = TRUE, recode = NULL,
#'   returnNumberOfPSU=FALSE, returnVarEstInputs = FALSE,
#'   omittedLevels = deprecated())
#'
glm.sdf <- function(formula,
                    family = binomial(link = "logit"),
                    data,
                    weightVar = NULL,
                    relevels = list(),
                    varMethod = c("jackknife", "Taylor"),
                    jrrIMax = 1,
                    dropOmittedLevels = TRUE,
                    defaultConditions = TRUE,
                    recode = NULL,
                    returnNumberOfPSU = FALSE,
                    returnVarEstInputs = FALSE,
                    omittedLevels = deprecated()) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "glm.sdf(omittedLevels)", "glm.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")

  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), data)
    class(res) <- "edsurveyGlmList"
    return(res)
  } else {
    return(calc.glm.sdf(
      formula = formula,
      family = family,
      data = data,
      weightVar = weightVar,
      relevels = relevels,
      varMethod = varMethod,
      jrrIMax = jrrIMax,
      dropOmittedLevels = dropOmittedLevels,
      defaultConditions = defaultConditions,
      missingDefaultConditions = missing(defaultConditions),
      recode = recode,
      returnVarEstInputs = returnVarEstInputs,
      returnLm0 = FALSE,
      returnNumberOfPSU = returnNumberOfPSU,
      call = call
    ))
  }
}

#' @rdname glm.sdf
#' @export
logit.sdf <- function(formula,
                      data,
                      weightVar = NULL,
                      relevels = list(),
                      varMethod = c("jackknife", "Taylor"),
                      jrrIMax = 1,
                      dropOmittedLevels = TRUE,
                      defaultConditions = TRUE,
                      recode = NULL,
                      returnNumberOfPSU = FALSE,
                      returnVarEstInputs = FALSE,
                      omittedLevels = deprecated()) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "logit.sdf(omittedLevels)", "logit.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), data)
    class(res) <- "edsurveyGlmList"
    return(res)
  } else {
    return(calc.glm.sdf(
      formula = formula,
      family = binomial(link = "logit"),
      data = data,
      weightVar = weightVar,
      relevels = relevels,
      varMethod = varMethod,
      jrrIMax = jrrIMax,
      dropOmittedLevels = dropOmittedLevels,
      defaultConditions = defaultConditions,
      missingDefaultConditions = missing(defaultConditions),
      recode = recode,
      returnVarEstInputs = returnVarEstInputs,
      returnNumberOfPSU = returnNumberOfPSU,
      returnLm0 = FALSE,
      call = call
    ))
  }
}

#' @rdname glm.sdf
#' @export
probit.sdf <- function(formula,
                       data,
                       weightVar = NULL,
                       relevels = list(),
                       varMethod = c("jackknife", "Taylor"),
                       jrrIMax = 1,
                       dropOmittedLevels = TRUE,
                       defaultConditions = TRUE,
                       recode = NULL,
                       returnNumberOfPSU = FALSE,
                       returnVarEstInputs = FALSE,
                       omittedLevels = deprecated()) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "glm.sdf(omittedLevels)", "glm.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), data)
    class(res) <- "edsurveyGlmList"
    return(res)
  } else {
    return(calc.glm.sdf(
      formula = formula,
      family = binomial(link = "probit"),
      data = data,
      weightVar = weightVar,
      relevels = relevels,
      varMethod = varMethod,
      jrrIMax = jrrIMax,
      dropOmittedLevels = dropOmittedLevels,
      defaultConditions = defaultConditions,
      missingDefaultConditions = missing(defaultConditions),
      recode = recode,
      returnVarEstInputs = returnVarEstInputs,
      returnNumberOfPSU = returnNumberOfPSU,
      returnLm0 = FALSE,
      call = call
    ))
  }
}

calc.glm.sdf <- function(formula,
                         family = binomial(link = "logit"),
                         data,
                         weightVar = NULL,
                         relevels = list(),
                         varMethod = c("jackknife", "Taylor"),
                         jrrIMax = 1,
                         dropOmittedLevels = TRUE,
                         defaultConditions = TRUE,
                         missingDefaultConditions = TRUE,
                         recode = NULL,
                         returnVarEstInputs = FALSE,
                         returnLm0 = FALSE,
                         returnNumberOfPSU = FALSE,
                         call = NULL) {
  if (is.null(call)) {
    call <- match.call()
  }
  varMethod <- substr(tolower(match.arg(varMethod)), 0, 1)
  if (!family$family %in% c("binomial", "quasibinomial")) {
    stop("Only fits binomial models.")
  }

  #############################
  ######### Outline:  #########
  #############################
  # 1) check, format inputs
  # 2) get the data
  # 3) relevel
  # 4a) yvar could have plausible values
  # 4b) xvar could have plauisble values
  # 5) run the main regression
  # 6) form the inputs for variance estimation
  # 7) form output, including final variance estimation

  # 1) check, format inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  # varMethod always jackknife

  # if the weight var is not set, use the default
  wgt <- checkWeightVar(data, weightVar)

  # check if there is an outcome variable and set it to the default if it is missing
  zeroLengthLHS <- attr(terms(formula), "response") == 0
  if (zeroLengthLHS) {
    yvar <- attributes(getAttributes(data, "pvvars"))$default
    formula <- update(formula, new = substitute(yvar ~ ., list(yvar = as.name(yvar))))
  } else {
    yvar <- all.vars(formula[[2]])
  } # End of If/Else: if (zeroLengthLHS)
    xvar <- all.vars(formula[[3]]) #xvar allows to check whether there is a PV on the right-hand side

  # grab the variables needed for the Taylor series method, if that is the variance estimation method being used
  taylorVars <- c()
  psuVar <- getPSUVar(data, weightVar = wgt)
  stratumVar <- getStratumVar(data, weightVar = wgt)
  # give an error if these do not exist.
  tv <- checkTaylorVars(psuVar, stratumVar, wgt, varMethod, returnNumberOfPSU)
  psuVar <- tv$psuVar
  stratumVar <- tv$stratumVar
  varMethod <- tv$varMethod
  returnNumberOfPSU <- tv$returnNumberOfPSU

  if (varMethod == "t") {
    taylorVars <- c(psuVar, stratumVar)
    jrrIMax <- NA
  } else {
    if (all(c(psuVar, stratumVar) %in% colnames(data))) {
      taylorVars <- c(psuVar, stratumVar)
    }
  }
  getDataVarNames <- c(all.vars(formula), wgt, taylorVars)
  tryCatch(getDataVarNames <- unique(c(getDataVarNames, PSUStratumNeeded(returnNumberOfPSU, data))),
    error = function(e) {
      warning(paste0("Stratum and PSU variables are required for this call and are not on the incoming data. Ignoring ", dQuote("returnNumberOfPSU=TRUE"), "."))
      returnNumberOfPSU <<- FALSE
    }
  )
  # 2) get the data
  # This is most of the arguments
  getDataArgs <- list(
    data = data,
    varnames = getDataVarNames,
    returnJKreplicates = (varMethod == "j"),
    drop = FALSE,
    dropOmittedLevels = dropOmittedLevels,
    recode = recode,
    includeNaLabel = TRUE,
    dropUnusedLevels = TRUE,
    addAttributes = TRUE
  )

  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if (!missingDefaultConditions) {
    getDataArgs <- c(getDataArgs, list(defaultConditions = defaultConditions))
  }
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)

  relVars <- colnames(edf)
  relVars <- relVars[!relVars %in% taylorVars]
  incomplete <- !complete.cases(edf[ , relVars])
  if (any(incomplete)) {
    warning("Removing ", sum(incomplete), " rows with NAs from analysis.")
    edf <- edf[!incomplete, ]
  }
  # if doing Taylor series, check Taylor vars too
  if (varMethod == "t") {
    incomplete <- !complete.cases(edf[ , taylorVars])
    if (any(incomplete)) {
      if (any(incomplete[!edf[ , wgt] %in% c(NA, 0)])) {
        warning("Removing ", sum(incomplete), " rows with NA PSU or stratum variables from analysis.")
      }
      edf <- edf[!incomplete, ]
    }
  }
  # remove non-positive (full sample) weights
  if (any(edf[ , wgt] <= 0)) {
    posWeights <- edf[ , wgt] > 0
    warning("Removing ", sum(!posWeights), " rows with nonpositive weight from analysis.")
    edf <- edf[posWeights, ]
  }

  if (varMethod == "t" && sum(is.na(edf[ , c(stratumVar, psuVar)])) != 0) {
    stop("Taylor series variance estimation requested but stratum or PSU variables contian an NA value.")
  }

  # check that there are not factors with only one level and give error message
  lapply(
    names(edf)[names(edf) %in% all.names(formula)],
    function(z) {
      if (is.factor(edf[ , z]) & length(levels(edf[ , z])) < 2) {
        stop(paste0(
          "The covariate ",
          dQuote(z),
          " has fewer than two levels and cannot be included in the regression."
        ))
      }
    }
  )
  # check that there is some data to work with
  if (nrow(edf) <= 0) {
    stop(paste0(
      sQuote("data"), " must have more than 0 rows after a call to ",
      sQuote("getData"), "."
    ))
  }

  # 3) deal with relevels.
  # An argument that allows the user to change the omitted level for a factor variable
  if (length(relevels) > 0) {
    for (i in seq_along(relevels)) {
      vari <- names(relevels)[i]
      if (!vari %in% names(edf)) {
        stop(paste0(
          "In the ", sQuote("relevels"),
          " argument, cannot find the variable named ", sQuote(vari), "."
        ))
      } # End of if statment: ! vari %in% names(edf)
      if (length(relevels[[i]]) != 1) {
        stop(paste0(
          "In the ", sQuote("relevels"),
          " argument, each relevel must have exactly one level."
        ))
      } # End of if statment: length(relevels[[i]]) != 1
      # check that the level exists
      lvls <- levels(edf[ , vari])
      if (inherits(edf[ , vari], "lfactor")) {
        # for a factor it can be either a level or a label
        lvls <- c(lvls, labels(edf[ , vari]))
      } # End of if statment: inherits(edf[ ,vari], "lfactor")
      if (!relevels[[i]] %in% lvls) {
        stop(paste0(
          "In the ", sQuote("relevels"),
          " argument, for the variable ", dQuote(vari), ", the level ",
          dQuote(relevels[[i]]), " not found. Found levels are ",
          pasteItems(dQuote(lvls)), "."
        ))
      } # End of if statment !relevels[[i]] %in% lvls
      edf[ , vari] <- relevel(edf[ , vari], ref = relevels[[i]])
    } # end for(i in seq_along(relevels))
  } # end if(length(relevels) > 0)

  # 4a) yvar and plausible values
  pvy <- hasPlausibleValue(yvar, data) # pvy is the plausible values of the y variable
  yvars <- yvar
  linkingError <- detectLinkingError(data, yvars)
  if(linkingError){
    if(varMethod == "t") {
      stop("Taylor series variance estimation not supported with NAEP linking error.")
    }
    if (jrrIMax != 1) {
      warning("The linking error variance estimator only supports ", dQuote("jrrIMax=1"), ". Resetting to 1.")
      jrrIMax <- 1
    }
  } 

  lyv <- length(yvars)
  if (any(pvy)) {
    if (linkingError) {
      pp <- getPlausibleValue(yvars[max(pvy)], data)
      suffix <- ifelse(grepl("_imp_", pp, fixed = TRUE), "_imp_", "_est_")
      suffix <- ifelse(grepl("_samp_", pp, fixed = TRUE), "_samp_", suffix)
      yvars <- paste0("outcome", suffix, seq_along(pp))
    } else {
      yvars <- paste0("outcome", seq_along(getPlausibleValue(yvars[max(pvy)], data)))
    }
  } else {
    # if not, make sure that this variable is numeric
    edf[ , "yvar"] <- as.numeric(eval(formula[[2]], edf))
    formula <- update(formula, new = substitute(yvar ~ ., list(yvar = as.name(yvar))))
    yvars <- "yvar"
  } # End of if statment: any(pvy)

  # 4b) cvar and plausible values
  pvx <- hasPlausibleValue(xvar, data) # pvx is the indicator of pvs on the right hand side
  xvars <- xvar
  linkingError <- detectLinkingError(data, xvars)
  if(linkingError){
    if(varMethod == "t") {
      stop("Taylor series variance estimation not supported with NAEP linking error.")
    }
    if (jrrIMax != 1) {
      warning("The linking error variance estimator only supports ", dQuote("jrrIMax=1"), ". Resetting to 1.")
      jrrIMax <- 1
    }
  } # end if linkingError

  lxv <- length(xvars)
  if(any(pvx)) {
    pvRHS <- xvar[pvx] # the name of the scale/subscale
    pvxs <- getPlausibleValue(pvRHS, data) # the corresponding PV names
    if (linkingError) {
      pp <- getPlausibleValue(xvars[max(pvx)], data)
      suffix <- ifelse(grepl("_imp_", pp, fixed = TRUE), "_imp_", "_est_")
      suffix <- ifelse(grepl("_samp_", pp, fixed = TRUE), "_samp_", suffix)
      xvars <- paste0("outcome", suffix, seq_along(pp))
    } else { # end if linkingError
      xvarPvs <- paste0("predictor", seq_along(pvxs))  # new names for the PVs
    } # end if linkingError
    xvarPv0 <- xvarPvs[1]
    
    for(i in seq_along(pvRHS)) { # number of scale/subscale on the right hand side
      # first, by PV, rename the first PVs to be e.g. composite
      edf[ , pvRHS[i]] <- edf[ , getPlausibleValue(pvRHS[i], data)[1]]
    }

    # then set xvarPvs[i] (e.g. predictor1) to be the evaluation at this PV point
    for(xvi in seq_along(xvarPvs)) { # number of PVs on the right hand side
      edf[ , xvarPvs[xvi]] <- edf[ ,pvxs[xvi]]            
    }

    #first have the predictors then the rest of the independent variables, adjusted for multiple PVs on the RHS
    xvars <- c(xvarPvs, xvars[which(!xvars %in% pvRHS)])
    # finally, correctly set xvar0
    edf$xvarPv0 <- edf[ , xvarPv0]
    # set PV name as the corresposing predictor to have the correct X
    for(pvi in seq_along(pvRHS)){
    predictorPV <- xvarPvs[pvxs == getPlausibleValue(pvRHS[i], data)[1]]
    edf[ , pvRHS[pvi]] <- edf[,predictorPV]
  } # end if(any(pvx)), no else
  }
  # 5) run the main regression
  # run a regression, starting with the first PV or maybe the only outcome variable
  yvar0 <- yvars[1]

  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  if (family$family %in% c("binomial", "quasibinomial")) { # if using binomial set y to 1 only if it has the highest value
    if (any(pvy)) {
      for (i in seq_along(yvars)) {
        # PV, so we have not evaluated the I() yet (if any)
        for (yvi in seq_along(pvy)) {
          if (pvy[yvi]) {
            edf[ , yvar[yvi]] <- edf[ , getPlausibleValue(yvar[yvi], data)[i]]
          }
        }
        edf[ , yvars[i]] <- as.numeric(eval(formula[[2]], edf))
      }
      oneDef <- max(edf[ , yvars], na.rm = TRUE)
      for (i in yvars) {
        edf[ , i] <- ifelse(edf[ , i] %in% oneDef, 1, 0)
      }
      edf$yvar0 <- edf[ , yvar0]
    } else {
      # for non-PV, I() has been evaluated
      oneDef <- max(edf[ , yvars], na.rm = TRUE)
      edf$yvar0 <- ifelse(edf$yvar %in% oneDef, 1, 0)
    }
  }
  # set up formula for initial regression with yvar0 predicted by all other vars
  frm <- update(formula, yvar0 ~ .)
  edf$w <- edf[ , wgt]

 # If there is any PV on RHS update variable orders of the formula to allign with getEst results
  if(any(pvx)){
     frm <- formula(paste("yvar0 ~", paste0(c(xvar[!pvx], pvRHS), collapse = "+")))
   } # end if(any(pvx)), no else

  # get an approximate fit without weights. Starting with weights can lead to
  # convergence problems.
  suppressWarnings(lm00 <- glm2(frm, data = edf, family = family))
  edf$c2 <- predict(lm00, type = "response")
  # get a final fit, with weights. Starting at c2
  suppressWarnings(lm0 <- glm2(frm, data = edf, weights = w, family = family, mustart = c2, epsilon = 1e-14))
  # get final prediction
  edf$c2 <- predict(lm0, type = "response")
  if (returnLm0) {
    return(lm0)
  }
  # this grabs the list of weight variable names
  wgtl <- getAttributes(data, "weights")[[wgt]]
  varEstInputs <- list()
  # note we have not made the B matrix
  madeB <- FALSE
  # there is now a very long set of conditionals
  stat_reg_glm <- function(fam = FALSE) {
    ref <- function(pv, wg, X) {
      if (family$family %in% c("binomial", "quasibinomial") & (fam)) {
        # do this for each yvar, but not for each jk weight (in parent function)
        edf$yvar0 <- ifelse(pv %in% oneDef, 1, 0)
      }
      edf$w <- wg
      suppressWarnings(lmi <- glm2(frm, data = edf, weights = w, family = family, mustart = c2, epsilon = 1e-14))
      return(coef(lmi))
    }
  }
  # 6) form the inputs for variance estimation
  if (any(pvy)) { # the y variable is a plausible value
    # this if condition estimates both  Taylor series method and jackknife method
    # for equations, see the statistics vignette
    if (varMethod == "t") {
      jrrIMax <- length(yvars)
    } else {
      jrrIMax <- min(jrrIMax, length(yvars))
    }

    # initialize: varm is the variance matrix by coefficient and PV (for V_jrr)
    varM <- list()
    varm <- matrix(NA, nrow = jrrIMax, ncol = length(coef(lm0)))

    if (varMethod == "j") {
      if (linkingError) {
        repWeights <- paste0(wgtl$jkbase, wgtl$jksuffixes)
        # get the mean estimate
        est <- getLinkingEst(
          data = edf,
          pvEst = yvars[grep("_est", yvars)],
          stat = stat_reg_glm(fam = TRUE),
          wgt = wgt
        )
        # coefficients matrix, r-squared vector
        coefm <- est$coef
        coef <- apply(coefm, 2, mean)
        coefm0 <- t(t(coefm) - coef)
        # get imputation variance
        impVar <- getLinkingImpVar(
          data = edf,
          pvImp = yvars[grep("_imp", yvars)],
          ramCols = ncol(getRAM()),
          stat = stat_reg_glm(fam = TRUE),
          wgt = wgt,
          T0 = est$est,
          T0Centered = FALSE
        )
        # get sampling variance
        sampVar <- getLinkingSampVar(edf,
          pvSamp = yvars[grep("_samp", yvars)],
          stat = stat_reg_glm(fam = TRUE),
          rwgt = repWeights,
          T0 = est$est,
          T0Centered = FALSE
        )
        varM[[1]] <- sampVar$Bi
        varEstInputs[["JK"]] <- sampVar$veiJK
        varEstInputs[["PV"]] <- impVar$veiImp
        # drop r.squared term
        varm[1, ] <- sampVar$V
        M <- length(yvars[grep("_est", yvars)])
        Vimp <- impVar$V
      } else { # end if (linkingError)
        # coefficients by PV (getEst in lm.sdf)
      if(any(pvx)){
          res <- getEst(
            data = edf,
            pvEst = yvars,
            stat = stat_reg_glm(fam = TRUE),
            wgt = wgt,
            pvx = pvx,
            xvarPvs = xvarPvs,
            xvar = xvar
          )
        } else {
        res <- getEst(
          data = edf,
          pvEst = yvars,
          stat = stat_reg_glm(fam = TRUE),
          wgt = wgt
        )
        } # end if (any(pvx))
        coefm <- res$coef
        varEstInputs[["JK"]] <- data.frame()
        jkSumMult <- getAttributes(data, "jkSumMultiplier")
        for (pvi in 1:jrrIMax) { # for each PV (up to jrrIMax)
          res <- getVarEstJK(
            stat = stat_reg_glm(fam = FALSE),
            yvar = edf[ , yvars[pvi]],
            wgtM = edf[ , paste0(wgtl$jkbase, wgtl$jksuffixes)],
            co0 = coefm[pvi, ],
            jkSumMult = jkSumMult,
            pvName = pvi,
            pvx = pvx
          )
          varM[[pvi]] <- res$Bi
          varEstInputs[["JK"]] <- rbind(varEstInputs[["JK"]], res$veiJK)
          varm[pvi, ] <- res$VsampInp
        } # end for(pvi in 1:jrrIMax)
        # number of PVs
        M <- length(yvars)
        # imputaiton variance / variance due to uncertaintly about PVs
        coef <- apply(coefm, 2, mean)
        coefm0 <- t(t(coefm) - coef)
        coefmPVByRow <- lapply(1:ncol(coefm0), function(coli) {
          data.frame(
            stringsAsFactors = FALSE,
            PV = 1:nrow(coefm0),
            variable = rep(names(coef(lm0))[coli], nrow(coefm0)),
            value = coefm0[ , coli]
          )
        })
        coefmPV <- do.call(rbind, coefmPVByRow)
        varEstInputs[["PV"]] <- coefmPV
        Vimp <- (M + 1) / M * apply(coefm, 2, var)
      } # end else for if(linkingError)
    } else { # End of if statment: varMethod == "j"
      # Taylor series variance esimation, no Plausible Values
      dofNum <- matrix(0, nrow = length(coef(lm0)), ncol = length(yvars))
      dofDenom <- matrix(0, nrow = length(coef(lm0)), ncol = length(yvars))
      # variances are calculated iteratively for all each y variable
      if(any(pvx)){
          est <- getEst(
          data = edf,
          pvEst = yvars,
          stat = stat_reg_glm(fam = TRUE),
          wgt = wgt,
          pvx = pvx,
          xvarPvs = xvarPvs,
          xvar = xvar
          )
        } else {
        est <- getEst(
          data = edf,
          pvEst = yvars,
          stat = stat_reg_glm(fam = TRUE),
          wgt = wgt
        )
        } # end if (any(pvx))
      coefm <- est$coef
      for (mm in seq_along(yvars)) {
        edf$yvar0 <- as.numeric(edf[ , yvars[mm]])
        y <- edf[ , yvars[mm]]
        suppressWarnings(lmi <- glm2(frm, data = edf, weights = w, family = family, mustart = c2, epsilon = 1e-14))
        coef <- b <- co0 <- coef(lmi)
        D <- vcov(lmi)
        eta <- predict(lmi, type = "link")
        pred <- predict(lmi, type = "response")
        # precision weights
        # in the notation of McCullagh and Nelder, this is d(mu)/d(eta)
        # evaluated at the (latent) predicted values (eta)
        mu.eta <- family$mu.eta(eta)
        # this is the partial of the likelihood at the unit level
        X <- sparse.model.matrix(frm, edf)
        uhij <- (y - pred) / (pred * (1 - pred)) * as.matrix(X) * mu.eta

        # vals by stratum (singleton PSUs removed in function)
        res <- getVarTaylor(uhij, edf, D, wgt, psuVar, stratumVar)
        vc <- D %*% res$vv %*% D

        # collect
        dofNum[ , mm] <- res$nums
        dofDenom[ , mm] <- res$nums2
        varM[[mm]] <- as.matrix(vc)
        varm[mm, ] <- as.numeric(diag(vc))
      }
      # number of PVs
      M <- length(yvars)
      # imputaiton variance / variance due to uncertaintly about PVs
      coef <- apply(coefm, 2, mean)
      coefm0 <- t(t(coefm) - coef)
      coefmPVByRow <- lapply(1:ncol(coefm0), function(coli) {
        data.frame(
          stringsAsFactors = FALSE,
          PV = 1:nrow(coefm0),
          variable = rep(names(coef(lm0))[coli], nrow(coefm0)),
          value = coefm0[ , coli]
        )
      })
      coefmPV <- do.call(rbind, coefmPVByRow)
      varEstInputs[["PV"]] <- coefmPV
      Vimp <- (M + 1) / M * apply(coefm, 2, var)
    } # End of if/else statment: varMethod == "j"
    # calculate van Buuren B
    B <- (1 / (M - 1)) * Reduce(
      "+", # add up the matrix results of the lapply
      lapply(1:nrow(coefm), function(q) {
        # within each PV set, calculate the outer product
        # (2.19 of Van Buuren)
        outer(coefm0[q, ], coefm0[q, ])
      })
    )
    madeB <- TRUE

    # 2.18 in var Buuren
    Ubar <- (1 / length(varM)) * Reduce("+", varM)
    # variance due to sampling
    Vjrr <- apply(varm[1:jrrIMax, , drop = FALSE], 2, mean)
  } else { # end if(any(pvy))
    # the y variable is not a plausible value
    # this section handles jackknife and Taylor series estimation
    if (varMethod == "j") {
      jkSumMult <- getAttributes(data, "jkSumMultiplier")
      coef <- coef(lm0)
      res <- getVarEstJK(
        stat = stat_reg_glm(fam = FALSE),
        yvar = edf[ , yvars[1]],
        wgtM = edf[ , paste0(wgtl$jkbase, wgtl$jksuffixes)],
        co0 = coef,
        jkSumMult = jkSumMult,
        pvName = 1,
        pvx = pvx
      )
      Ubar <- res$Bi
      Vjrr <- res$VsampInp
      varEstInputs[["JK"]] <- res$veiJK
    } else { # end if(varMethod == "j")
      # Taylor series variance esimation, no PVs
      X <- sparse.model.matrix(frm, edf)
      y <- edf$yvar0
      coef <- b <- co0 <- coef(lm0)
      D <- vcov(lm0)
      eta <- predict(lm0, type = "link")
      pred <- predict(lm0, type = "response")
      # precision weights
      # in the notation of McCullagh and Nelder, this is d(mu)/d(eta)
      # evaluated at the (latent) predicted values (eta)
      mu.eta <- family$mu.eta(eta)
      # this is the partial of the likelihood at the unit level
      uhij <- (y - pred) / (pred * (1 - pred)) * as.matrix(X) * mu.eta

      # vals by stratum (singleton PSUs removed in function)
      res <- getVarTaylor(uhij, edf, D, wgt, psuVar, stratumVar)
      vc <- D %*% res$vv %*% D

      # collect
      dofNum <- res$nums
      dofDenom <- res$nums2
      Vjrr <- as.numeric(diag(vc))
      # this is the VC matrix, store it for vcov to recover
      Ubar <- as.matrix(vc)
    } # end else for if(varMethod == "j")

    # not pvy common
    M <- 1 # only on replicate when there are no PVs
    Vimp <- 0 # no imputation variance when there are no PVs
    varm <- NULL # no variance by PV
    coefm <- NULL # no coefficients matrix by PV
    B <- 0 * Ubar # no PVs
  } # end else for if(any(pvy))

  # 7) form output, including final variance estimation
  V <- Vjrr + Vimp
  names(coef) <- names(coef(lm0))
  se <- sqrt(V)
  names(se) <- names(coef)

  # get fitted and resid based on overall coef
  X <- model.matrix(frm, edf)
  fittedLatent <- as.vector(X %*% coef)
  fitted1 <- family$linkinv(fittedLatent)
  if (linkingError) {
    ye <- grep("_est_", yvars)
    Y <- vapply(ye, function(yi) {
      as.vector(edf[ , yvars[yi]])
    }, FUN.VALUE = numeric(nrow(edf)))
    resid1 <- Y - fitted1
    colnames(resid1) <- yvars[ye]
  } else {
    Y <- vapply(seq_along(yvars), function(yi) {
      as.vector(edf[ , yvars[yi]])
    }, FUN.VALUE = numeric(nrow(edf)))
    resid1 <- Y - fitted1
    colnames(resid1) <- yvars
  }

  # residual df calculation
  nobs <- nrow(edf)
  n.ok <- nobs - sum(edf$origwt == 0)
  nvars <- ncol(X)
  rank <- rankMatrix(X)
  resdf <- n.ok - rank
  df.r <- resdf

  # get residual based on coef by PV
  if (!is.null(coefm)) {
    if (!is.matrix(coefm)) {
      coefm <- t(as.matrix(coefm))
    }
    fitted2 <- family$linkinv(as.matrix(X %*% t(coefm)))
    resid2 <- Y - fitted2
  }

  coefmat <- data.frame(
    coef = coef,
    se = se,
    t = coef / se
  )
  if (varMethod == "t") {
    m <- length(wgtl$jksuffixes)
    if (inherits(dofNum, "matrix")) {
      coefmat$dof <- apply((3.16 - 2.77 / sqrt(m)) * dofNum^2 / dofDenom, 1, mean)
    } else {
      coefmat$dof <- (3.16 - 2.77 / sqrt(m)) * dofNum^2 / dofDenom
    }
  } else {
    coefmat$dof <- vapply(names(coef), function(cn) {
      DoFCorrection(varEstA = varEstInputs, varA = cn, method = "JR")
    }, FUN.VALUE=numeric(1))
  }
  pti <- pt(coefmat$t, df = coefmat$dof)
  coefmat[ , "Pr(>|t|)"] <- 2 * pmin(pti, 1 - pti)
  njk <- length(wgtl$jksuffixes)
  if (varMethod == "t") {
    njk <- NA
  }
  varmeth <- ifelse(varMethod == "t", "Taylor series", "jackknife")
  res <- list(
    call = call, formula = formula, family = family, coef = coef, se = se, Vimp = Vimp,
    Vjrr = Vjrr, M = M, varm = varm, coefm = coefm,
    coefmat = coefmat, weight = wgt,
    npv = length(yvars), jrrIMax = min(jrrIMax, length(yvars)),
    njk = njk, varMethod = varmeth,
    residuals = resid1, fitted.values = fitted1, fittedLatent = fittedLatent, residual.df = resdf
  )
  if (!is.null(coefm)) {
    res <- c(res, list(PV.residuals = resid2, PV.fitted.values = fitted2))
  }

  if (returnVarEstInputs) {
    res <- c(res, list(varEstInputs = varEstInputs))
    if (varMethod == "t") {
      warning(paste0(
        "Taylor series method not supported with the ",
        sQuote("varEstInputs"), " argument set to ", dQuote("TRUE"), "."
      ))
    }
  }

  if (madeB) {
    # equation 2.29 in van Buuren, pp 42
    tryCatch(
      rbar <- (1 + 1 / M) * (1 / nrow(Ubar)) * sum(diag(B %*% solve(Ubar))),
      error = function(e) {
        rbar <<- (1 + 1 / M) * (1 / nrow(Ubar)) * sum(diag(B %*% MASS::ginv(Ubar)))
        frm <- Formula(frm)
        if (terms(frm, lhs = 0, rhs = NULL) == ~1) {
          warning("A variance estimate was replaced with NA because there was no variance across strata.", call. = FALSE)
        }
      }
    )
    Ttilde <- (1 + rbar) * Ubar
    res <- c(res, list(B = B, U = Ubar, rbar = rbar, Ttilde = Ttilde))
  } else {
    res <- c(res, list(B = B, U = Ubar))
  }
  if (returnNumberOfPSU) {
    stratumVar <- getAttributes(data, "stratumVar")
    psuVar <- getAttributes(data, "psuVar")
    if ("JK1" %in% stratumVar) {
      res <- c(res, list(nPSU = nrow(edf)))
    } else if (all(c(stratumVar, psuVar) %in% colnames(edf))) {
      if (sum(is.na(edf[ , c(stratumVar, psuVar)])) == 0) {
        res <- c(res, list(nPSU = nrow(unique(edf[ , c(stratumVar, psuVar)]))))
      } else {
        warning("Cannot return number of PSUs because the stratum or PSU variables contain NA values.")
      }
    }
  }
  # add waldDenomBaseDof if relevant
  if (!is.null(stratumVar) && !is.null(psuVar)) {
    if (all(c(stratumVar, psuVar) %in% colnames(edf))) {
      if (sum(is.na(edf[ , c(stratumVar, psuVar)])) == 0) {
        res <- c(res, list(waldDenomBaseDof = waldDof(edf, stratumVar, psuVar)))
      }
    }
    if ("JK1" %in% getStratumVar(data)) {
      res <- c(res, list(waldDenomBaseDof = "JK1"))
    }
  }
  res <- c(res, list(n0 = nrow2.edsurvey.data.frame(data), nUsed = nrow(edf)))
  if (inherits(data, "edsurvey.data.frame")) {
    res <- c(res, list(data = data))
  } else {
    res <- c(res, list(lm0 = lm0))
  }

  class(res) <- "edsurveyGlm"
  return(res)
}

#' @method print edsurveyGlm
#' @export
print.edsurveyGlm <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  print(coef(x), ...)
}

#' @method print edsurveyGlmList
#' @export
print.edsurveyGlmList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("glm", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(coef(x[[i]]), ...)
  }
}

#' @method summary edsurveyGlm
#' @export
summary.edsurveyGlm <- function(object, ...) {
  class(object) <- "summary.edsurveyGlm"
  object
}

#' @method summary edsurveyGlmList
#' @export
summary.edsurveyGlmList <- function(object, ...) {
  class(object) <- "summary.edsurveyGlmList"
  for (i in seq_along(object)) {
    class(object[[i]]) <- "summary.edsurveyGlm"
  }
  object
}

#' @method print summary.edsurveyGlm
#' @importFrom stats printCoefmat
#' @export
print.summary.edsurveyGlm <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  cat(paste0("\nFormula: ", paste(deparse(x$formula), collapse = ""), "\n"))
  cat(paste0("Family: ", x$family$family, " (", x$family$link, ")\n\n"))
  if (x$npv != 1) {
    cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ", x$varMethod, "\n"))
  if (!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  cat(paste0("full data n: ", x$n0, "\n"))
  if (!is.null(x$nPSU)) {
    cat(paste0("n PSU: ", x$nPSU, "\n"))
  }
  cat(paste0("n used: ", x$nUsed, "\n\n"))
  cat(paste0("Coefficients:\n"))
  printCoefmat(x$coefmat, P.values = TRUE, has.Pvalue = TRUE, ...)
}

#' @method print summary.edsurveyGlmList
#' @export
print.summary.edsurveyGlmList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("glm", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(x[[i]], ...)
  }
}

#' @method coef edsurveyGlm
#' @export
coef.edsurveyGlm <- function(object, ...) {
  object$coef
}

#' @method coef edsurveyGlmList
#' @export
coef.edsurveyGlmList <- function(object, ...) {
  do.call(cbind, lapply(object, function(li) {
    li$coef
  }))
}

#' @method vcov edsurveyGlm
#' @export
vcov.edsurveyGlm <- function(object, ...) {
  vcov.edsurveyLm(object, ...)
}

#' @title Odds Ratios for edsurveyGlm Models
#' @description Converts coefficients from \code{edsurveyGlm} logit regression model to odds ratios.
#' @param model an \code{edsurveyGlm} model
#' @param alpha the alpha level for the confidence level
#'
#' @return
#' An \code{oddsRatio.edsurveyGlm} object with the following elements:
#'    \item{OR}{odds ratio coefficient estimates}
#'    \item{2.5\%}{lower bound 95\% confidence interval}
#'    \item{97.5\%}{upper bound 95\% confidence interval}
#'
#' @export
oddsRatio <- function(model, alpha = 0.05) {
  if (!inherits(model, "edsurveyGlm")) stop("x must be of class 'edsurveyGlm'.")
  if (model$family$family != "binomial" & model$family$family != "quasibinomial") {
    stop("Model must be binomial.")
  }
  if (model$family$link != "logit") {
    stop("Model must be logit.")
  }
  OR <- exp(coef(model))
  varDiag <- diag(vcov(model))
  CIlo <- exp(coef(model) - (qt(1 - alpha / 2, df = model$coefmat$dof) * sqrt(varDiag)))
  CIup <- exp(coef(model) + (qt(1 - alpha / 2, df = model$coefmat$dof) * sqrt(varDiag)))
  r <- cbind(OR, CIlo, CIup)
  pctRound <- 1
  pct <- round(100 * c(alpha / 2, 1 - alpha / 2), pctRound)
  while (pct[1] == 0) {
    pctRound <- pctRound + 1
    pct <- round(100 * c(alpha / 2, 1 - alpha / 2), pctRound)
  }
  colnames(r) <- c("OR", paste0(pct, "%"))
  r <- as.data.frame(r)
  class(r) <- c("oddsRatio.edsurveyGlm", "data.frame")
  r
}

# @export
setMethod(
  "coef",
  c(object = "edsurveyGlm"),
  coef.edsurveyGlm
)

# @export
setMethod(
  "coef",
  c(object = "edsurveyGlmList"),
  coef.edsurveyGlmList
)

# @export
setMethod(
  "vcov",
  c(object = "edsurveyGlm"),
  vcov.edsurveyGlm
)
