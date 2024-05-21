#' @title EdSurvey Quantile Regression Models
#' @aliases rq
#' @description Fits a quantile regression model that uses weights and variance estimates appropriate for the data.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   quantile regression model. See \ifelse{latex}{\code{rq} in the \code{quantreg} package}{\code{\link[quantreg]{rq}}}.
#'                   If \emph{y} is left blank, the default subject scale or subscale variable
#'                   will be used. (You can find the default using
#'                   \code{\link{showPlausibleValues}}.)
#'                   If \emph{y} is a variable for a subject scale or subscale (one of the
#'                   names shown by \code{\link{showPlausibleValues}}),
#'                   then that subject scale or subscale is used.
#' @param data       an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'                   or an \code{edsurvey.data.frame.list}
#' @param tau        the quantile to be estimated. The value could be set between 0 and 1 with a default of 0.5.
#' @param weightVar  a character indicating the weight variable to use.
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, it  uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param jrrIMax    when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}}
#'                   term can be estimated with any number of plausible values, and values larger than the number of
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param relevels   a list. Used to change the contrasts from the
#'                   default treatment contrasts to the treatment contrasts with a chosen omitted
#'                   group (the reference group). The name of each element should be the variable name, and the value
#'                   should be the group to be omitted (the reference group).
#' @param dropOmittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      those levels of all factor variables that are specified
#'                      in an \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses
#'                          the default conditions stored in an \code{edsurvey.data.frame}
#'                          to subset the data. Use \code{print} on an
#'                          \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode=}\code{list(}\code{var1} \code{=} \code{list(}\code{from=}
#'                  \code{c("a",} \code{"b",} \code{"c"),} \code{to=} \code{"d"))}.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of
#'                          primary sampling units (PSUs)
#'
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}
#'
#' @param ... additional parameters passed from \ifelse{latex}{\code{rq}}{\code{\link[quantreg]{rq}}}
#'
#' @details
#'
#' The function computes an estimate on the \code{tau}-th conditional quantile function of the response,
#' given the covariates, as specified by the formula argument. Like \code{lm.sdf()}, the
#' function presumes a linear specification for the quantile regression model (i.e., that the
#' formula defines a model that is linear in parameters). Unlike \code{lm.sdf()}, the jackknife is the
#' only applicable variance estimation method used by the function.
#'
#' For further details on quantile regression models and how they are implemented in R, see Koenker
#' and Bassett (1978), Koenker (2005), and the vignette from the \code{quantreg} package---
#' accessible by \code{vignette("rq",package="quantreg")}---on which this function is
#' built.
#'
#' For further details on how left-hand side variables, survey sampling weights, and estimated
#' variances are correctly handled, see \code{\link{lm.sdf}} or the vignette titled
#' \emph{\href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey}}.
#'
#'
#'
#' @references
#' Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys.
#' \emph{International Statistical Review}, \emph{51}(3), 279--292.
#'
#' Johnson, E. G., & Rust, K. F. (1992). Population inferences and variance estimation for NAEP
#' data. \emph{Journal of Education Statistics}, \emph{17}(2), 175--190.
#'
#' Koenker, R. W., & Bassett, G. W. (1978). Regression quantiles, \emph{Econometrica, 46,} 33--50.
#'
#' Koenker, R. W. (2005). \emph{Quantile regression}. Cambridge, UK: Cambridge University Press.
#'
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
#'
#'
#' @return
#' An \code{edsurvey.rq} with the following elements:
#'    \item{call}{the function call}
#'    \item{formula}{the formula used to fit the model}
#'    \item{tau}{the quantile to be estimated}
#'    \item{coef}{the estimates of the coefficients}
#'    \item{se}{the standard error estimates of the coefficients}
#'    \item{Vimp}{the estimated variance from uncertainty in the scores (plausible value variables)}
#'    \item{Vjrr}{the estimated variance from sampling}
#'    \item{M}{the number of plausible values}
#'    \item{varm}{the variance estimates under the various plausible values}
#'    \item{coefm}{the values of the coefficients under the various plausible values}
#'    \item{coefmat}{the coefficient matrix (typically produced by the summary of a model)}
#'    \item{weight}{the name of the weight variable}
#'    \item{npv}{the number of plausible values}
#'    \item{njk}{the number of the jackknife replicates used; set to \code{NA} when Taylor series variance
#'    estimates are used}
#'    \item{rho}{the mean value of the objective function across the plausible values}
#'
#' @seealso \ifelse{latex}{\code{rq}}{\code{\link[quantreg]{rq}}}
#' @author Trang Nguyen, Paul Bailey, and Yuqi Liao
#'
#' @example \man\examples\rq.sdf.R
#' @importFrom Matrix sparse.model.matrix rankMatrix
#' @importFrom quantreg rq rq.wfit
#' @importFrom stats aggregate pt relevel model.matrix as.formula complete.cases
#' @importFrom Formula Formula
#' @importFrom MASS ginv
#' @export
rq.sdf <- function(formula,
                   data,
                   tau = 0.5,
                   weightVar = NULL,
                   relevels = list(),
                   jrrIMax = 1,
                   dropOmittedLevels = TRUE,
                   defaultConditions = TRUE,
                   recode = NULL,
                   returnNumberOfPSU = FALSE,
                   omittedLevels = deprecated(),
                   ...) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "rq.sdf(omittedLevels)", "rq.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")

  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), data)
    class(res) <- "edsurveyLmList"
    return(res)
  } else {
    return(calc.rq.sdf(
      formula = formula,
      data = data,
      weightVar = weightVar,
      relevels = relevels,
      jrrIMax = jrrIMax,
      omittedLevels = dropOmittedLevels,
      defaultConditions = defaultConditions,
      missingDefaultConditions = missing(defaultConditions),
      recode = recode,
      returnLm0 = FALSE,
      call = call,
      returnNumberOfPSU = returnNumberOfPSU,
      tau = tau,
      ...
    ))
  }
}

calc.rq.sdf <- function(formula,
                        data,
                        weightVar = NULL,
                        relevels = list(),
                        varMethod = "jackknife",
                        jrrIMax = 1,
                        omittedLevels = TRUE,
                        defaultConditions = TRUE,
                        missingDefaultConditions = TRUE,
                        recode = NULL,
                        returnVarEstInputs = FALSE,
                        returnLm0 = FALSE,
                        call = NULL,
                        returnNumberOfPSU = FALSE,
                        tau,
                        ...) {
  if (is.null(call)) {
    call <- match.call()
  }
  #######################
  ######## Outline ######
  #######################
  # 1) check, format inputs
  # 2) get the data
  # 3) deal with relevels.
  # 4) deal with yvar having plausible values
  # 5) run the main regression
  # 6) run the regressions, form the inputs for variance estimation
  # 7) form output, including final variance estimation

  # 1) check, format inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data # short for survey data.frame

  # get varMethod, turn into first character as lower case
  varMethod <- substr(tolower(match.arg(varMethod)), 0, 1)

  # if the weight var is not set, use the default
  wgt <- checkWeightVar(data, weightVar)

  psuVar <- getPSUVar(data, weightVar = wgt)
  stratumVar <- getStratumVar(data, weightVar = wgt)
  tv <- checkTaylorVars(psuVar, stratumVar, wgt, varMethod, returnNumberOfPSU)
  psuVar <- tv$psuVar
  stratumVar <- tv$stratumVar
  varMethod <- tv$varMethod

  # check if there is an outcome variable and set it to the default if it is missing
  zeroLengthLHS <- attr(terms(formula), "response") == 0
  if (zeroLengthLHS) {
    yvar <- attributes(getAttributes(sdf, "pvvars"))$default
    formula <- update(formula, new = substitute(yvar ~ ., list(yvar = as.name(yvar))))
  } else {
    yvar <- all.vars(formula[[2]])
  } # End of If/Else: if (zeroLengthLHS)

  # grab the variables needed for the Taylor series method, if that is the variance estimation method being used
  taylorVars <- c()
  getDataVarNames <- c(all.vars(formula), wgt, taylorVars)
  tryCatch(getDataVarNames <- unique(c(getDataVarNames, PSUStratumNeeded(returnNumberOfPSU, data))),
    error = function(e) {
      warning(paste0("Stratum and PSU variables are required for this call and are not on the incoming data. Ignoring ", dQuote("returnNumberOfPSU=TRUE"), "."))
      returnNumberOfPSU <<- FALSE
    }
  )

  # check the length and the value of tau
  if (length(tau) != 1) {
    stop("Quantile Regression requires a single value of tau (quantile to be estimated).")
  }
  if (tau <= 0 | tau >= 1) {
    stop("Tau (quantile to be estimated) should be between 0 and 1.")
  }

  # 2) get the data
  # This is most of the arguments
  getDataArgs <- list(
    data = sdf,
    varnames = getDataVarNames,
    returnJKreplicates = (varMethod == "j"),
    drop = FALSE,
    omittedLevels = omittedLevels,
    recode = recode,
    includeNaLabel = TRUE,
    dropUnusedLevels = TRUE
  )

  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if (!missingDefaultConditions) {
    getDataArgs <- c(getDataArgs, list(defaultConditions = defaultConditions))
  }
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
  if (any(!complete.cases(edf))) {
    warning("Removing rows with NAs from analysis.")
    edf <- edf[complete.cases(edf), ]
  }

  # check that there are not factors with only one level and give more informative error.
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
          " argument, can't find the variable named ", sQuote(vari), "."
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
          " argument, for the variable ", sQuote(vari), ", the level ",
          sQuote(relevels[[i]]), " not found. Found levels are ",
          paste(sQuote(lvls), collapse = ", "), "."
        ))
      } # End of if statment !relevels[[i]] %in% lvls
      edf[ , vari] <- relevel(edf[ , vari], ref = relevels[[i]])
    } # end for(i in seq_along(relevels))
  } # end if(length(relevels) > 0)

  # 4) deal with yvar having plausible values
  pvy <- hasPlausibleValue(yvar, sdf) # pvy is the plausible values of the y variable
  yvars <- yvar
  linkingError <- detectLinkingError(data, yvars)
  lyv <- length(yvars)
  if (any(pvy)) {
    pvs <- getPlausibleValue(yvars[max(pvy)], data)
    yvars <- paste0(
      "outcome",
      seq_along(pvs),
      ifelse(grepl("_imp_", pvs), "_imp",
        ifelse(grepl("_samp_", pvs), "_samp", "_est")
      )
    )
  } else {
    # no PVs, just make sure that this variable is numeric
    edf[ , "yvar"] <- as.numeric(eval(formula[[2]], edf))
    formula <- update(formula, new = substitute(yvar ~ ., list(yvar = as.name(yvar))))
    yvars <- "yvar"
  } # End of if statment: any(pvy)

  yvar0 <- yvars[1]
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  if (any(pvy)) {
    for (i in seq_along(yvars)) {
      # PV, so we have not evaluated the I() yet (if any)
      # first, by PV, rename the ith PVs to be e.g. composite
      for (yvi in seq_along(pvy)) {
        if (pvy[yvi]) {
          edf[ , yvar[yvi]] <- edf[ , getPlausibleValue(yvar[yvi], data)[i]]
        }
      }
      # then set yvars[i] (e.g. outcome1) to be the evaluation at this PV point
      edf[ , yvars[i]] <- as.numeric(eval(formula[[2]], edf))
    }
    # finally, correctly set yvar0
    edf$yvar0 <- edf[ , yvar0]
  } # end if(any(pvy)), no else

  # 5) run the main regression
  # run a regression, starting with the first PV or maybe the only outcome variable
  yvar0 <- yvars[1]
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  edf$yvar0 <- edf[ , yvar0]
  frm <- update(formula, yvar0 ~ .)
  edf$w <- edf[ , wgt]
  lm0 <- rq(frm, data = edf, weights = w, tau = tau, ...)
  if (returnLm0) {
    return(lm0)
  }
  # this grabs the list of weight variable names
  wgtl <- getAttributes(sdf, "weights")[[wgt]]
  varEstInputs <- list()
  # note we have not made the B matrix
  madeB <- FALSE
  X_lmi <- model.matrix(frm, edf)
  # used to estimate a regression at a particular y and weight. X is fixed.
  stat_rq <- function(pv, w) {
    lmi <- rq.wfit(x = X_lmi, y = pv, weights = w, tau = tau, ...)
    coi <- coef(lmi)
    # rho from bottom of second column of 1296 in Kronker and Machado 1999
    # here "<" turns to T/F which gets recast as 1/0 implementing the I function automatically
    # sum to get V(tau) (page 1297)
    rho <- sum(lmi$resid * (tau - (lmi$resid < 0)))
    return(c(coi, "r.squared" = rho))
  }
  rho <- lm0$rho
  jkSumMult <- getAttributes(data, "jkSumMultiplier")
  # 6) run the regressions, form the inputs for variance estimation
  if (any(pvy)) { # the y variable is a plausible value
    if (varMethod != "j") {
      stop("Quantile Regression requires Jackknife as the variance estimation method.")
    }
    rho <- rep(0, length(yvars))
    if (linkingError) {
      if (jrrIMax != 1) {
        warning("The linking error variance estimator only supports ", dQuote("jrrIMax=1"), ". Resetting to 1.")
        jrrIMax <- 1
      }
      repWeights <- paste0(wgtl$jkbase, wgtl$jksuffixes)
      # NAEP linking error case, use linking error var estimator
      # get the mean estimate
      est <- getLinkingEst(
        data = edf,
        pvEst = yvars[grep("_est", yvars)],
        stat = stat_rq,
        wgt = wgt
      )
      # coefficients matrix
      coefm <- est$coef
      coef <- apply(coefm, 2, mean)
      coefm0 <- t(t(coefm) - coef)
      # get imputation variance
      impVar <- getLinkingImpVar(
        data = edf,
        pvImp = yvars[grep("_imp", yvars)],
        ramCols = ncol(getRAM()),
        stat = stat_rq,
        wgt = wgt,
        T0 = est$est,
        T0Centered = FALSE
      )
      # get sampling variance
      sampVar <- getLinkingSampVar(edf,
        pvSamp = yvars[grep("_samp", yvars)],
        stat = stat_rq,
        rwgt = repWeights,
        T0 = est$est,
        T0Centered = FALSE
      )

      varM <- list(sampVar$Bi)
      varEstInputs[["JK"]] <- sampVar$veiJK
      varEstInputs[["PV"]] <- impVar$veiImp
      # drop r.squared term
      varm <- matrix(NA, nrow = jrrIMax, ncol = length(coef(lm0)))
      varm[1, ] <- sampVar$V[-length(sampVar$V)]
      # M just for estimation variables
      M <- length(yvars[grep("_est", yvars)])
      Vimp <- impVar$V
      Vimp <- Vimp[!names(Vimp) %in% "r.squared"]
    } else { # end if(linkingError)

      # this if condition takes care of the Taylor series method as well as the jackknife method
      # for equations, see the statistics vignette
      jrrIMax <- min(jrrIMax, length(yvars))
      # varm is the variance matrix by coefficient and PV (for V_jrr)
      varm <- matrix(NA, nrow = jrrIMax, ncol = length(coef(lm0)))
      varM <- list()
      # coefficients by PV
      coefm <- getEst(
        data = edf,
        pvEst = yvars,
        stat = stat_rq,
        wgt = wgt
      )
      # store rho
      rho <- coefm$est["r.squared"]
      # remove rho, hiding as r.squared to help the helper function getVarEstJK
      coefm <- coefm$coef[ , colnames(coefm$coef) != "r.squared"]
      coef <- apply(coefm, 2, mean)
      coefm0 <- t(t(coefm) - coef)
      for (pvi in 1:jrrIMax) { # for each PV (up to jrrIMax)
        res <- getVarEstJK(
          stat = stat_rq,
          yvar = edf[ , yvars[pvi]],
          wgtM = edf[ , paste0(wgtl$jkbase, wgtl$jksuffixes)],
          co0 = coefm[pvi, ],
          jkSumMult = jkSumMult,
          pvName = pvi
        )
        varM[[pvi]] <- res$Bi
        varEstInputs[["JK"]] <- rbind(varEstInputs[["JK"]], res$veiJK)
        varm[pvi, ] <- res$VsampInp
      } # End while loop: pvi < length(yvars)
      M <- length(yvars)
      # imputation variance
      Vimp <- (M + 1) / M * apply(coefm, 2, var)
      # make PV varEstInputs
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
    } # end else for if(linkingError)
    # imputaiton variance / variance due to uncertaintly about PVs
    # calculate van Buuren B
    B <- (1 / (M - 1)) * Reduce(
      "+", # add up the matrix results of the sapply
      sapply(1:nrow(coefm), function(q) {
        # within each PV set, calculate the outer product
        # (2.19 of Van Buuren)
        outer(coefm0[q, ], coefm0[q, ])
      }, simplify = FALSE)
    )
    madeB <- TRUE

    # \bar{U} from 2.18 in var Buuren
    Ubar <- (1 / length(varM)) * Reduce("+", varM)
    # sampling variance
    Vjrr <- apply(varm[1:jrrIMax, , drop = FALSE], 2, mean)
    names(Vjrr) <- names(coef(lm0))
  } else { # end if(pvy)
    # the y variable is not a plausible value
    # this section handles jackknife and Taylor series estimation
    if (varMethod == "j") {
      # jackknife variance estimation
      coef <- coef(lm0) # coefficients come from full sample weights run
      jkSumMult <- getAttributes(data, "jkSumMultiplier")
      res <- getVarEstJK(
        stat = stat_rq,
        yvar = edf[ , yvars[1]],
        wgtM = edf[ , paste0(wgtl$jkbase, wgtl$jksuffixes)],
        co0 = coef,
        jkSumMult = jkSumMult,
        pvName = 1
      )
      Ubar <- res$Bi
      Vjrr <- res$VsampInp
      varEstInputs[["JK"]] <- res$veiJK
      M <- 1 # only on replicate when there are no PVs
      Vimp <- 0 # no imputation variance when there are no PVs
      varm <- NULL # no variance by PV
      coefm <- NULL # no coefficients matrix by PV
    } else {
      # error message
      stop("Quantile Regression requires Jackknife as the variance estimation method.")
    } # end else for if(varMethod == "j")
  } # end else for if(pvy)

  # 7) form output, including final variance estimation

  # Deal with estimates with no sampling variance, make sure they return NA for SE
  Vjrr[Vjrr == 0] <- NA
  V <- Vjrr + Vimp
  coef <- coef[seq_along(coef(lm0))]
  names(coef) <- names(coef(lm0))
  se <- sqrt(V)
  names(se) <- names(coef(lm0))

  # get fitted and resid based on overall coef
  X <- model.matrix(frm, edf)
  fitted1 <- as.vector(X %*% coef)
  if (linkingError) {
    yve <- yvars[grep("_est", yvars)]
    Y <- sapply(seq_along(yve), function(yi) {
      as.vector(edf[ , yve[yi]])
    }, simplify = TRUE)
  } else {
    Y <- sapply(seq_along(yvars), function(yi) {
      as.vector(edf[ , yvars[yi]])
    }, simplify = TRUE)
  }
  resid1 <- Y - fitted1
  colnames(resid1) <- NULL

  # residual df calculation (see vignette)
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
    # remove r-sq, if present
    coefm <- coefm[ , seq_along(coef)]
    fitted2 <- as.matrix(X %*% t(coefm))
    resid2 <- Y - fitted2
  }

  coefmat <- data.frame(
    coef = coef,
    se = se,
    t = coef / se
  )

  # assign var that are almost zero (numerically zero) to be exactly zero.
  # If this isn't done the dof can be optimistic.
  if (!is.null(varEstInputs$JK)) {
    varEstInputs$JK$value[which(abs(varEstInputs$JK$value) < (sqrt(.Machine$double.eps) * sqrt(nrow(varEstInputs$JK))))] <- 0
  }
  if (varMethod == "j") {
    coefmat$dof <- sapply(names(coef), function(cn) {
      DoFCorrection(varEstA = varEstInputs, varA = cn, method = "JR")
    })
  } else {
    # error message
    stop("Quantile Regression requires Jackknife as the variance estimation method.")
  }

  pti <- pt(coefmat$t, df = coefmat$dof)
  coefmat[ , "Pr(>|t|)"] <- 2 * pmin(pti, 1 - pti)
  njk <- length(wgtl$jksuffixes)
  varmeth <- ifelse(varMethod == "t", "Taylor series", "jackknife")
  res <- list(
    call = call, formula = formula, coef = coef, se = se, Vimp = Vimp,
    Vjrr = Vjrr, M = M, varm = varm, coefm = coefm,
    coefmat = coefmat, rho = mean(rho), weight = wgt,
    npv = length(yvars), jrrIMax = min(jrrIMax, length(yvars)),
    njk = njk, varMethod = varmeth,
    residuals = resid1, fitted.values = fitted1, residual.df = resdf
  )
  if (!is.null(coefm)) {
    res <- c(res, list(PV.residuals = resid2, PV.fitted.values = fitted2))
  }

  if (madeB) {
    # equation 2.29 in var Buuren, pp 42
    # make sure Ubar has the correct dim (k)
    k <- length(coef)
    B <- B[1:k, 1:k]
    tryCatch(
      # make sure Ubar has the correct dim
      rbar <- (1 + 1 / M) * (1 / nrow(Ubar)) * sum(diag(B %*% solve(Ubar))),
      error = function(e) {
        rbar <<- (1 + 1 / M) * (1 / nrow(Ubar)) * sum(diag(B %*% MASS::ginv(Ubar)))
        frm <- Formula::Formula(frm)
        if (terms(frm, lhs = 0, rhs = NULL) == ~1) {
          warning("A variance estimate was replaced with NA, because there was no variance across strata.", call. = FALSE)
        } else {
          # this happens with the solve fails but the rank of Ubar is greater than 1
          warning("Variance estimation problematic, consider using jackknife", call. = FALSE)
        }
      }
    )
    Ttilde <- (1 + rbar) * Ubar
    res <- c(res, list(B = B, U = Ubar, rbar = rbar, Ttilde = Ttilde))
  }
  if (returnNumberOfPSU) {
    stratumVar <- getAttributes(data, "stratumVar")
    psuVar <- getAttributes(data, "psuVar")
    if ("JK1" %in% stratumVar) {
      res <- c(res, list(nPSU = nrow(edf)))
    } else {
      res <- c(res, list(nPSU = nrow(unique(edf[ , c(stratumVar, psuVar)]))))
    }
  }
  if (returnVarEstInputs) {
    res <- c(res, list(returnVarEstInputs = returnVarEstInputs))
    psuVar <- getPSUVar(data, wgt)
    stratumVar <- getStratumVar(data, wgt)
    if (all(c(stratumVar, psuVar) %in% colnames(edf))) {
      res <- c(res, list(waldDenomBaseDof = waldDof(edf, stratumVar, psuVar)))
    }
  }
  res <- c(res, list(n0 = nrow2.edsurvey.data.frame(data), nUsed = nrow(edf)))
  if (inherits(data, "edsurvey.data.frame")) {
    res <- c(res, list(data = data))
  } else {
    res <- c(res, list(lm0 = lm0))
  }
  res$tau <- tau
  class(res) <- "edsurveyRq"
  return(res)
}

#' @method print edsurveyRq
#' @export
print.edsurveyRq <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  print(coef(x), ...)
}

#' @method print edsurveyRqList
#' @export
print.edsurveyRqList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("rq", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(coef(x[[i]]), ...)
  }
}

#' @method summary edsurveyRq
#' @export
summary.edsurveyRq <- function(object, ...) {
  class(object) <- "summary.edsurveyRq"
  object
}

#' @method summary edsurveyRqList
#' @export
summary.edsurveyRqList <- function(object, ...) {
  class(object) <- "summary.edsurveyRqList"
  for (i in seq_along(object)) {
    class(object[[i]]) <- "summary.edsurveyRq"
  }
  object
}

#' @method print summary.edsurveyRq
#' @importFrom stats printCoefmat
#' @export
print.summary.edsurveyRq <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  cat(paste0("\nFormula: ", paste(deparse(x$formula), collapse = ""), "\n\n"))
  cat(paste0("tau: ", x$tau, "\n"))
  if (x$npv != 1) {
    cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ", x$varMethod, "\n")) # keep reporting variance method so it is clear to the user, even though the user doesn't need to specify varMethod
  if (!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  cat(paste0("full data n: ", x$n0, "\n"))
  if (!is.null(x$nPSU)) {
    cat(paste0("n PSU: ", x$nPSU, "\n"))
  }
  cat(paste0("n used: ", x$nUsed, "\n\n"))

  cat(paste0("Coefficients:\n"))
  printCoefmat(x$coefmat, P.values = TRUE, has.Pvalue = TRUE)
}

#' @method print summary.edsurveyRqList
#' @export
print.summary.edsurveyRqList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("rq", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(x[[i]], ...)
  }
}

#' @method coef edsurveyRq
#' @export
coef.edsurveyRq <- function(object, ...) {
  object$coef
}

#' @method coef edsurveyRqList
#' @export
coef.edsurveyRqList <- function(object, ...) {
  sapply(object, function(li) {
    li$coef
  })
}

#' @method vcov edsurveyRq
#' @export
vcov.edsurveyRq <- function(object, ...) {
  if (all(c("U", "B") %in% names(object))) {
    if (object$M > 1) {
      # there are PVs, V_samp + V_imp
      return(object$U + (object$M + 1) / object$M * object$B)
    } else {
      # no PVs, V_samp only
      return(object$U)
    }
  }
  if (is.null(object$varEstInputs)) {
    stop("This model must be fit with returnVarEstInputs=TRUE or with Taylor series to find the covariance matrix.")
  }
  varnames <- expand.grid(names(coef(object)), names(coef(object)))
  vc <- mapply(varEstToCov, varA = varnames$Var1, varB = varnames$Var2, MoreArgs = list(varEstA = object$varEstInputs, jkSumMultiplier = object$data$jkSumMultiplier))
  matrix(vc, nrow = length(coef(object)), ncol = length(coef(object)))
}
