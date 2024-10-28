#' @title Multivariate Regression
#'
#' @description Fits a multivariate linear model that uses weights and variance
#'              estimates appropriate for the \code{edsurvey.data.frame}.
#'
#' @param formula    a \ifelse{latex}{\code{Formula} package \code{Formula}}{\code{\link[Formula]{Formula}}} for the
#'                   linear model. See \ifelse{latex}{\code{Formula}}{\code{\link[Formula]{Formula}}};
#'                   left-hand side variables are separated with
#'                   vertical pipes (\code{|}). See Examples.
#' @param data       an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @param weightVar  character indicating the weight variable to use (see Details).
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}}
#'                   term (see
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param relevels   a list. Used to change the contrasts from the
#'                   default treatment contrasts to treatment contrasts with a chosen omitted
#'                   group (the reference group).
#'                   To do this, the user puts an element on the list with the same name as
#'                   a variable to change contrasts on
#'                   and then make the value for that list element equal to the value
#'                   that should
#'                   be the omitted group (the reference group).
#' @param dropOmittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      those levels of all factor variables that are specified
#'                      in \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses
#'                          the default conditions stored in \code{edsurvey.data.frame}
#'                          to subset the data. Use \code{print} on an
#'                          \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode} \code{=} \code{list(var1=} \code{list(from=c("a","b","c"),} \code{to ="d"))}.
#' @param returnVarEstInputs a logical value. Set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allow for
#'                           computation of covariances between estimates.
#' @param estMethod a character value indicating which estimation method to use.
#'                  Default is \code{OLS}; other option is \code{GLS}.
#' @param verbose logical; indicates whether a detailed printout should display during execution
#'
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}
#'
#' @details
#'
#' This function implements an estimator that correctly handles multiple left-hand
#' side variables that are either numeric or plausible values, allows for survey
#' sampling weights, and estimates variances using the jackknife replication method.
#' The vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#' describes estimation of the reported statistics.
#'
#' The \bold{coefficients} are estimated using the sample weights according to the section
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Not Present}
#' or the section
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Present,}
#' depending on if there are assessment variables or variables with plausible values in them.
#'
#' The \bold{coefficient of determination (R-squared value)} is similarly estimated by finding
#' the average R-squared using the sample weights for each set of plausible values.
#'
#' \subsection{Variance estimation of coefficients}{
#'   All variance estimation methods are shown in the vignette titled
#'   \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#'
#'   When the predicted value does not have plausible values, the variance of the coefficients
#'   is estimated according to the section \dQuote{Estimation of Standard Errors
#'   of Weighted Means When Plausible Values Are Not Present, Using the Jackknife Method.}
#'
#'   When plausible values are present, the variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Present, Using the Jackknife Method.}
#' }
#'
#' For more information on the specifics of multivariate regression, see the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Multivariate_Regression.pdf}{Methods and Overview of Using EdSurvey for Multivariate Regression}.
#'
#'
#'
#' @return
#' An \code{edsurvey.mvrlm} with elements:
#'    \item{call}{the function call}
#'    \item{formula}{the formula used to fit the model}
#'    \item{coef}{the estimates of the coefficients}
#'    \item{se}{the standard error estimates of the coefficients}
#'    \item{Vimp}{the estimated variance caused by uncertainty in the scores (plausible value variables)}
#'    \item{Vjrr}{the estimated variance caused by sampling}
#'    \item{M}{the number of plausible values}
#'    \item{varm}{the variance estimates under the various plausible values}
#'    \item{coefm}{the values of the coefficients under the various plausible values}
#'    \item{coefmat}{the coefficient matrix (typically produced by the summary of a model)}
#'    \item{r.squared}{the coefficient of determination}
#'    \item{weight}{the name of the weight variable}
#'    \item{npv}{the number of plausible values}
#'    \item{njk}{the number of the jackknife replicates used}
#'    \item{varEstInputs}{When \code{returnVarEstInputs} is \code{TRUE},
#'                        this element is returned. These are
#'                        used for calculating covariances with
#'                        \code{\link{varEstToCov}}.}
#'    \item{residuals}{residuals for each of the PV models}
#'    \item{fitted.values}{model fitted values}
#'    \item{residCov}{residual covariance matrix for dependent variables}
#'    \item{residPV}{residuals for each dependent variable}
#'    \item{inputs}{coefficient estimation input matrices}
#'    \item{n0}{full data \emph{n}}
#'    \item{nUsed}{\emph{n} used for model}
#'    \item{B}{imputation variance-covariance matrix, before multiplication by (M+1)/M}
#'    \item{U}{sampling variance-covariance matrix}
#'
#' @seealso \ifelse{latex}{the stats package \code{lm}}{\code{\link[stats]{lm}}}, \code{\link{lm.sdf}}
#' @author Alex Lishinski and Paul Bailey
#'
#' @example \man\examples\mvrlm.sdf.R
#' @importFrom Matrix sparse.model.matrix Diagonal
#' @importFrom Formula Formula as.Formula model.part
#' @importFrom stats lm aggregate pt relevel model.matrix lm.wfit as.formula model.frame
#' @importFrom utils stack
#' @importFrom car makeHypothesis linearHypothesis
#' @export

mvrlm.sdf <- function(formula,
                      data,
                      weightVar = NULL,
                      relevels = list(),
                      jrrIMax = 1,
                      dropOmittedLevels = TRUE,
                      defaultConditions = TRUE,
                      recode = NULL,
                      returnVarEstInputs = FALSE,
                      estMethod = "OLS",
                      verbose = TRUE,
                      omittedLevels = deprecated()) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "mvrlm.sdf(omittedLevels)", "mvrlm.sdf(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")

  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), data)
    class(res) <- "edsurveyMvrlmList"
    return(res)
  } else {
    return(calc.mvrlm.sdf(
      formula = formula,
      data = data,
      weightVar = weightVar,
      relevels = relevels,
      jrrIMax = jrrIMax,
      omittedLevels = dropOmittedLevels,
      defaultConditions = defaultConditions,
      missingDefaultConditions = missing(defaultConditions),
      recode = recode,
      returnVarEstInputs = returnVarEstInputs,
      estMethod = estMethod,
      verbose = verbose
    ))
  }
}

calc.mvrlm.sdf <- function(formula,
                           data,
                           weightVar = NULL,
                           relevels = list(),
                           varMethod = c("jackknife"),
                           jrrIMax = 1,
                           omittedLevels = TRUE,
                           defaultConditions = TRUE,
                           missingDefaultConditions = TRUE,
                           recode = NULL,
                           returnVarEstInputs = FALSE,
                           call = NULL,
                           estMethod = "OLS",
                           verbose = TRUE) {
  if (is.null(call)) {
    call <- match.call()
  }

  # outline:
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

  wgt <- checkWeightVar(data, weightVar)

  # checking for linking error and stop
  if (any(grepl("_linking", all.vars(formula), fixed = TRUE))) {
    stop("mvrlm.sdf does not support linking error.")
  }
  # 2) Get the data
  getDataArgs <- list(
    data = sdf,
    varnames = c(all.vars(formula), wgt), # , taylorVars),
    returnJKreplicates = (varMethod == "jackknife"),
    drop = FALSE,
    omittedLevels = omittedLevels,
    recode = recode,
    includeNaLabel = TRUE,
    dropUnusedLevels = TRUE
  )

  # # Default conditions should be included only if the user set it. This adds the argument only if needed
  if (!missingDefaultConditions) {
    getDataArgs <- c(getDataArgs, list(defaultConditions = defaultConditions))
  }
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
  if (any(edf[ , wgt] <= 0)) {
    if (verbose) {
      message("Removing rows with 0 weight from analysis.")
    }
    edf <- edf[edf[ , wgt] > 0, ]
  }

  # 3) deal with relevels.
  # An argument that allows the user to change the omitted level for a factor variable
  if (length(relevels) > 0) {
    for (i in seq_along(relevels)) {
      vari <- names(relevels)[i]
      if (!vari %in% names(edf)) {
        stop(paste0(
          "In the ", sQuote("relevels"),
          " argument, cannot find the variable named ", dQuote(vari), "."
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

  #################
  # 4) Model Formula Building

  frm <- Formula(formula)
  # get the lhs and rhs model terms
  lhsVars <- terms(frm, lhs = NULL, rhs = 0)
  rhsVars <- terms(frm, lhs = 0, rhs = NULL)

  # as a character vector
  yvar <- attr(lhsVars, "term.labels")
  # keep the number of dependent vars
  nDV <- length(yvar)

  if (nDV < 2) {
    stop("Multivariate regression requires >= 2 dependent variables; fix formula or use lm.sdf.")
  }

  # 4) deal with yvar having plausible values

  # vector indicating whether each y var has
  pvy <- unlist(lapply(X = yvar, FUN = hasPlausibleValue, sdf))

  # initialize list of y vars
  yvars <- list()
  # add PVs to list of Y vars if there are any PVs
  if (sum(pvy) > 0) {
    npv <- length(getPlausibleValue(yvar[pvy][1], sdf))
    for (i in seq_along(pvy)) {
      if (pvy[i]) {
        yvars[[i]] <- getPlausibleValue(yvar[[i]], sdf)
      } else {
        # if not, make sure that this variable is numeric
        edf[ , yvar[[i]]] <- as.numeric(edf[ , yvar[[i]]])
        # and repeat variable name npv times
        yvars[[i]] <- rep(yvar[[i]], npv)
      } # End of if statment: pvy
    }
  } else { # if no PV y variables, just grab the y variable names from above
    yvars <- yvar
  }

  # put the rhs and lhs formula terms back together for the pth PV
  pvModelFormula <- function(p) {
    dVars <- lapply(yvars, `[[`, p)
    dVars <- paste(dVars, collapse = " | ")

    iVars <- attr(rhsVars, "term.labels")
    iVars <- paste(iVars, collapse = " | ")

    formString <- paste(dVars, iVars, sep = " ~ ")
    frm <- as.Formula(formString)
    frm
  }

  # grabs the appropriate X,Y,and W for a given formula (with the original weights)
  coefEstInputs <- function(frm, wgt, edf) {
    # must tell formula "frm" to use the current environment
    env <- list2env(list(edf = edf, wgt = wgt))
    attr(frm, ".Environment") <- env
    mdf <- model.frame(frm, data = edf, weights = edf[ , wgt])
    X <- model.matrix(object = frm, data = mdf, rhs = NULL)
    coefNames <- colnames(X)
    Y <- as.matrix(model.part(object = frm, data = mdf, lhs = NULL))
    W <- Diagonal(x = mdf[ , "(weights)"])
    list(X = X, Y = Y, W = W, coefNames = coefNames, mdf = mdf, frm = frm)
  }


  #######################
  # 5) Multivariate Regression Coefficient Estimation Helper Functions

  # Unweighted coefficient estimation function
  coefEstUnw <- function(X, Y) {
    solve(t(X) %*% X) %*% (t(X) %*% Y)
  }

  # Coefficient estimation function accounting for weights
  coefEstWtd <- function(X, Y, W) {
    bHat <- try(solve((t(X) %*% W) %*% X) %*% (t(X) %*% W %*% Y), silent = TRUE)
    if (inherits(bHat, "try-error")) { # deals with the case where there's a singularity by using lm.wfit instead
      bHat <- lm.wfit(x = X, y = Y, w = diag(W))$coefficients
    }
    bHat
  }

  # calculate residuals
  getResiduals <- function(Y, X, Beta) {
    Y - (X %*% Beta)
  }

  # calculate the residual covariance matrix
  getResidCov <- function(epsilon) {
    t(epsilon) %*% epsilon
  }

  # calculate inverse omega matrix (but with weights)
  getOmegaInv <- function(residCov, W) {
    omegaHatInv <- solve(residCov) %x% solve(W)
  }

  # generalized estimation function allowing an estimated residual covariance matrix to be included for certain estimation methods
  coefEstOmega <- function(X, Y, omega, W) {
    # apply the weights to X first
    X <- W * X
    # replace X with block diagonal X
    X <- do.call(bdiag, args = replicate(n = nDV, X, simplify = FALSE))
    # apply the weights to Y first
    Y <- W * Y
    # replace Y with version of Y that works
    Y <- do.call(rbind, replicate(n = nDV, Y, simplify = FALSE))
    solve((t(X) %*% omega) %*% X) %*% ((t(X) %*% omega) %*% Y)
  }

  # function for extracting the coefficients from block diagonal matrix that comes out of coefEstOmega
  extractCoefs <- function(coefs) {
    nEQ <- nDV
    cols <- list()
    # grab the diagonal blocks of coefficients for each column (dep var)
    for (i in 1:nEQ) {
      cols[[i]] <- coefs[(((i - 1) * nCoefs) + 1):(nCoefs * i), i]
    }

    mat <- do.call(cbind, cols)
    colnames(mat) <- colnames(coefs)
    rownames(mat) <- inputs$coefNames
    mat
  }

  getR2 <- function(fitted, weight, resid) {
    r <- resid
    f <- fitted
    w <- weight

    m <- sum(w * f / sum(w))
    mss <- sum(w * (f - m)^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
    r.squared <- mss / (mss + rss)
    r.squared
  }

  # get fitted values for regression model
  getFitted <- function(X, B) {
    X %*% B
  }

  ### estimate model for baseline values

  # get the formula for the first PV (or just the values if there are no PVs)
  frm <- pvModelFormula(1)

  # Generate and save coefficient estimation inputs
  inputs <- coefEstInputs(frm = frm, wgt = wgt, edf = edf)
  estInputs <- inputs

  # get coefficient estimates
  bHatUnw <- coefEstUnw(inputs$X, inputs$Y)
  bHatWtd <- coefEstWtd(inputs$X, inputs$Y, inputs$W)

  # get residuals and residual covariance
  epsilon <- getResiduals(Y = inputs$Y, X = inputs$X, Beta = bHatWtd)
  sigmaHat <- getResidCov(epsilon = epsilon)

  # get number of coefficients and fitted values
  nCoefs <- length(inputs$coefNames)
  fitted <- getFitted(inputs$X, bHatWtd)

  # get GLS coefficient estimates
  omegaHatInv <- solve(sigmaHat) %x% solve(inputs$W)
  bHatOmega <- extractCoefs(coefEstOmega(inputs$X, inputs$Y, omega = omegaHatInv, diag(inputs$W)))

  #######################
  # 6) Variance Estimation

  wgtl <- getAttributes(sdf, "weights")[[wgt]]

  varEstInputs <- list()

  madeB <- FALSE

  if (sum(pvy) > 0) { # there are plausible values in 1 or more of the dependent variables
    jrrIMax <- min(jrrIMax, min(unlist(lapply(yvars, length))))

    # for the first (PV) formula
    coefInputs <- coefEstInputs(frm = frm, wgt = wgt, edf = edf)

    # OLS estimator used to calculate coefficients
    co0 <- coefEstWtd(coefInputs$X, coefInputs$Y, coefInputs$W)

    if (estMethod == "GLS") {
      # get the omega hat matrix for estimation from the initial OLS run
      epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
      sigmaHat <- getResidCov(epsilon)
      omegaHatInv <- solve(sigmaHat) %x% solve(coefInputs$W)
      # do the estimation and extract the coefficients
      bHatOmega <- coefEstOmega(coefInputs$X, coefInputs$Y, omegaHatInv, diag(coefInputs$W))
      co0 <- extractCoefs(bHatOmega)
    }

    # flattened matrix of coefficients
    coef0 <- as.vector(co0)
    names(coef0) <- paste(rep(colnames(co0), each = nCoefs), rownames(co0))
    # varm is the variance matrix by coefficient and PV (for V_jrr)
    varm <- matrix(NA, nrow = jrrIMax, ncol = (ncol(co0) * nrow(co0)))
    varM <- list()
    # coefficients by PV
    coefm <- matrix(NA, nrow=min(unlist(lapply(yvars, length))), ncol=(ncol(co0) * nrow(co0)))

    # R-squared by PV (needs to be a matrix)
    r2s <- matrix(NA, nrow = length(yvars[[1]]), ncol = length(yvar))

    # Initialize objects for residuals
    # PV-wise list of residuals
    residuals <- list()
    # DV-wise list of residuals
    resid <- list()

    # Initialize objects for residual covariance and fitted values
    residCovar <- list()
    fitted <- list()

    # y is a variable with plausible values and we are using the JK1 approach
    for (pvi in 1:jrrIMax) {
      coefa <- matrix(NA, nrow = length(wgtl$jksuffixes), ncol = (ncol(co0) * nrow(co0)))

      # switch formula to the pertinent plausible value
      frm <- pvModelFormula(pvi)
      # get the new inputs
      coefInputs <- coefEstInputs(frm = frm, wgt = wgt, edf = edf)
      # get the new coefs
      co0 <- coefEstWtd(coefInputs$X, coefInputs$Y, coefInputs$W)

      if (estMethod == "GLS") {
        # get the omega hat matrix for estimation from the initial OLS run
        epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
        sigmaHat <- getResidCov(epsilon)
        omegaHatInv <- solve(sigmaHat) %x% solve(coefInputs$W)
        # do the estimation and extract the coefficients
        bHatOmega <- coefEstOmega(coefInputs$X, coefInputs$Y, omegaHatInv, diag(coefInputs$W))
        co0 <- extractCoefs(bHatOmega)
      } else {
        epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
        sigmaHat <- getResidCov(epsilon)
      }

      residCovar[[length(residCovar) + 1]] <- sigmaHat
      resid[[length(resid) + 1]] <- epsilon
      fitted[[length(fitted) + 1]] <- getFitted(coefInputs$X, co0)

      # take the coefs and add them to the matrix (as a flattened row)
      coefm[pvi, ] <- as.vector(co0)
      # also save the sample weight coefficients as a vector for the calculation below the loop
      coef0 <- as.vector(co0)
      # R2s, where they are extracted

      epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
      residuals[[length(residuals) + 1]] <- epsilon

      fitVals <- getFitted(coefInputs$X, co0)

      yCols <- split(inputs$Y, rep(1:ncol(coefInputs$Y), each = nrow(coefInputs$Y)))
      residCols <- split(epsilon, rep(1:ncol(epsilon), each = nrow(epsilon)))
      fittedCols <- split(fitVals, rep(1:ncol(fitVals), each = nrow(fitVals)))

      R2 <- mapply(FUN = getR2, fitted = fittedCols, resid = residCols, MoreArgs = list(weight = coefInputs$W))

      r2s[pvi, ] <- unlist(R2)

      for (jki in seq_along(wgtl$jksuffixes)) {
        coefInputs_jk <- coefEstInputs(frm = frm, wgt = paste0(wgtl$jkbase, wgtl$jksuffixes[jki]), edf = edf)
        coefs <- coefEstWtd(coefInputs_jk$X, coefInputs_jk$Y, coefInputs_jk$W)

        if (estMethod == "GLS") {
          # get the omega hat matrix for estimation from the initial OLS run
          epsilon <- getResiduals(Y = coefInputs_jk$Y, X = coefInputs_jk$X, Beta = coefs)
          sigmaHat <- getResidCov(epsilon)
          omegaHatInv <- solve(sigmaHat) %x% solve(coefInputs_jk$W)

          # calculate and extract the coefficients
          bHatOmega <- coefEstOmega(coefInputs_jk$X, coefInputs_jk$Y, omegaHatInv, diag(coefInputs_jk$W))
          coefs <- extractCoefs(bHatOmega)
        } else {
          epsilon <- getResiduals(Y = coefInputs_jk$Y, X = coefInputs_jk$X, Beta = co0)
          sigmaHat <- getResidCov(epsilon)
        }
        coefa[jki, ] <- as.vector(coefs)
      }

      coefa <- t((t(coefa) - coef0))

      if (sum(!is.na(coefa)) == 0 || sum(coefa, na.rm = TRUE) == 0) {
        stop("No variance across strata. This could be due to only one stratum being included in the sample.")
      }

      # parameters to construct variance estimation input data frames
      njk <- length(wgtl$jksuffixes)
      M <- jrrIMax
      baseUnit <- njk * length(coefInputs$coefNames)

      if (pvi == 1) {
        dfl <- lapply(1:ncol(coefa), function(coli) {
          data.frame(
            PV = rep(pvi, nrow(coefa)),
            JKreplicate = 1:nrow(coefa),
            variable = rep(coefInputs$coefNames, nDV)[coli],
            value = coefa[ , coli]
          )
        })
        veiJK <- do.call(rbind, dfl)
      } else {
        dfl <- lapply(1:ncol(coefa), function(coli) {
          data.frame(
            PV = rep(pvi, nrow(coefa)),
            JKreplicate = 1:nrow(coefa),
            # variable=names(coef(lmi))[coli],
            variable = rep(coefInputs$coefNames, nDV)[coli],
            value = coefa[ , coli]
          )
        })
        veiJK <- rbind(veiJK, do.call(rbind, dfl))
      }

      # }
      # varm is now the diagonal of this. Notice that this only uses first
      # jrrIMax PVs
      varM[[pvi]] <- getAttributes(data, "jkSumMultiplier") *
        Reduce(
          "+",
          lapply(1:nrow(coefa), function(jki) {
            cc <- coefa[jki, ]
            cc[is.na(cc)] <- 0
            outer(cc, cc)
          })
        )
      coefa <- coefa^2
      varm[pvi, ] <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum)
    } # End of for loop: (pvi in 1:jrrIMax)

    # add column with dv names
    veiJK$dv <- rep(yvar, each = baseUnit, length.out = baseUnit * M * nDV)
    varEstInputs[["JK"]] <- veiJK

    # imputation variance / variance due to uncertaintly about PVs

    while (pvi < min(unlist(lapply(yvars, length)))) {
      pvi <- pvi + 1

      frm <- pvModelFormula(pvi)
      # get the new inputs
      coefInputs <- coefEstInputs(frm = frm, wgt = wgt, edf = edf)

      # get the new coefs
      co0 <- coefEstWtd(coefInputs$X, coefInputs$Y, coefInputs$W)

      epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
      residuals[[length(residuals) + 1]] <- epsilon

      fitVals <- getFitted(coefInputs$X, co0)

      yCols <- split(inputs$Y, rep(1:ncol(coefInputs$Y), each = nrow(coefInputs$Y)))
      residCols <- split(epsilon, rep(1:ncol(epsilon), each = nrow(epsilon)))
      fittedCols <- split(fitVals, rep(1:ncol(fitVals), each = nrow(fitVals)))

      R2 <- mapply(FUN = getR2, fitted = fittedCols, resid = residCols, MoreArgs = list(weight = coefInputs$W))
      r2s[pvi, ] <- unlist(R2)
      # take the coefs and add them to the matrix (as a flattened row)
      coefm[pvi, ] <- as.vector(co0)
    }

    coefm0 <- t(t(coefm) - apply(coefm, 2, mean))
    B <- (1 / (nrow(coefm) - 1)) * Reduce(
      "+", # add up the matrix results of the lapply
      lapply(1:nrow(coefm), function(q) {
        # within each PV set, calculate the outer product
        # (2.19 of Van Buuren)
        outer(coefm0[q, ], coefm0[q, ])
      })
    )
    madeB <- TRUE

    # \bar{U} from 2.18 in var Buuren
    Ubar <- (1 / length(varM)) * Reduce("+", varM)

    M <- pvi
    Vimp <- (M + 1) / M * apply(coefm, 2, var)

    R2 <- colMeans(r2s, na.rm = TRUE)
    names(R2) <- yvar

    coef <- apply(coefm, 2, mean)
    coef <- matrix(coef, nrow = length(coefInputs$coefNames), ncol = nDV)

    colnames(coef) <- yvar

    # # variance due to sampling
    Vjrr <- apply(varm[1:jrrIMax, , drop = FALSE], 2, mean)

    coefsPV <- list()

    coefm <- Matrix(coefm)

    for (i in 1:nrow(coefm)) {
      coefsPV[[length(coefsPV) + 1]] <- matrix(coefm[i, ], nrow = length(coefInputs$coefNames), ncol = nDV, dimnames = list(coefInputs$coefNames, lapply(yvars, `[[`, i)))
    }

    coefm <- coefsPV

    # change the format for the coefficent matrix return object
    out <- list()
    for (i in 1:nDV) {
      co <- unlist(lapply(coefm, function(x) {
        x[ , i]
      }))
      out[[length(out) + 1]] <- matrix(co, nrow = length(coefm), ncol = length(coefInputs$coefNames), byrow = TRUE)
    }

    coefm <- out

    names(residuals) <- paste("PV", seq_along(residuals))

    sigmaHat <- (1 / M) * Reduce("+", residCovar)
    colnames(sigmaHat) <- yvar
    rownames(sigmaHat) <- yvar

    varEstInputs[["PV"]] <- coefm0
  } else { # end if statement if (pvy)

    # jackknife variance estimation
    coefInputs <- coefEstInputs(frm = frm, wgt = wgt, edf = edf)

    co0 <- coefEstWtd(coefInputs$X, coefInputs$Y, coefInputs$W)

    if (estMethod == "GLS") {
      epsilon <- getResiduals(Y = coefInputs$Y, X = coefInputs$X, Beta = co0)
      sigmaHat <- getResidCov(epsilon)
      omegaHatInv <- solve(sigmaHat) %x% solve(coefInputs$W)

      # calculate and extract the coefficients
      bHatOmega <- coefEstOmega(coefInputs$X, coefInputs$Y, omegaHatInv, diag(coefInputs$W))
      co0 <- extractCoefs(bHatOmega)
    }

    coefa <- matrix(NA, nrow = length(wgtl$jksuffixes), ncol = (ncol(co0) * nrow(co0)))

    for (jki in seq_along(wgtl$jksuffixes)) {
      coefInputs_jk <- coefEstInputs(frm = frm, wgt = paste0(wgtl$jkbase, wgtl$jksuffixes[jki]), edf = edf)
      coefs <- coefEstWtd(coefInputs_jk$X, coefInputs_jk$Y, coefInputs_jk$W)

      if (estMethod == "GLS") {
        epsilon <- getResiduals(Y = coefInputs_jk$Y, X = coefInputs_jk$X, Beta = coefs)
        sigmaHat <- getResidCov(epsilon)
        omegaHatInv <- solve(sigmaHat) %x% solve(coefInputs$W)

        # calculate and extract the coefficients
        bHatOmega <- coefEstOmega(coefInputs_jk$X, coefInputs_jk$Y, omegaHatInv, diag(coefInputs_jk$W))
        coefs <- extractCoefs(bHatOmega)
      }

      coefa[jki, ] <- as.vector(coefs)
    }

    # flattened matrix of coefficients
    coef0 <- as.vector(co0)
    names(coef0) <- rep(coefInputs$coefNames, nDV)

    coefa <- t((t(coefa) - coef0)) # conservative JK estimator

    dfl <- lapply(1:ncol(coefa), function(coli) {
      data.frame(
        PV = rep(1, nrow(coefa)),
        JKreplicate = 1:nrow(coefa),
        variable = names(coef0)[coli],
        value = coefa[ , coli]
      )
    })

    veiJK <- do.call(rbind, dfl)

    njk <- length(wgtl$jksuffixes)
    baseUnit <- njk * length(coefInputs$coefNames)

    Ubar <- getAttributes(data, "jkSumMultiplier") *
      Reduce(
        "+",
        lapply(1:nrow(coefa), function(jki) {
          cc <- coefa[jki, ]
          cc[is.na(cc)] <- 0
          outer(cc, cc)
        })
      )


    # add column with dv names
    veiJK$dv <- rep(yvar, each = baseUnit, length.out = baseUnit * nDV)
    varEstInputs[["JK"]] <- veiJK

    njk <- length(wgtl$jksuffixes)
    coefa <- coefa^2 # this is JK-2
    Vjrr <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum)
    coef <- co0 # coefficients come from full sample weights run
    Vimp <- 0 # no imputation variance when there are no PVs
    M <- 1 # only one replicate when there are no PVs
    varm <- NULL # no variance by PV
    coefm <- NULL # no coefficients matrix by PV

    yCols <- split(inputs$Y, rep(1:ncol(inputs$Y), each = nrow(inputs$Y)))
    residCols <- split(epsilon, rep(1:ncol(epsilon), each = nrow(epsilon)))
    residuals <- epsilon

    fitVals <- getFitted(coefInputs$X, co0)
    fittedCols <- split(fitVals, rep(1:ncol(fitVals), each = nrow(fitVals)))

    R2 <- mapply(FUN = getR2, fitted = fittedCols, resid = residCols, MoreArgs = list(weight = coefInputs$W))

    R2 <- as.numeric(R2)
    names(R2) <- yvar
  } # end else statement for if (pvy)

  # 7) form output, including final variance estimation
  V <- Vjrr + Vimp
  rownames(coef) <- coefInputs$coefNames
  se <- sqrt(V)
  coef <- Matrix(coef)
  names(se) <- rep(coefInputs$coefNames, times = nDV)

  njk <- length(wgtl$jksuffixes)

  varmeth <- "jackknife"

  # begin to construct coefficient matrices
  coefmatlist <- list()
  for (i in 1:nDV) {
    coefmatlist[[length(coefmatlist) + 1]] <- data.frame(
      coef = coef[ , i],
      se = se[((i - 1) * length(coef[ , i]) + 1):(i * length(coef[ , i]))],
      t = coef[ , i] / se[((i - 1) * length(coef[ , i]) + 1):(i * length(coef[ , i]))]
    )
  }

  names(coefmatlist) <- yvar
  coefmat <- coefmatlist

  # format variance estimation inputs appropriately for dof calculation
  if (sum(pvy) > 0) {
    varEstInputs$PV <- data.frame(varEstInputs$PV)
    varEstInputs$PV <- stack(varEstInputs$PV, drop = TRUE, stringsAsFactors = FALSE)

    levels(varEstInputs$PV$ind) <- unlist(lapply(coefmat, rownames), use.names = FALSE)

    colnames(varEstInputs$PV) <- c("value", "variable")

    varEstInputs$PV$PV <- rep(1:M, length.out = nrow(varEstInputs$PV))
    varEstInputs$PV$DV <- rep(yvar, each = nrow(varEstInputs$PV) / length(yvar))
  }

  for (var in yvar) {
    # variables with plausible values
    if (pvy[which(yvar == var)]) {
      vei <- list()
      # subset these correctly for DoF calculation
      vei$JK <- varEstInputs$JK[varEstInputs$JK$dv == var, ]
      vei$PV <- varEstInputs$PV[varEstInputs$PV$DV == var, ]

      cmyvar <- coefmat[[var]]

      dof <- vapply(rownames(cmyvar), function(cn) {
        DoFCorrection(varEstA = vei, varA = cn, method = "JR")
      }, FUN.VALUE=numeric(1))

      coefmat[[var]]$dof <- dof

      pti <- pt(coefmat[[var]]$t, df = coefmat[[var]]$dof)
      coefmat[[var]][ , "Pr(>|t|)"] <- 2 * pmin(pti, 1 - pti)
    } else {
      # variables without plausible values
      vei <- list()
      vei$JK <- varEstInputs$JK[varEstInputs$JK$dv == var & varEstInputs$JK$PV == 1, ]

      cmyvar <- coefmat[[var]]

      dof <- vapply(rownames(cmyvar), function(cn) {
        DoFCorrection(varEstA = vei, varA = cn, method = "JR")
      }, FUN.VALUE=numeric(1))

      coefmat[[var]]$dof <- dof

      pti <- pt(coefmat[[var]]$t, df = coefmat[[var]]$dof)
      coefmat[[var]][ , "Pr(>|t|)"] <- 2 * pmin(pti, 1 - pti)
    }
  }

  # reformat residuals output object to be a DV-wise list
  residPV <- list()
  if (sum(pvy) > 0) {
    for (var in yvar) {
      index <- which(yvar == var)
      resid <- lapply(resid, as.matrix)
      a1 <- rapply(resid, classes = "matrix", how = "list", f = function(x) x[ , index, drop = FALSE])
      a1 <- do.call(cbind, a1)
      rownames(a1) <- NULL # drop scrambled row names
      residPV[[index]] <- a1
    } # calculate residual covariance from pv residual matrices
    covarComp <- lapply(residPV, rowMeans)
    ee <- do.call(cbind, covarComp)
    residCov <- t(ee) %*% ee
    dimnames(residCov) <- list(yvar, yvar)
    # remove meaningless row names
  } else {
    residPV <- NULL
    ee <- as.matrix(residuals)
    residCov <- t(ee) %*% ee
  }

  res <- list(
    call = call, formula = formula, coef = as.matrix(coef), se = se, Vimp = Vimp,
    Vjrr = Vjrr, M = M, varm = varm, coefm = coefm, coefmat = coefmat,
    r.squared = R2, weight = wgt, npv = length(yvars[[1]]),
    njk = njk, residuals = residuals, fitted.values = fitted,
    residCov = residCov, residPV = residPV, inputs = estInputs,
    n0 = nrow2.edsurvey.data.frame(data), nUsed = nrow(edf), nDV = nDV
  )

  if (madeB) {
    res <- c(res, list(B = B, U = Ubar))
  } else {
    # used for TS vcov
    res <- c(res, list(U = Ubar))
    if (!any(pvy)) {
      res <- c(res, list(B = 0 * Ubar))
    }
  }

  if (returnVarEstInputs) {
    res <- c(res, list(varEstInputs = varEstInputs))
  }

  class(res) <- c("edsurveyMvrlm")
  res
}

#' @method print edsurveyMvrlm
#' @export
print.edsurveyMvrlm <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  print(x$coef)
}

#' @method print edsurveyMvrlmList
#' @export
print.edsurveyMvrlmList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("lm", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(coef(x[[i]]), ...)
  }
}

#' @method coef edsurveyMvrlm
#' @export
coef.edsurveyMvrlm <- function(object, ...) {
  names <- expand.grid(
    colnames(object$coef),
    rownames(object$coef)
  )
  coef <- as.vector(t(object$coef))
  names(coef) <- apply(X = names, MARGIN = 1, FUN = function(x) {
    paste(x, collapse = "_")
  })
  coef
}

#' @method coef edsurveyMvrlmList
#' @export
coef.edsurveyMvrlmList <- function(object, ...) {
  # object[[1]] used because nDV and yvar names are the same across the list
  yvar <- names(object[[1]]$coefmat)
  coeflist <- list()
  for (i in 1:object[[1]]$nDV) {
    coeflist[[yvar[i]]] <- lapply(object, function(x) {
      x$coef[ , i]
    })
  }
  return(coeflist)
}

#' @method summary edsurveyMvrlm
#' @export
summary.edsurveyMvrlm <- function(object, ...) {
  class(object) <- "summary.edsurveyMvrlm"
  object
}

#' @method summary edsurveyMvrlmList
#' @export
summary.edsurveyMvrlmList <- function(object, ...) {
  class(object) <- "summary.edsurveyMvrlmList"
  for (i in seq_along(object)) {
    class(object[[i]]) <- "summary.edsurveyMvrlm"
  }
  object
}


#' @method print summary.edsurveyMvrlm
#' @importFrom stats printCoefmat
#' @export
print.summary.edsurveyMvrlm <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  cat(paste0("\nFormula: ", paste(deparse(x$formula), collapse = ""), "\n\n"))
  if (x$npv != 1) {
    cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ", x$varMethod, "\n"))
  if (!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  cat(paste0("full data n: ", x$n0, "\n"))
  cat(paste0("n used: ", x$nUsed, "\n\n"))
  cat(paste0("Coefficients:\n\n"))
  for (i in seq_along(x$coefmat)) {
    cat(names(x$coefmat)[i], "\n")
    printCoefmat(x$coefmat[[i]], P.values = TRUE, has.Pvalue = TRUE)
    cat("\n")
  }
  cat(paste0("Residual correlation matrix:\n\n"))
  print(round(cov2cor(x$residCov), digits = 3))
  cat("\nMultiple R-squared by dependent variable: \n\n")
  print(round(x$r.squared, 4))
}

#' @method print summary.edsurveyMvrlmList
#' @export
print.summary.edsurveyMvrlmList <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  for (i in seq_along(x)) {
    cat("\n", "lm", i, "\n")
    if(use_es_round) {
      x[[i]] <- es_round(x[[i]])
    }
    print(x[[i]], ...)
  }
}

#' @method vcov edsurveyMvrlm
#' @export
vcov.edsurveyMvrlm <- function(object, ...) {
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

#' @method linearHypothesis edsurveyMvrlm
#' @importFrom car linearHypothesis.default
#' @export
linearHypothesis.edsurveyMvrlm <- function(model, hypothesis.matrix, rhs = NULL,
                                           test = c("Chisq", "F"), vcov. = NULL, singular.ok = FALSE, verbose = FALSE,
                                           coef. = coef(model), ...) {
  if (is.character(hypothesis.matrix)) {
    hypothesis <- makeHypothesis.edsurveyMvrlm(model, hypothesis.matrix)
  }
  names <- expand.grid(
    colnames(coef(model)),
    rownames(coef(model))
  )
  linearHypothesis.default(model, hypothesis.matrix = hypothesis, rhs = rhs, test = test, vcov. = vcov., verbose = FALSE)
}

#' @method makeHypothesis edsurveyMvrlm
#' @export
makeHypothesis.edsurveyMvrlm <- function(cnames, hypothesis, rhs = NULL) {
  if(!missing(rhs)){
    warning("Argument 'rhs' will be ignored.")
  }
  names <- expand.grid(
    colnames(cnames$coef),
    rownames(cnames$coef)
  )

  varnames <- apply(X = names, MARGIN = 1, FUN = function(x) {
    paste(x, collapse = "_")
  })

  hypothesis.matrix <- car::makeHypothesis(varnames, hypothesis)
  if (is.matrix(hypothesis.matrix)) {
    hypothesis.matrix <- hypothesis.matrix[ , -ncol(hypothesis.matrix)]
  } else if (is.numeric(hypothesis.matrix)) {
    hypothesis.matrix <- hypothesis.matrix[-length(hypothesis.matrix)]
  }
  hypothesis.matrix
}


#' @method df.residual edsurveyMvrlm
#' @export
df.residual.edsurveyMvrlm <- function(object, ...) {
  residuals <- if (is.list(object$residuals)) {
    df <- nrow(object$residuals[[1]]) - prod(dim(object$coef)) - 1
  } else {
    df <- nrow(object$residuals) - prod(dim(object$coef)) - 1
  }
  df
}
