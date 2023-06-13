#' @title Wald Tests
#' @description Tests on coefficient(s) of \code{edsurveyGlm}
#'              and \code{edsurveyLm} models.
#' @param model an \code{edsurveyGlm} and \code{edsurveyLm}
#' @param coefficients coefficients to be tested, by name or position in
#'                     \code{coef} vector. See Details.
#' @param H0 reference values to test coefficients against, default = 0
#'
#' @details
#'
#' When plausible values are present, likelihood ratio tests cannot be used.
#' However, the Wald test can be used to test estimated parameters in a model,
#' with the null hypothesis being that a parameter(s) is equal to some value(s).
#' In the default case where the null hypothesis value of the parameters is 0,
#' if the test fails to reject the null hypothesis, removing the variables from
#' the model will not substantially harm the fit of that model. Alternative null
#' hypothesis values also can be specified with the \code{H0} argument.
#' See Examples.
#'
#' Coefficients to test can be specified by an integer (or integer vector)
#' corresponding to the order of coefficients in the summary output. Coefficients
#' also can be specified using a character vector, to specify coefficient names
#' to test. The name of a factor variable can be used to test all levels of that
#' variable.
#'
#' This test produces both chi-square and \emph{F}-tests; their calculation is described
#' in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-WaldTest.pdf}{\emph{Methods and Overview of Using EdSurvey for Running Wald Tests}}.
#'
#' @return
#' An \code{edsurveyWaldTest} object with the following elements:
#'    \item{Sigma}{coefficient covariance matrix}
#'    \item{coefficients}{indices of the coefficients tested}
#'    \item{H0}{null hypothesis values of coefficients tested}
#'    \item{result}{result object containing the values of the chi-square and \emph{F}-tests}
#'    \item{hypoMatrix}{hypothesis matrix used for the Wald Test}
#'
#' @references
#'
#' Diggle, P. J., Liang, K.-Y., & Zeger, S. L. (1994). \emph{Analysis of longitudinal data}. Oxford, UK: Clarendon Press.
#'
#' Draper, N. R., & Smith, H. (1998). \emph{Applied regression analysis}. New York, NY: Wiley.
#'
#' Fox, J. (1997). \emph{Applied regression analysis, linear models, and related methods}. Thousand Oaks, CA: SAGE.
#'
#' [Institute for Digital Research and Education. (n.d.). FAQ: How are the likelihood ratio, Wald, and LaGrange multiplier (score) tests different and/or similar?](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/). Los Angeles: University of California at Los Angeles. Retrieved from [https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/)
#'
#' Korn, E., & Graubard, B. (1990). Simultaneous testing of regression coefficients with complex survey data: Use of Bonferroni t statistics. \emph{The American Statistician}, \emph{44}(4), 270--276.
#'
#' @example man/examples/waldTest.R
#' @author Alex Lishinski and Paul Bailey
#'
#' @importFrom stats pchisq pf
#' @export
waldTest <- function(model, coefficients, H0 = NULL) {
  if (inherits(model, c("edsurveyLmList"))) {
    res <- lapply(model, waldTest, coefficients = coefficients, H0 = H0)
    class(res) <- "edsurveyWaldList"
    return(res)
  } else {
    return(calc.waldTest(model = model, coefficients = coefficients, H0 = H0))
  }
}

calc.waldTest <- function(model, coefficients, H0 = NULL) {
  possibleClasses <- c("edsurveyGlm", "edsurveyLm", "edsurveyRq", "mixedSdfResults", "mml.sdf", "summary.mml.sdf")
  if (!(any(unlist(lapply(possibleClasses, function(x) {
    inherits(model, x)
  }))))) {
    stop(paste0(sQuote("model"), " must be of class ", pasteItems(dQuote(possibleClasses), "or"), "."))
  }

  if (!"waldDenomBaseDof" %in% names(model) & (inherits(model, "edsurveyGlm") | inherits(model, "edsurveyLm"))) {
    warning("return value ", sQuote("waldDenomBaseDof"), " not on model. Try including the PSU and stratum variables on the data before running the model. Some public uses files have these suppressed to protect confidentiality, for example, by setting these columns to NA. When this happens, it prevents the accurate calculation of a Wald test statistic.")
  }
  # pull the coefficients and cov matrix from model object
  b <- coef(model)
  V <- vcov(model)
  n <- names(coef(model))
  # resolve coefficient names to position if names provided
  if (is.character(coefficients)) {
    if (length(coefficients) == 1) {
      if (!any(grepl(coefficients, n))) {
        stop(paste0("Variable ", dQuote(coefficients), " not found."))
      } else {
        coefficients <- grep(coefficients, n)
      }
    } else if (any(!(coefficients %in% n))) {
      stop(paste0("Variable(s) ", pasteItems(dQuote(coefficients[!(coefficients %in% n)])), " not found."))
    } else {
      coefficients <- unlist(lapply(coefficients, FUN = grep, n))
    }
  }

  w <- length(coefficients)

  # set default H0
  if (is.null(H0)) {
    H0 <- rep(0, w)
  }

  # check length of H0 against coefficients. repeat if 1 value provided, otherwise return error if not the same length
  if (length(H0) == 1) {
    H0 <- rep(H0, length(coefficients))
  }
  if (length(H0) != length(coefficients)) {
    stop("H0 must be same length as coefficients.")
  }

  # initialize rectangular matrix
  L <- matrix(0, ncol = length(b), nrow = w)

  # set coefficients
  for (i in 1:w) {
    L[i, coefficients[i]] <- 1
  }

  # set matrix names to coefficient names
  dimnames(L) <- list(paste("L", as.character(seq(NROW(L))), sep = ""), names(b))

  # Wald test matrix algebra
  f <- L %*% b
  mat <- qr.solve(L %*% V %*% t(L)) # stable inversion of the matrix
  stat <- t(f - H0) %*% mat %*% (f - H0)

  # calculate chi-square p-value
  p <- 1 - pchisq(stat, df = w)

  res <- list(chi2 = c(chi2 = stat, df = w, P = p))
  if (!is.null(model$waldDenomBaseDof)) {
    if (model$waldDenomBaseDof %in% "JK1") {
      # JK1, or the sample is an SRS, do the non-survey Wald test
      df1 <- nrow(L)
      df2 <- model$residual.df[1]
      fstat <- stat / df1
      res <- c(res, list(Ftest = c(Fstat = fstat, df1 = df1, df2 = df2, P = 1 - pf(fstat, df1, df2))))
    } else {
      # survey Wald test
      df1 <- nrow(L)
      df2 <- model$waldDenomBaseDof - df1
      fstat <- df2 * stat / (df1 * (model$waldDenomBaseDof - 1)) # denom doesn't get + 1
      if (df2 > 0) {
        res <- c(res, list(Ftest = c(Fstat = fstat, df1 = df1, df2 = df2, P = 1 - pf(fstat, df1, df2))))
      } else {
        if ("varMethod" %in% names(model) & "Taylor" %in% model$varMethod) {
          res <- c(res, list(FtMessage = "Too many coefficients  to calculate an F-statistic. Using jackknife variance estimation may resolve this."))
        } else {
          if (model$waldDenomBaseDof == 1) {
            res <- c(res, list(FtMessage = "Insufficient number of PSUs to calculate an F-statistic."))
          } else {
            res <- c(res, list(FtMessage = "Too many coefficients to calculate an F-statistic."))
          }
        }
      }
    }
  }

  # set output object with chi-square and F test results

  # create output
  structure(list(Sigma = V, b = b, coefficients = coefficients, H0 = H0, result = res, hypoMatrix = L),
    class = "edsurveyWaldTest"
  )
}

# helper function used to calculate the Wald test dof according to
# Korn, E. & Graubard, B. "Simultaneous testing of regression coefficients with complex survey data: Use of Bonferroni t statistics" The American Statistician, 44 (4) 270--276.
# used by glm.sdf, lm.sdf to get the denominator dof base used in waldTest
# @param data a data.frame with stratumVar, psuVar on it
# @param stratumVar the stratum variable
# @param psuVar the primary sampling unit (PSU) variable
# @return
# an integer that is the denominator dof (less subtracting the number of coefficients)
waldDof <- function(data, stratumVar, psuVar) {
  if ("JK1" %in% stratumVar) {
    return("JK1")
  }
  if (!inherits(data, "data.frame")) {
    stop("Data must be a data.frame.")
  }
  if (!is.null(stratumVar) && !stratumVar %in% colnames(data)) {
    stop("Data must have stratumVar ", sQuote(stratumVar), " on it.")
  }
  if (!is.null(psuVar) && !psuVar %in% colnames(data)) {
    stop("Data must have psuVar ", sQuote(psuVar), " on it.")
  }
  # return the number of unique PSU/strata, with no more than minN per stratum.
  uniqueMin <- function(df, minN = 2) {
    tab <- table(df)
    sum(apply(tab, 1, function(x) {
      min(minN, length(x[x > 0]))
    }))
  }
  return(uniqueMin(data[ , c(stratumVar, psuVar)]) - uniqueMin(data[ , stratumVar, drop = FALSE]) + 1)
}


#' @method print edsurveyWaldTest
#' @export
print.edsurveyWaldTest <- function(x, digits = 2, ...) {
  coefficients <- x$coefficients
  b <- x$b
  H0 <- x$H0
  v <- x$result$chi2
  df <- x$df
  coef_names <- names(x$b)[x$coefficients]

  eout("Wald test:\n")
  eout("----------\n")
  eout("H0:")
  for (i in 1:length(coef_names)) {
    eout(paste0(coef_names[i], " = ", H0[i], "\n"))
  }
  eout("\n")
  eout("Chi-square test:\n")
  eout(paste0(
    "X2 = ", format(v["chi2"], digits = digits, nsmall = 1), ", df = ", v["df"],
    ", P(> X2) = ", format(v["P"], digits = digits, nsmall = 1), "\n"
  ))

  if (!is.null(x$result$Ftest)) {
    v <- x$result$Ftest
    eout("\n")
    eout("F test:\n")
    eout(paste0(
      "W = ", format(v["Fstat"], digits = digits, nsmall = 1),
      ", df1 = ", v["df1"],
      ", df2 = ", v["df2"],
      ", P(> W) = ", format(v["P"], digits = digits), "\n"
    ))
  } else {
    if (!is.null(x$result$FtMessage)) {
      eout("\n")
      eout(paste0("NOTE: ", x$result$FtMessage))
    }
  }
}
