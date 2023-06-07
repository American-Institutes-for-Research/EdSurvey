#' @title Degrees of Freedom
#'
#' @description Calculates the degrees of freedom for a statistic
#'              (or of a contrast between two statistics) based on the
#'              jackknife and imputation variance estimates.
#' @param varEstA the \code{varEstInput} object returned from certain functions, such as
#'                \code{\link{lm.sdf}} when \code{returnVarEstInputs=}\code{TRUE}).
#'                The variable \code{varA} must be on this dataset.
#'                See Examples.
#' @param varEstB similar to the \code{varEstA} argument.
#'                If left blank, both are assumed to come
#'                from \code{varEstA}. When set, the degrees of freedom
#'                are for a contrast between \code{varA} and \code{varB},
#'                and the \code{varB} values are taken from \code{varEstB}.
#' @param varA a character that names the statistic in the \code{varEstA}
#'             argument for which the degrees of freedom calculation is required.
#' @param varB a character that names the statistic in the \code{varEstB}
#'             argument for which a covariance is required. When \code{varB}
#'             is specified, returns the degrees of freedom for
#'             the contrast between \code{varA} and \code{varB}.
#' @param method a character that is either \code{WS} for the Welch-Satterthwaite
#'               formula or
#'               \code{JR} for the Johnson-Rust correction to the
#'               Welch-Satterthwaite formula
#'
#' @details
#' This calculation happens under the notion that statistics
#' have little variance within strata, and
#' some strata will contribute fewer than a full degree of freedom.
#'
#' The functions are not vectorized, so both \code{varA} and
#' \code{varB} must contain exactly one variable name.
#'
#' The method used to compute the degrees of freedom is in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#' section \dQuote{Estimation of Degrees of Freedom.}
#'
#' @return
#' numeric; the estimated degrees of freedom
#'
#' @references Johnson, E. G., & Rust, K. F. (1992). Population inferences and variance estimation for NAEP data. \emph{Journal of Educational Statistics,} \emph{17,} 175--190.
#' @example man/examples/DoFCorrection.R
#' @author Paul Bailey
#' @export
DoFCorrection <- function(varEstA, varEstB = varEstA, varA, varB = varA, method = c("WS", "JR")) {
  # grab just first element of method so we have only one method we are using
  method <- match.arg(method)
  if (!varA %in% unique(varEstA$JK$variable)) {
    stop(paste0("Could not find ", sQuote("varA"), " value of ", dQuote(varA), " in varEstA$JK$variable. Existing values include ", pasteItems(dQuote(unique(varEstA$JK$variable))), "."))
  }
  if (!varB %in% unique(varEstB$JK$variable)) {
    stop(paste0("Could not find ", sQuote("varB"), " value of ", dQuote(varB), " in varEstB$JK$variable. Existing values include ", pasteItems(dQuote(unique(varEstB$JK$variable))), "."))
  }
  if (!is.null(varEstA$PV) & !is.null(varEstB$PV)) {
    if (!varA %in% unique(varEstA$PV$variable)) {
      stop(paste0("Could not find ", sQuote("varA"), " value of ", dQuote(varA), " in varEstA$PV$variable. Existing values include ", pasteItems(dQuote(unique(varEstA$PV$variable))), "."))
    }
    if (!varB %in% unique(varEstB$PV$variable)) {
      stop(paste0("Could not find ", sQuote("varB"), " value of ", dQuote(varB), " in varEstB$PV$variable. Existing values include ", pasteItems(dQuote(unique(varEstB$PV$variable))), "."))
    }
  }
  mergeVar <- c("PV", "JKreplicate")
  if (any(!mergeVar %in% names(varEstA$JK))) {
    stop(paste0("Merge vars missing from varEstA$JK ", pasteItems(dQuote(mergeVar[!mergeVar %in% names(varEstA$JK)])), "."))
  }
  if (any(!mergeVar %in% names(varEstB$JK))) {
    stop(paste0("Merge vars missing from varEstB$JK ", pasteItems(dQuote(mergeVar[!mergeVar %in% names(varEstB$JK)])), "."))
  }
  if (all.equal(varEstA, varEstB)[[1]] == TRUE & all.equal(varA, varB) == TRUE) {
    # the variable is already the difference (e.g. A - B)
    JK <- subset(varEstA$JK, variable == varA)
    JK$cov <- (JK$value)^2
  } else {
    # the difference must be calculted
    JK <- merge(subset(varEstA$JK, variable == varA),
      subset(varEstB$JK, variable == varB),
      by = mergeVar, all.x = FALSE, all.y = FALSE,
      suffixes = c(".A", ".B")
    )
    if (nrow(JK) < 1) {
      stop("Could not find appropriate values in JK results to calculate covariance.")
    }
    JK$cov <- (JK$value.A - JK$value.B)^2
  }
  # remove rows where it is not possible to calcualte the statistic
  JK <- JK[!is.na(JK$cov), ]

  # the results will be NA if the denominator is zero
  if (!any(abs(JK$cov) > 0, na.rm = TRUE)) {
    # there is no variance, which means zero degress of freedom
    return(0)
  }
  mStar <- (1 / length(unique(JK$PV)))
  res <- mStar * (sum(JK$cov)^2 / sum(JK$cov^2))
  if (method == "JR") {
    m <- nrow(JK) / length(unique(JK$PV))
    # Johnson and Rust 1992
    res <- (3.16 - 2.77 / sqrt(m)) * res
  }
  return(res)
}
