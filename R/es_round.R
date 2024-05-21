#' @title round ouptut of EdSurvey functions before printing
#'
#' @description Applies rounding rules
#'
#' @param object the object (usually the result of an analysis function) to be rounded
#' @param round_n function used to round sample n-sizes
#' @param round_pop_n function used to round weighted n-sizes, these are also called population size estimates
#' @param round_est function used to round estimates; examples include means and percentiles of scores, as well as regression coefficients
#' @param round_est_se function used to round standard errors of estimates
#' @param round_pct function used to round percentages
#' @param round_pct_se function used to round the standard errors of percentages
#' @param round_specific_element a list of rounding functions, the function is applied to elements with that name. See Examples
#' @param ... additional arguments passed to methods
#' 
#' rounds every statistic that is a function of data, including the header and tables
#'
#' @return the object is returned, with relevant elements rounded
#'
#' @author Paul Bailey
#' @example man/examples/es_round.R
#' @export
es_round <- function(object,
                     round_n = getOption("EdSurvey_round_n_function"),
                     round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                     round_est = getOption("EdSurvey_round_est_function"),
                     round_est_se = getOption("EdSurvey_round_est_se_function"),
                     round_pct = getOption("EdSurvey_round_pct_function"),
                     round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                     round_specific_element=NULL,
                     ...) {
  UseMethod("es_round")
}

#' @author Paul Bailey
#' @export
es_round.gapList <- function(object,
                     round_n = getOption("EdSurvey_round_n_function"),
                     round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                     round_est = getOption("EdSurvey_round_est_function"),
                     round_est_se = getOption("EdSurvey_round_est_se_function"),
                     round_pct = getOption("EdSurvey_round_pct_function"),
                     round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                     round_specific_element=NULL,
                     ...) {
  es_round.gap(object=object,
               round_n = round_n,
               round_pop_n = round_pop_n,
               round_pct = round_pct,
               round_pct_se = round_pct_se,
               round_specific_element=round_specific_element,
               ...)
}

#' @export
es_round.percentile <- function(object,
                                round_n = getOption("EdSurvey_round_n_function"),
                                round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                round_est = getOption("EdSurvey_round_est_function"),
                                round_est_se = getOption("EdSurvey_round_est_se_function"),
                                round_pct = getOption("EdSurvey_round_pct_function"),
                                round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                round_specific_element=NULL,
                                ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object <- es_round_with(object, "estimate", round_est, round_specific_element, ...)
  object <- es_round_with(object, "se", round_est_se, round_specific_element, ...)
  object <- es_round_with(object, "df", round_est, round_specific_element, ...)
  object <- es_round_with(object, "confInt.ci_lower", round_est, round_specific_element, ...)
  object <- es_round_with(object, "confInt.ci_upper", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.gap <- function(object,
                         round_n = getOption("EdSurvey_round_n_function"),
                         round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                         round_est = getOption("EdSurvey_round_est_function"),
                         round_est_se = getOption("EdSurvey_round_est_se_function"),
                         round_pct = getOption("EdSurvey_round_pct_function"),
                         round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                         round_specific_element=NULL,
                         ...) {
  object$labels <- es_round_with(object$labels, "n0A", round_n, round_specific_element, ...)
  object$labels <- es_round_with(object$labels, "n0B", round_n, round_specific_element, ...)
  object$labels <- es_round_with(object$labels, "nUsedA", round_n, round_specific_element, ...)
  object$labels <- es_round_with(object$labels, "nUsedB", round_n, round_specific_element, ...)
  if("percentage" %in% names(object)) {
    object$percentage <- es_round_with(object$percentage, "pctA", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "pctB", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffAA", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffBB", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffAB", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffABAB", round_pct, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "pctAse", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "pctBse", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "covAA", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "covBB", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "covAB", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "covABAB", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffAAse", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffBBse", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffABse", round_pct_se, round_specific_element, ...)
    object$percentage <- es_round_with(object$percentage, "diffABABse", round_pct_se, round_specific_element, ...)
  }
  
  if("results" %in% names(object)) {
    object$results <- es_round_with(object$results, "estimateA", round_est, round_specific_element, ...)
    object$results <- es_round_with(object$results, "estimateAse", round_est_se, round_specific_element, ...)
    object$results <- es_round_with(object$results, "estimateB", round_est, round_specific_element, ...)
    object$results <- es_round_with(object$results, "estimateBse", round_est_se, round_specific_element, ...)
    object$results <- es_round_with(object$results, "diffAB", round_est, round_specific_element, ...)
    object$results <- es_round_with(object$results, "covAB", round_est, round_specific_element, ...)
    object$results <- es_round_with(object$results, "diffABse", round_est_se, round_specific_element, ...)
    object$results <- es_round_with(object$results, "dofAB", round_est, round_specific_element, ...)
  }
  return(object)
}

#'@export
#' @author Paul Bailey
es_round.edsurveyLm <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "coef", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.edsurveyLm <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "coef", round_est, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "se", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "t", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "dof", round_est, round_specific_element, ...)
  if("stdCoef" %in% names(object$coefmat)) {
    object$coefmat <- es_round_with(object$coefmat, "stdCoef", round_est, round_specific_element, ...)
    object$coefmat <- es_round_with(object$coefmat, "stdSE", round_est_se, round_specific_element, ...)
  }
  return(object)
}


#' @author Paul Bailey
#' @export
es_round.summary2 <- function(object,
                              round_n = getOption("EdSurvey_round_n_function"),
                              round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                              round_est = getOption("EdSurvey_round_est_function"),
                              round_est_se = getOption("EdSurvey_round_est_se_function"),
                              round_pct = getOption("EdSurvey_round_pct_function"),
                              round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                              round_specific_element=NULL,
                              ...) {
  object <- es_round_with(object, "N", round_pop_n, round_specific_element, ...)
  object$summary <- es_round_with(object$summary, "N", round_pop_n, round_specific_element, ...)
  object$summary <- es_round_with(object$summary, "Weighted Percent", round_pct, round_specific_element, ...)
  object$summary <- es_round_with(object$summary, "Weighted Percent SE", round_pct_se, round_specific_element, ...)
  object$summary <- es_round_with(object$summary, "Weighted N", round_pop_n, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.levelsSDF <- function(object,
                               round_n = getOption("EdSurvey_round_n_function"),
                               round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                               round_est = getOption("EdSurvey_round_est_function"),
                               round_est_se = getOption("EdSurvey_round_est_se_function"),
                               round_pct = getOption("EdSurvey_round_pct_function"),
                               round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                               round_specific_element=NULL,
                               ...) {
  lapply(object, es_round_with, name="n", round_function=round_pop_n, round_specific_element=round_specific_element, ...)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyTable <- function(object,
                                   round_n = getOption("EdSurvey_round_n_function"),
                                   round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                   round_est = getOption("EdSurvey_round_est_function"),
                                   round_est_se = getOption("EdSurvey_round_est_se_function"),
                                   round_pct = getOption("EdSurvey_round_pct_function"),
                                   round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                   round_specific_element=NULL,
                                   ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object$data <- es_round_with(object$data, "N", round_n, round_specific_element, ...)
  object$data <- es_round_with(object$data, "WTD_N", round_pop_n, round_specific_element, ...)
  object$data <- es_round_with(object$data, "PCT", round_pct, round_specific_element, ...)
  object$data <- es_round_with(object$data, "SE(PCT)", round_pct_se, round_specific_element, ...)
  object$data <- es_round_with(object$data, "MEAN", round_est, round_specific_element, ...)
  object$data <- es_round_with(object$data, "SE(MEAN)", round_est_se, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.achievementLevels <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object$discrete <- es_round_with(object$discrete, "N", round_n, round_specific_element, ...)
  object$discrete <- es_round_with(object$discrete, "wtdN", round_pop_n, round_specific_element, ...)
  object$discrete <- es_round_with(object$discrete, "Percent", round_pct, round_specific_element, ...)
  object$discrete <- es_round_with(object$discrete, "StandardError", round_pct_se, round_specific_element, ...)
  if("cumulative" %in% names(object)) {
    object$cumulative <- es_round_with(object$cumulative, "N", round_n, round_specific_element, ...)
    object$cumulative <- es_round_with(object$cumulative, "wtdN", round_pop_n, round_specific_element, ...)
    object$cumulative <- es_round_with(object$cumulative, "Percent", round_pct, round_specific_element, ...)
    object$cumulative <- es_round_with(object$cumulative, "StandardError", round_pct_se, round_specific_element, ...)
  }
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyCor <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object <- es_round_with(object, "correlation", round_est, round_specific_element, ...)
  object <- es_round_with(object, "se", round_est_se, round_specific_element, ...)
  object <- es_round_with(object, "confidenceInterval", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyGlm <- function(object,
                                 round_n = getOption("EdSurvey_round_n_function"),
                                 round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                 round_est = getOption("EdSurvey_round_est_function"),
                                 round_est_se = getOption("EdSurvey_round_est_se_function"),
                                 round_pct = getOption("EdSurvey_round_pct_function"),
                                 round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                 round_specific_element=NULL,
                                 ...) {
  object$coefmat <- es_round_with(object$coefmat, "coef", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.edsurveyGlm <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "coef", round_est, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "se", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "t", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "dof", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyWaldTest <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "b", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.mml.sdf <- function(object,
                             round_n = getOption("EdSurvey_round_n_function"),
                             round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                             round_est = getOption("EdSurvey_round_est_function"),
                             round_est_se = getOption("EdSurvey_round_est_se_function"),
                             round_pct = getOption("EdSurvey_round_pct_function"),
                             round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                             round_specific_element=NULL,
                             ...) {
  object$mml <- es_round_with(object$mml, round_est, round_specific_element, ...) 
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.mml.sdf <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object$Summary <- es_round_with(object$Summary, "obs", round_n, round_specific_element, ...)
  object$Summary <- es_round_with(object$Summary, "weightedObs", round_pop_n, round_specific_element, ...)
  object$Summary$coefficients <- es_round_with(object$Summary$coefficients, 1, round_est, round_specific_element, ...)
  object$Summary$coefficients <- es_round_with(object$Summary$coefficients, 2, round_est_se, round_specific_element, ...)
  object$Summary$coefficients <- es_round_with(object$Summary$coefficients, 3, round_est_se, round_specific_element, ...)
  object$Summary$coefficients <- es_round_with(object$Summary$coefficients, 4, round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.mixedSdfResults <- function(object,
                                             round_n = getOption("EdSurvey_round_n_function"),
                                             round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                             round_est = getOption("EdSurvey_round_est_function"),
                                             round_est_se = getOption("EdSurvey_round_est_se_function"),
                                             round_pct = getOption("EdSurvey_round_pct_function"),
                                             round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                             round_specific_element=NULL,
                                             ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  
  object$vars <- es_round_with(object$vars, "vcov", round_est, round_specific_element, ...)
  object$vars <- es_round_with(object$vars, "ngrp", round_est, round_specific_element, ...)
  object$vars <- es_round_with(object$vars, "level", round_est, round_specific_element, ...)
  
  object$ngroups <- es_round_with(object$ngroups, "Observations", round_n, round_specific_element, ...)
  
  object$groupSum <- es_round_with(object$groupSum, "n size", round_n, round_specific_element, ...)
  object$groupSum <- es_round_with(object$groupSum, "mean wgt", round_est, round_specific_element, ...)
  object$groupSum <- es_round_with(object$groupSum, "sum wgt", round_est, round_specific_element, ...)
  
  object$varsmatSum <- es_round_with(object$varsmatSum, "Variance", round_est, round_specific_element, ...)
  object$varsmatSum <- es_round_with(object$varsmatSum, "Std. Error", round_est_se, round_specific_element, ...)
  object$varsmatSum <- es_round_with(object$varsmatSum, "Std.Dev.", round_est_se, round_specific_element, ...)
  
  object <- es_round_with(object, "coef", round_est, round_specific_element, ...)
  object <- es_round_with(object, "ICC", round_est, round_specific_element, ...)
  
  for(i in 1:length(object$wgtStats)) {
    object$wgtStats[[i]] <- es_round_with(object$wgtStats[[i]], "mean", round_est, round_specific_element, ...)
    object$wgtStats[[i]] <- es_round_with(object$wgtStats[[i]], "sum", round_est, round_specific_element, ...)
    object$wgtStats[[i]] <- es_round_with(object$wgtStats[[i]], "min", round_est, round_specific_element, ...)
    object$wgtStats[[i]] <- es_round_with(object$wgtStats[[i]], "max", round_est, round_specific_element, ...)
  }
  
  for(name in names(object$ranefMat)) {
    object$ranefMat <- es_round_with(object$ranefMat, name, round_est, round_specific_element, ...)
  }
  
  object <- es_round_with(object, "resid", round_est, round_specific_element, ...)
  
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.mixedSdfResults <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  object$groupSum <- es_round_with(object$groupSum, "n size", round_n, round_specific_element, ...)
  object$groupSum <- es_round_with(object$groupSum, "mean wgt", round_n, round_specific_element, ...)
  object$groupSum <- es_round_with(object$groupSum, "sum wgt", round_n, round_specific_element, ...)
  
  object$varsmatSum <- es_round_with(object$varsmatSum, "Variance", round_est, round_specific_element, ...)
  object$varsmatSum <- es_round_with(object$varsmatSum, "Std. Error", round_est_se, round_specific_element, ...)
  object$varsmatSum <- es_round_with(object$varsmatSum, "Std.Dev.", round_est_se, round_specific_element, ...)
  
  object$coef <- es_round_with(object$coef, "Estimate", round_est, round_specific_element, ...)
  object$coef <- es_round_with(object$coef, "Std. Error", round_est_se, round_specific_element, ...)
  object$coef <- es_round_with(object$coef, "t value", round_est_se, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyMvrlm <- function(object,
                                   round_n = getOption("EdSurvey_round_n_function"),
                                   round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                   round_est = getOption("EdSurvey_round_est_function"),
                                   round_est_se = getOption("EdSurvey_round_est_se_function"),
                                   round_pct = getOption("EdSurvey_round_pct_function"),
                                   round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                   round_specific_element=NULL,
                                   ...) {
  object <- es_round_with(object, "coef", round_est, round_specific_element, ...)
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.edsurveyMvrlm <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  
  for(construct in names(object$coefmat)) {
    object$coefmat[[construct]] <- es_round_with(object$coefmat[[construct]], 
                                                 "coef", round_est, round_specific_element, ...)
    object$coefmat[[construct]] <- es_round_with(object$coefmat[[construct]], 
                                                 "se", round_est_se, round_specific_element, ...)
    object$coefmat[[construct]] <- es_round_with(object$coefmat[[construct]], 
                                                 "t", round_est_se, round_specific_element, ...)
    object$coefmat[[construct]] <- es_round_with(object$coefmat[[construct]], 
                                                 "dof", round_est, round_specific_element, ...)
  }
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.edsurveyRq <- function(object,
                                        round_n = getOption("EdSurvey_round_n_function"),
                                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                                        round_est = getOption("EdSurvey_round_est_function"),
                                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                                        round_pct = getOption("EdSurvey_round_pct_function"),
                                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                                        round_specific_element=NULL,
                                        ...) {
  object <- es_round_with(object, "coef", round_est, round_specific_element, ...)
  
  return(object)
}

#' @author Paul Bailey
#' @export
es_round.summary.edsurveyRq <- function(object,
                        round_n = getOption("EdSurvey_round_n_function"),
                        round_pop_n = getOption("EdSurvey_round_pop_n_function"),
                        round_est = getOption("EdSurvey_round_est_function"),
                        round_est_se = getOption("EdSurvey_round_est_se_function"),
                        round_pct = getOption("EdSurvey_round_pct_function"),
                        round_pct_se = getOption("EdSurvey_round_pct_se_function"),
                        round_specific_element=NULL,
                        ...) {
  object <- es_round_with(object, "n0", round_n, round_specific_element, ...)
  object <- es_round_with(object, "nUsed", round_n, round_specific_element, ...)
  
  object$coefmat <- es_round_with(object$coefmat, "coef", round_est, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "se", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "t", round_est_se, round_specific_element, ...)
  object$coefmat <- es_round_with(object$coefmat, "dof", round_est, round_specific_element, ...)
  
  return(object)
}

# rounds an object component
# @param object the boject to round
# @param name the name of the element in object to round
# @param round_function the function to round with
# @param round_specific_element if the \code{name} is on \code{round_specific_element}, then \code{es_round_with} will instead use the function in that element for rouding
# @return returns the object, with the element \code{name} rounded
# @author Paul Bailey
es_round_with <- function(object, name, round_function, round_specific_element=NULL, ...) {
  attr <- FALSE
  num <- FALSE
  # if name is a number, it's a column index
  if(!inherits(name,"numeric")) {
    # if name isn't an index and it isn't present in the object
    # or object attributes, just return the object
    if(!name %in% names(object)) {
      if(!name %in% names(attributes(object))) {
        return(object)
      }else {
        attr <- TRUE
      }
    }
  }else {
    num <- TRUE
  }
  # check for specific instructions about this column
  if(name %in% names(round_specific_element)) {
    round_function <- round_specific_element[[name]]
  }
  # check that the function we are about to apply is, in fact, a function
  if(!inherits(round_function, "function")) {
    stop(paste0("the rounding function must be a function. Found a ", paste(dQuote(class(round_function)), collapse= ", "), " instead"))
  }
  if(attr) {
    attributes(object)[[name]] <- round_function(attributes(object)[[name]], ...)
  }else if(num) {
    object[,name] <- round_function(object[,name], ...)
  }else {
    object[[name]] <- round_function(object[[name]], ...)
  }
  return(object)
}

#'@title rounding helper for NCES
#'@param n the value to be rounded; accepts a vector
#'@return the rounded value
#'@author Paul Bailey
#'@export
roundNCES <- function(n) {
  n <- ifelse(n >= 100000, round(n, -3), n)
  n <- ifelse(n >= 1000, round(n, -2), n)
  n <- ifelse(n >= 100, round(n, -1), n)
  n <- ifelse(n <=62, 0, n)
  n <- round(n, -1)
  return(n)
}

#' @title rounding helper
#'
#' @param n round to this level
#'
#' @return a function that rounds to n
#' @author Paul Bailey
#' @export
roundn <- function(n) {
  fn <- function(x) {
    round(x, n)
  }
  return(fn) 
}

