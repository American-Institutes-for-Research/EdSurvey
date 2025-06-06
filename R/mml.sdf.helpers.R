#' @method summary mml.sdf
#' @export
summary.mml.sdf <- function(object, gradientHessian = TRUE,
                            varType = c("Taylor"),
                            strataVar = NULL, PSUVar = NULL, singletonFix = c("use mean", "drop"), ...) {
  x <- object
  # sunmary
  summary <- summary(
    object=x$mml,
    varType=varType,
    strataVar=strataVar, PSUVar=PSUVar, singletonFix=match.arg(singletonFix), ...
  )
  # get calls
  summaryCall <- match.call()
  call <- x$Call
  # item score dictionary
  scoreDict <- x$scoreDict
  return(structure(
    list(
      "Call" = call,
      "Summary Call" = summaryCall,
      "Summary" = summary,
      "scoreDict" = scoreDict,
      object = x
    ),
    class = "summary.mml.sdf"
  ))
}

#' @method coef mml.sdf
#' @export
coef.mml.sdf <- function(object, ...) {
  coef(object$mml)
}

#' @method print mml.sdf
#' @export
print.mml.sdf <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  co <- coef(x)
  print(co, ...)
}

#' @method print summary.mml.sdf
#' @export
print.summary.mml.sdf <- function(x, use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  cat(paste0("Call:\n"))
  print(x$Call)
  cat(paste0("Summary Call:\n"))
  print(x$`Summary Call`)
  cat("\n")
  cat("Summary:\n")
  cof <- x$Summary$coefficients
  cof1 <- cof[1:(nrow(cof) - 1), , drop = FALSE]
  cof2 <- cof[nrow(cof), 1:2, drop = FALSE]
  printCoefmat(cof1)
  cat("\n")
  cat("Residual Variance Estimate:\n")
  print(cof2)
  cat("\n")
  if (length(unique(x$Summary$Convergence)) == 1) {
    cat(paste0("Convergence = ", unique(x$Summary$Convergence), "\n"))
  } else {
    cat(paste0("Convergence = ", paste(x$Summary$Convergence, collapse = ", "), "\n"))
  }
  if("iterations" %in% names(x$Summary) && all(!is.na(x$Summary$iterations)) && all(x$Summary$iterations != -1)) {
    cat(paste0("Iterations = ", paste(x$Summary$iterations, collapse = ", "), "\n"))
  }
  if ("LogLik" %in% names(x$Summary)) {
    cat(paste0("LogLike = ", paste(round(x$Summary$LogLik, 2), collapse = ", "), "\n"))
  }
  cat(paste0("Observations = ", paste(x$Summary$obs, collapse = ", "), "\n"))
  if (any(!is.na(x$Summary$weightedObs))) {
    cat(paste0("Weighted observations = ", paste(round(x$Summary$weightedObs, 2), collapse = ", "), "\n"))
  }
}

#' @method vcov mml.sdf
#' @export
vcov.mml.sdf <- function(object, ...) {
  vcov(object$mml)
}
