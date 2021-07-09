#' @method summary edSurveyMML
#' @export
summary.edSurveyMML <- function(object, gradientHessian=FALSE,
                                varType=c("consistent", "robust", "cluster", "replicate", "Taylor"),
                                clusterVar=NULL, jkSumMultiplier=1, # cluster
                                repWeight=NULL, # replicate
                                strataVar=NULL, PSUVar=NULL, singletonFix=c("drop", "use mean"),...){
  x <- object
  # sunmary 
  summary <- summary(x$mml, 
                     gradientHessian,
                     varType,
                     clusterVar, jkSumMultiplier, # cluster
                     repWeight, # replicate
                     strataVar, PSUVar, singletonFix, ...)
  # get calls
  summaryCall <- match.call()
  call <- x$Call
  # item mapping 
  mapping <- x$itemMapping 
  # item score dictionary  
  scoreDict <- x$scoreDict 
  return(structure(list("Call" = call, 
                        "Summary Call" = summaryCall,
                        "Summary" = summary,
                        "itemMapping"= mapping,
                        "scoreDict" = scoreDict,
                        object = x),
                   class="summary.edSurveyMML"))
} 

#' @method print edSurveyMML
#' @export
print.edSurveyMML <- function(x, ...){
  co <- coef(x$mml)
  print(co, ...)
}

#' @method print summary.edSurveyMML
#' @export
print.summary.edSurveyMML <- function(x, ...){
  cat(paste0("Call:\n"))
  print(x$Call)
  cat(paste0("Summary Call:\n"))
  print(x$`Summary Call`)
  cat("\n")
  cat("Summary:\n")
  cof <- x$Summary$coefficients
  cof1 <- cof[1:(nrow(cof)-1),,drop=FALSE]
  cof2 <- cof[nrow(cof),1:2,drop=FALSE]
  printCoefmat(cof1)
  cat("\n")
  cat("Residual Variance Estimate:\n")
  print(cof2)
  cat("\n")
  cat(paste0("Convergence = ", x$Summary$converged, "\n"))
  cat(paste0("Iterations = ", x$Summary$iterations, "\n"))
  cat(paste0("LogLike = ", round(x$Summary$LL,2), "\n"))
  cat(paste0("Observations = ", x$Summary$obs, "\n"))
  if(!is.na(x$Summary$weightedObs)) {
    cat(paste0("Weighted observations = ", round(x$Summary$weightedObs,2), "\n"))
  }
} 
