#' Draw plausible values from an mml fit
#' @param x a fit from a call to \code{\link{mml.sdf}}, or a \code{summary.mml.sdf}, which is a  \code{summary} of
#' \code{mml.sdf} call.
#' @param npv integer indicating the number of plausible values to draw
#' @param pvVariableNameSuffix suffix to append to the name of the new plausible values
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} to which the plausible values will be added and from which the covariates and item responses will be taken to generate them
#' @param stochasticBeta this is deprecated
#' @param construct the construct to draw PVs for
#' @param ... additional parameters
#' @importFrom Dire drawPVs
#' @export
drawPVs.sdf <- function(x, npv = 5L, pvVariableNameSuffix = "_dire",
                        stochasticBeta = deprecated(),
                        data,
                        construct = NULL, ...) {
  mml <- x
  if (lifecycle::is_present(stochasticBeta)) {
    lifecycle::deprecate_soft("4.0.4", "drawPVs.sdf(stochasticBeta)", details="the latent regression coefficients are always treated as stochastic now.")
  }
  checkDataClass(data, c("light.edsurvey.data.frame", "edsurvey.data.frame"))
  ### check data class
  checkDataClass(mml, c("summary.mml.sdf", "mml.sdf"))
  if(inherits(mml, "mml.sdf")) {
    mml <- summary(mml)
  }
  ### check for merge key
  idVar <- mml$Summary$idVar
  if (!idVar %in% colnames(data)) {
    stop(paste0("the ", dQuote("idVar"), " must be on ", dQuote("data"), " to merge on PVs. You must specify it when calling the mml.sdf function"))
  }

  # get mml data, depending whether a summary or mml object was given
  construct0 <- NULL
  theSubject <- mml$object$theSubject
  survey <- mml$object$survey
  scoreDict <- mml$object$scoreDict
  if ("formula" %in% names(mml$object$Call)) {
    construct0 <- mml$object$Call$formula[[2]]
  }
  # check that we have a construct
  if (missing(construct)) {
    if (!is.null(construct0)) {
      construct <- construct0
    } else {
      stop(paste0("Construct not assigned and not available from mml object. To name the construct you wish to draw PVs for, use the ", dQuote("construct"), " argument."))
    }
  }
  mml <- mml$Summary
  # draw plausible values
  pvs <- drawPVs(
    x = mml,
    npv = npv,
    ...
  )
  if (construct0 == "composite") {
    constructNames <- c(pvs$testScale$subtest, "composite")
  } else {
    constructNames <- construct0
  }
  finalNames <- paste0(constructNames, pvVariableNameSuffix)
  newPV <- lapply(finalNames, function(x) {
    return(list(varnames=paste0(x,1:npv)))
  })
  names(newPV) <- finalNames
  pvvars0 <- getAttributes(data, "pvvars")
  if (any(names(pvvars0) %in% names(newPV))) {
    warning("overwriting some constructs already on data")
  }
  pvvars0[names(newPV)] <- newPV
  # merge dispatches an edf differently from an lesdf
  data <- merge(data, pvs[,c(idVar,colnames(pvs)[!colnames(pvs) %in% colnames(data)])], by = idVar, all.x = TRUE, all.y = FALSE)
  data <- setAttributes(data, "pvvars", pvvars0)
  return(data)
}

#' @method drawPVs mml.sdf
#' @export
drawPVs.mml.sdf <- function(x,
                            npv = 5L,
                            pvVariableNameSuffix = "_dire",
                            stochasticBeta = deprecated(),
                            data, ...) {
  drawPVs.sdf(x = x, npv = npv, pvVariableNameSuffix = pvVariableNameSuffix, data = data, ...)
}

#' @method drawPVs summary.mml.sdf
#' @export
drawPVs.summary.mml.sdf <- function(x,
                                    npv = 5L,
                                    pvVariableNameSuffix = "_dire",
                                    stochasticBeta = deprecated(),
                                    data, ...) {
  drawPVs.sdf(x = x, npv = npv, pvVariableNameSuffix = pvVariableNameSuffix, data = data, ...)
}
