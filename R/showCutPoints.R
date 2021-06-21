#' @title Retrieve Achievement Level Cutpoints
#'
#' @description Retrieves a summary of the achievement level cutpoints for a
#'              selected study represented in an
#'              \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, 
#'              or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\showCutPoints.R
#' @export
showCutPoints <- function(data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }

  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  cat(paste0("Achievement Levels:\n"))
  als <- getAttributes(data, "achievementLevels")
  if (length(als) > 0) {
    for(i in 1:length(als)) {
      cat(paste0("  ", names(als)[i], ":  ", paste(unname(als[[i]]),collapse=", "), "\n"))
    }
  } else {
    # paste a statement if there are no achievement levels in the sdf
    cat(paste0("  No achievement levels.\n"))
  }
}
