# @author Paul Bailey
#' @method head edsurvey.data.frame
#' @export
head.edsurvey.data.frame <- function(x, ...) {
  edf <- getData(x,
    varnames = colnames(x),
    dropUnusedLevels = FALSE, omittedLevels = FALSE,
    addAttributes = FALSE
  )
  head(edf, ...)
}
