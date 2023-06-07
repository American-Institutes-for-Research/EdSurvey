#' @importFrom LaF laf_open_fwf laf_open_csv close ncol nrow
#' @importFrom lifecycle deprecated
#' @import Matrix
#' @import methods
#' @import lfactors
#' @import NAEPprimer

setGeneric("coef")
setGeneric("subset")

setOldClass("edsurvey.data.frame")
setOldClass("light.edsurvey.data.frame")
setOldClass("edsurvey.data.frame.list")

setAs("light.edsurvey.data.frame", "data.frame", function(from) {
  as.data.frame.light.edsurvey.data.frame(from)
})

setOldClass("edsurveyLm")
setOldClass("edsurveyLmList")
setOldClass("edsurveyGlm")
setOldClass("edsurveyGlmList")

# stub
writeNAEP_XML <- function(fileContents) {
  stop("remove writeNAEP_XML from all.R")
}

parseNAEP_XML <- function(filename) {
  stop("remove parseNAEP_XML from all.R")
}
