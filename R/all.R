#' @importFrom LaF laf_open_fwf laf_open_csv close ncol nrow
#' @import Matrix
#' @import methods
#' @import lfactors
#' @import NAEPprimer

setGeneric("glm")
setGeneric("lm")
setGeneric("coef")
setGeneric("subset")

setOldClass("edsurveyLm")
setOldClass("edsurveyLmList")
setOldClass("edsurveyGlm")
setOldClass("edsurveyGlmList")

setOldClass("edsurvey.data.frame")
setOldClass("light.edsurvey.data.frame")
setOldClass("edsurvey.data.frame.list")
setClassUnion("sdf", members = c("edsurvey.data.frame","edsurvey.data.frame.list","light.edsurvey.data.frame"))
## as(some.light.edsurvey.data.frame)
setAs("light.edsurvey.data.frame","data.frame", function(from) {
  as.data.frame.light.edsurvey.data.frame(from)
})
