#' this just implements attach for an edsurvey.data.frame and a light.edusrvey.data.frame
#' @param what equivalent to `what` in base::attach, but can also be an edsurvey.data.frame
#' @param pos equivalent to `pos` in base::attach
#' @param name equivalent to `name` in base::attach
#' @param warn.conflicts equivalent to `warn.conflicts` in base::attach
#' @author Blue Webb
#' @export
setGeneric('attach',
           def = function(what, pos = 2L, name = deparse1(substitute(what), backtick = FALSE), 
                          warn.conflicts = TRUE) {
             
             if(!inherits(what,"edsurvey.data.frame")){
               standardGeneric("attach")
             }
             else{
               suppressWarnings(
                 z <- getData(what, varnames=colnames(what),
                              dropUnusedLevels=TRUE, omittedLevels=FALSE,
                              addAttributes=TRUE, returnJKreplicates=TRUE
                 )
               )
               base::attach(z, pos, name, warn.conflicts)
             }
           })

