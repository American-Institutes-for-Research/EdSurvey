# @author Paul Bailey
#' @method [ light.edsurvey.data.frame
#' @export
"[.light.edsurvey.data.frame" <- function(x,i, j, ...) {
  # do the subset like it's a data.frame
  res <- NextMethod("[")
  if(is.vector(res)) {
    return(res)
  }
  # copy over all of the attributes
  atrs <- names(attributes(x))
  # but don't coppy these attributs over
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # copy the attributes over
  for(z in atrs) {
    attr(res, z) <- attr(x,z)
  }
  if(inherits(res, "data.frame")) {
    class(res) <- class(x)
  }
  res
}

# @author Trang Nguyen and Paul Bailey
#' @method subset light.edsurvey.data.frame
#' @export
subset.light.edsurvey.data.frame <- function(x, subset, ..., inside=FALSE) {
  subsetEnv <- parent.frame(n=2)
  
  if(!inherits(x, c("light.edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be a light.edsurvey.data.frame."))
  }
  
  if(inside) {
    if(inherits(subset, "character")) {
      subset <- parse(text=subset)[[1]]
    }
    condition_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the condition will not change as that
    # variable is updated.
    # add it to the user conditions
    
    # parse the condition
    # substitute in variables that are available in the current environment
    condition_call <- substitute(subset)
    iparse <- function(iparseCall, subsetEnv, iparseDepth=1L) {
      # for each element
      for(iparseInd in 1:length(iparseCall)) {
        # if it is a name
        if(class(iparseCall[[iparseInd]]) %in% c("name")) {
          iparseCall_c <- as.character(iparseCall[[iparseInd]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if(! iparseCall_c %in% colnames(x)) {
            if(length(find(iparseCall_c)) > 0) {
              if (iparseCall[[iparseInd]] == "%in%" || is.function(iparseCall[[iparseInd]]) || is.function(get(iparseCall_c, find(iparseCall_c)))) {
                ev <- eval(substitute(iparseCall[[iparseInd]]), parent.frame())
              } else {
                # get the variable
                ev <- eval(iparseCall[[iparseInd]], parent.frame())
              }
              iparseCall[[iparseInd]] <- ev
            } #end if length(find(iparseCall_c)) > 0)
            # but, if dynGet returns, use that instead
            iparsedg <- dynGet(iparseCall_c, ifnotfound="", minframe=1L)
            # if dynGet found something
            if(any(iparsedg != "")) {
              iparseCall[[iparseInd]] <- iparsedg
            }
          } # end if(!call_c)
        } # end if(class(iparseCall[[iparseInd]]) %in% c("name")) {
        if(class(iparseCall[[iparseInd]]) %in% "call") {
          # if this is a call, recursively parse that
          iparseCall[[iparseInd]] <- iparse(iparseCall[[iparseInd]], subsetEnv, iparseDepth)
        } #end of if statment: if class(iparseCall[[iparseInd]]) %in% "call"
      } # end of for loop: i in 1:length(iparseCall)
      iparseCall
    } # End of fucntion: iparse
    condition_call <- iparse(condition_call, subsetEnv)
    #condition_call <- iparse(condition_call, parent.frame())
  } # Enf of if esle statmet: if imside is true 
  # perform actual subset
  r <- eval(condition_call, x)
  res <- x[r, , drop=FALSE]
  return(res)
}
