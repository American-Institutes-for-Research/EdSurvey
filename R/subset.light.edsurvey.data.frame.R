# @author Paul Bailey
#' @method [ light.edsurvey.data.frame
#' @export
"[.light.edsurvey.data.frame" <- function(x, i, j, ...) {
  # do the subset like it's a data.frame
  res <- NextMethod("[")
  if (is.vector(res)) {
    return(res)
  }
  # copy over all of the attributes
  atrs <- names(attributes(x))
  # but don't coppy these attributs over
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # copy the attributes over
  for (z in atrs) {
    attr(res, z) <- attr(x, z)
  }
  if (inherits(res, "data.frame")) {
    class(res) <- class(x)
  }
  res
}

# @author Trang Nguyen and Paul Bailey
#' @method subset light.edsurvey.data.frame
#' @export
subset.light.edsurvey.data.frame <- function(x, subset, ..., inside = FALSE) {
  if (!inherits(x, c("light.edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be a light.edsurvey.data.frame."))
  }

  if (inside) {
    if (inherits(subset, "character")) {
      subset <- parse(text = subset)[[1]]
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
    condition_call <- iparse(substitute(subset), x = x)
  } # Enf of if esle statmet: if imside is true
  # perform actual subset
  r <- eval(condition_call, x)
  res <- x[r, , drop = FALSE]
  return(res)
}
