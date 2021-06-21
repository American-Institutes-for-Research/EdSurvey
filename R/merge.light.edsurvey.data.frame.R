#' @name merge
#' @title EdSurvey Merge
#'
#' @description Takes a \code{data.frame} or a \code{light.edsurvey.data.frame} and merges with a \code{light.edsurvey.data.frame}.
#'
#' @param x a \code{light.edsurvey.data.frame}. The attributes of the resulting \code{light.edsurvey.data.frame} 
#'          are taken from \code{x}.
#' @param y either a \code{light.edsurvey.data.frame} or a \code{data.frame}
#' @param ... arguments to be passed to \ifelse{latex}{\code{merge}}{\code{\link[base]{merge}}} 
#' @return a \code{light.edsurvey.data.frame} with the same attributes as \code{x}
#'
#' @seealso \ifelse{latex}{\code{merge}}{\code{\link[base]{merge}}} 
#' @method merge light.edsurvey.data.frame
#' @aliases merge merge.light.edsurvey.data.frame
#' @example man/examples/merge.light.edsurvey.data.frame.R
#' @author Trang Nguyen
#'
#' @export
merge.light.edsurvey.data.frame <- function(x, y, ...) {
  # use the cbind method as if it were a data.frame
  res <- NextMethod("merge") # res is the (preliminary) result
  # copy over all of the attributes
  # first, get their names
  atrs <- names(attributes(x))
  # remove data.frame attributes not in a light.edsurvey.data.frame
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # then loop over all attributes and add them to the result
  lapply(atrs, function(z) {
    # make a temporary copy of res
    dat <- get("res")
    # add the attribute to the temporary copy
    attr(dat, z) <- attr(get("x"), z)
    # then make res (in the environment of the function) be the temporary
    # copy that now has this attribute
    res <<- dat 
  })
  if (inherits(res, "data.frame")) {
    class(res) <- class(x)
  } # End of test of data.frame inheritance
  res
}
