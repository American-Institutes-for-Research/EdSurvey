#' @name as.data.frame
#' @title Coerce to a Data Frame
#'
#' @description Function to coerce a \code{light.edsurvey.data.frame} to a \code{data.frame}.
#' @param x a \code{light.edsurvey.data.frame}
#' @param ... other arguments to be passed to \code{\link[base]{as.data.frame}}
#' @return a \code{data.frame}
#' @export
#'
#' @author Trang Nguyen
as.data.frame.light.edsurvey.data.frame <- function(x, ...) {
  res <- x
  atrs <- names(attributes(x))
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # remove edsurvey-related attributes
  for (z in atrs) {
    attr(res, z) <- NULL
  }
  class(res) <- "data.frame"
  res
}
