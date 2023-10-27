#' @title Combine R Objects by Rows or Columns
#'
#' @description Implements \code{\link{cbind}} and \code{\link{rbind}} for \code{light.edsurvey.data.frame} class.
#'              It takes a sequence of \code{vector}, \code{matrix}, \code{data.frame}, or \code{light.edsurvey.data.frame} arguments and combines
#'              by columns or rows, respectively.
#' @details Because \code{cbind} and \code{rbind} are standard generic functions that do not use method dispatch, we set this function as generic,
#'          which means it overwrites \code{base::cbind} and \code{base::rbind} on loading. If none of the specified elements are of class \code{light.edsurvey.data.frame},
#'          the function will revert to the standard \code{base} method. However, to be safe, you might want to explicitly use \code{base::cbind} when needed after loading the package.
#'
#'          The returned object will contain attributes only from the first \code{light.edsurvey.data.frame} object in the call to
#'          \code{cbind.light.edsurvey.data.frame}.
#' @param ... one or more objects of class \code{vector}, \code{data.frame}, \code{matrix}, or \code{light.edsurvey.data.frame}
#' @param deparse.level integer determining under which circumstances column and row names are built from the actual arguments. See \ifelse{latex}{\code{cbind}}{\code{\link[base:cbind]{cbind}}}.
#' @return  a matrix-like object like \code{matrix} or \code{data.frame}. Returns a \code{light.edsurvey.data.frame} if there is
#'          at least one \code{light.edsurvey.data.frame} in the list of arguments.
#'
#'
#' @seealso \ifelse{latex}{\code{cbind}}{\code{\link[base:cbind]{cbind}}}
#' @author Trang Nguyen, Michael Lee, and Paul Bailey
#' @method cbind light.edsurvey.data.frame
#' @export
cbind.light.edsurvey.data.frame <- function(..., deparse.level=1) {
  args <- list(...)
  atrs <- NULL
  if (all(sapply(args, function(x) { !inherits(x, "light.edsurvey.data.frame") }))) {
    return(do.call(base::cbind, c(args, list(deparse.level = deparse.level))))
  }
  for (x in list(...)) {
    if (inherits(x, "light.edsurvey.data.frame")) {
      atrs <- names(attributes(x))
      atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
      atrslist <- attributes(x)
      break # only use attributes of the first light.edsurvey.data.frame
    }
  }
  if (!is.null(atrs)) {
    res <- data.frame(...)
    lapply(atrs, function(z) {
      # make a temporary copy of res
      dat <- get("res")
      # add the attribute to the temporary copy
      attr(dat, z) <- atrslist[[z]]
      # then make res (in the environment of the function) be the temporary
      # copy that now has this attribute
      res <<- dat
    })
    if (inherits(res, "data.frame")) {
      class(res) <- class(x)
    }
  } else {
    res <- base::cbind(..., deparse.level = deparse.level)
  }
  res
}
