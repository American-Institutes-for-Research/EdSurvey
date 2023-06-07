#' @title Dimensions of an edsurvey.data.frame or an edsurvey.data.frame.list
#'
#' @description Returns the dimensions of an \code{edsurvey.data.frame} or an
#'              \code{edsurvey.data.frame.list}.
#'
#' @param x an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @return For an \code{edsurvey.data.frame}, returns a
#'         numeric vector of length two, with the first element being the number
#'         of rows and the second element being the number of columns.
#'
#'         For an \code{edsurvey.data.frame.list}, returns a list of length
#'         two, where the first element is named \code{nrow} and is a
#'         numeric vector containing the number of rows for each element of the
#'         \code{edsurvey.data.frame.list}. The second element is named
#'         \code{ncol} and is the number of columns for each element.
#'         This is done so that the \code{nrow} and \code{ncol} functions
#'         return meaningful results, even if nonstandard.
#'
#' @author Paul Bailey
#' @aliases dim.edsurvey.data.frame.list
#' @export
dim.edsurvey.data.frame <- function(x) {
  testVars <- NULL
  excludeVars <- NULL

  for (di in x$dataList) {
    if (di$isDimLevel) {
      testVars <- names(di$lafObject)
    } else {
      excludeVars <- c(excludeVars, names(di$lafObject))
    }
  }

  nrow <- length(suppressWarnings(getData(x, testVars[!testVars %in% excludeVars][1], drop = TRUE, dropOmittedLevels = FALSE, defaultConditions = FALSE)))

  # every column must have a name
  ncol <- length(colnames(x))
  return(c(nrow, ncol))
}

#' @export
dim.edsurvey.data.frame.list <- function(x) {
  res <- sapply(x$data, function(li) {
    if (is.null(li)) {
      c(NA, NA)
    } else {
      dim(li)
    }
  })
  return(list(nrow = res[1, ], ncol = res[2, ]))
}

# this method used internally to get the number of rows on the original data
nrow2.edsurvey.data.frame <- function(x) {
  getAttributes(x, "dim0")[1]
}


# dimnames (for rownames and colnames)
#' @author Trang Nguyen
#' @export
dimnames.edsurvey.data.frame <- function(x) {
  # grab cache variable names
  nameVals <- colnames(x$cache)
  # these two are not intended to be shown
  nameVals <- nameVals[!nameVals %in% c("DEFAULT", "zzz_Filler")]
  for (di in x$dataList) {
    if (!is.null(di$lafObject)) {
      nameVals <- c(nameVals, names(di$lafObject))
    }
  }

  return(list(NA, unique(nameVals)))
}

#' @author Trang Nguyen
#' @export
dimnames.edsurvey.data.frame.list <- function(x) {
  ret <- lapply(x$datalist, function(li) {
    dimnames(li)[[2]]
  })
  return(list(NA, ret))
}
