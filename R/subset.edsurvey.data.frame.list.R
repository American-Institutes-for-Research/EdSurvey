# @author Trang Nguyen and Paul Bailey
#' @importFrom utils find
#' @method subset edsurvey.data.frame.list
#' @export
subset.edsurvey.data.frame.list <- function(x, subset, inside = FALSE, drop = FALSE, ...) {
  checkDataClass(x, c("edsurvey.data.frame.list"))

  if (inside) {
    if (inherits(subset, "character")) {
      subset <- parse(text = subset)[[1]]
    }
    subset_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the subset will not change as that
    # variable is updated.
    # add it to the user subsets

    # parse the subset
    # substitute in variables that are available in the current environment
    subset_call <- iparse(substitute(subset), x = x)
  } # Enf of if esle statmet: if inside is true

  res <- x
  subsetVars <- all.vars(subset_call)
  res$datalist <- lapply(seq_along(x$datalist), function(dataListi) {
    dataList_li <- x$datalist[[dataListi]]
    # check whether the variable exists the edsurvey.data.frame
    for (dataList_v in subsetVars) {
      if (!dataList_v %in% colnames(dataList_li)) {
        warning(paste0("Variable ", sQuote(dataList_v), "is not found in the data ", sQuote(x$covs[dataListi, ]), "."))
        return(NULL)
      }
    }
    dataList_li[["userConditions"]] <- c(dataList_li[["userConditions"]], list(subset_call))
    dataList_li
  })

  # Remove NULL element
  if (drop) {
    index_removed <- which(vapply(
      res$datalist,
      function(i) {
        return(is.null(i) || nrow(i) == 0)
      },
      FUN.VALUE=logical(1))
    )
  } else {
    index_removed <- which(vapply(X=res$datalist, FUN=is.null, FUN.VALUE=logical(1)))
  }


  if (length(index_removed) > 0) {
    res$datalist[index_removed] <- NULL
    res$covs <- res$covs[-index_removed, names(res$covs), drop = FALSE]
    row.names(res$covs) <- NULL
  }

  # if there is no element left
  if (length(res$datalist) == 0) {
    res <- NULL
  }
  if (length(res$datalist) == 1) {
    res <- res$datalist[[1]]
  }
  res
} # end of fuction subset.edsurvey.data.frame
