#' @rdname cbind.light.edsurvey.data.frame
# @author: Trang Nguyen
#' @method rbind light.edsurvey.data.frame
#' @export
rbind.light.edsurvey.data.frame <- function(..., deparse.level = 1) {
  args <- list(...)
  atrs <- NULL
  if (all(sapply(args, function(x) {!inherits(x, "light.edsurvey.data.frame") }))) {
    return(do.call(base::rbind, c(args, list(deparse.level = deparse.level))))
  }

  for (x in list(...)) {
    if (inherits(x, "light.edsurvey.data.frame")) {
      atrs <- names(attributes(x))
      atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
      atrslist <- attributes(x)
      break # only use attributes of the first light.edsurvey.data.frame
    }
  }

  # coerce all light.edsurvey.data.frame to data.frame
  for (i in 1:length(args)) {
    if (inherits(args[[i]], "light.edsurvey.data.frame")) {
      args[[i]] <- as.data.frame(args[[i]])
    }
  }
  res <- do.call(base::rbind, c(args, list(deparse.level = deparse.level)))
  if (!is.null(atrs)) {
    lapply(atrs, function(z) {
      # make a temporary copy of res
      dat <- get("res")
      # add the attribute to the temporary copy
      attr(dat, z) <- atrslist[[z]]
      # then make res (in the environment of the function) be the temporary
      # copy that now has this attribute
      res <<- dat
    })
  }
  if (inherits(res, "data.frame")) {
    class(res) <- class(x)
  } # End of test of data.frame inheritance
  res
}
