#' @rdname edsurvey-class
#' @export
setAttributes <- function(data, attribute, value) {
  # return attributes in a uniform way for either an edsurvey.data.frame or
  # a light.edsurvey.data.frame

  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so attributes returned in data
    attrs <- attributes(data)$names
  } else {
    # data is not an edsurvey.data.frame, so attributes returned in
    # attributes(data)
    attrs <- names(attributes(data))[!names(attributes(data)) %in% c(
      "names",
      "class", "row.names"
    )]
  }
  if (is.null(attrs) & length(attrs == 0)) {
    stop(paste0("Attribute ", attribute, " not found."))
  }

  # if attribute searched for is not in defaultAttributes, stop function
  # and return a warning
  if (!(attribute %in% attrs)) {
    stop(paste0("Attribute ", attribute, " not found."))
  }

  # return the attribute depending on the class of data
  if (inherits(data, c("edsurvey.data.frame"))) {
    data[[attribute]] <- value
  } else {
    attributes(data)[[attribute]] <- value
  }
  invisible(data)
}
