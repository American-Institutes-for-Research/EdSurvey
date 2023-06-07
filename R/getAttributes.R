#' @rdname edsurvey-class
#' @export
getAttributes <- function(data, attribute = NULL, errorCheck = TRUE) {
  # return attributes in a uniform way for either an edesurvey.data.frame or
  # a light.edsurvey.data.frame

  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  stopifnot("errorCheck must be a logical value of length 1" = is.logical(errorCheck) && length(errorCheck) == 1)

  # get attributes from data
  # how we gt attributes depends on the class of data.
  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so attributes returned in data
    attrs <- attributes(data)$names
  } else {
    # data is not an edsurvey.data.frame, so attributes returned in
    # attributes(data)
    attrs <- names(attributes(data))[!names(attributes(data)) %in% c("names", "class", "row.names")]
  }

  # validate
  if (is.null(attrs) & length(attrs == 0)) {
    if (errorCheck) {
      stop(paste0("Attribute ", attribute, " not found."))
    }
    return(NULL)
  }

  # recodes can be NULL, so it won't show up as a named attribute
  # so return it as NULL when missing from the list
  if (attribute %in% c("recodes", "psuVar", "stratumVar") & !(attribute %in% attrs)) {
    return(NULL)
  }

  # if attribute searched for is not in defaultAttributes, stop function
  # and return a warning
  if (!(attribute %in% attrs)) {
    if (errorCheck) {
      stop(paste0("Attribute ", attribute, " not found."))
    }
    return(NULL)
  }

  # return the attribute depending on the class of data
  if (inherits(data, c("edsurvey.data.frame"))) {
    attr <- data[[attribute]]
  } else {
    attr <- attributes(data)[[attribute]]
    if (length(attr) == 1 && inherits(attr, "character") && attr == "NULL") {
      return(NULL)
    }
  }
  return(attr)
}
