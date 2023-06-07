# checks if the class of `data` is one of `classes` and gives an appropriate error if it is not.
checkDataClass <- function(data, classes, argument = "data") {
  # see if it inherits from classes
  if (!inherits(data, classes)) {
    # giving an appropriate error requires us to figure out what is in classes
    # here we check that with all.equal. Because all.equal is picky about sort order
    # take that off the table by sorting first.
    if (all.equal(
      sort(classes),
      sort(c("edsurvey.data.frame", "light.edsurvey.data.frame"))
    )[[1]][1]
    == TRUE) {
      stop(paste0(
        "The argument ", sQuote(argument), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See ",
        dQuote(paste0(
          "Using the ", sQuote("EdSurvey"),
          " Package's getData Function to Manipulate the NAEP Primer Data vignette"
        )),
        " for how to work with data in a light.edsurvey.data.frame."
      ))
    }
    if (all.equal(
      sort(classes),
      sort(c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
    )[[1]]
    == TRUE) {
      stop(paste0(
        "The argument ", sQuote(argument), " must be an edsurvey.data.frame, a light.edsurvey.data.frame, or an edsurvey.data.frame.list. See ",
        dQuote(paste0(
          "Using the ", sQuote("EdSurvey"),
          " Package's getData Function to Manipulate the NAEP Primer Data vignette"
        )),
        " for how to work with data in a light.edsurvey.data.frame."
      ))
    }
    # not expected, but just in case
    stop(paste0(sQuote(argument), " must be one of ", pasteItems(dQuote(classes), "or"), "."))
  }
}
