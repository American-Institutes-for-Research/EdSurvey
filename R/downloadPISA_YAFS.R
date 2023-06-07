#' @title Instructions for Downloading and Unzipping PISA YAFS Files
#'
#' @description Provides instructions to download PISA YAFS data to be processed in \code{readPISA_YAFS}.
#'
#' @param years an integer vector indicating the study year. Valid year is 2016 only.
#' @author Tom Fink
#'
#' @example man/examples/downloadPISA_YAFS.R
#' @seealso \code{\link{readPISA_YAFS}}
#' @importFrom utils browseURL
#' @export
downloadPISA_YAFS <- function(years = c(2016)) {
  if (is.null(years)) {
    stop(paste0("The argument ", sQuote("years"), " must not be null."))
  }

  if (any(!(years %in% c(2016)))) {
    stop(paste0("The argument ", sQuote("years"), " must have a value of ", sQuote("2013"), "."))
  }

  linkURL <- "https://nces.ed.gov/pubsearch/pubsinfo.asp?pubid=2021022"

  txt <- c()
  txt <- c(txt, paste0("The 2016 PISA YAFS data is available for download at this location: ", linkURL, "\n"))

  txt <- c(txt, paste0("Download and Extract the PISA YAFS ASCII raw data file (*.dat) and the associated SPSS control file (.sps) for use with ", sQuote("readPISA_YAFS"), " from the website.\n"))

  txt <- c(txt, paste0("See ", sQuote("?readPISA_YAFS"), " for additional details.\n"))

  txt <- paste0(paste(paste(txt, collapse = "\n\n"), collapse = "\n"), "\n\n")

  eout(txt)
  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL, ") in your browser:  "))

  if (tolower(trimws(nav)) == "y") {
    browseURL(linkURL)
  }

  return(invisible(NULL))
}
