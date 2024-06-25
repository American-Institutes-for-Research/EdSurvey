#' @title Instructions for Downloading and Unzipping NHES Files
#'
#' @description Provides instructions to download the public-use National Household Education Survey (NHES) data in SPSS (*.sav) format
#'              for use with the \code{readNHES} function.
#'              The data originates from the \href{https://nces.ed.gov/datalab/onlinecodebook}{NCES Online Codebook} zip files.
#'              This function works for data from the years
#'              1991, 1993, 1995, 1996, 1999, 2001, 2003, 2005, 2007, 2012, 2016, and 2019.
#'
#' @param years an integer vector of the assessment years. Valid years are
#'              1991, 1993, 1995, 1996, 1999, 2001, 2003, 2005, 2007, 2012, 2016, and 2019.
#'              The instructions are the same for each year, this is used as reference only.
#'
#' @note The NHES data files are additionally available from the \href{https://nces.ed.gov/nhes/dataproducts.asp}{NHES data product page}.  However,
#'       the data files provided at that page do not include all available years of data, and contain inconsistent data file formats.
#'
#' @author Tom Fink
#' @seealso \code{\link{readNHES}}
#' @example man/examples/downloadNHES.R
#' @importFrom utils browseURL
#' @export
downloadNHES <- function(years = c(
                           1991, 1993, 1995, 1996, 1999,
                           2001, 2003, 2005, 2007, 2012, 2016, 2019
                         )) {
  validYears <- c(
    1991, 1993, 1995, 1996, 1999,
    2001, 2003, 2005, 2007, 2012, 2016, 2019
  )

  if (is.null(years)) {
    stop(paste0("The argument ", sQuote("years"), " must not be null."))
  }

  if (any(!(years %in% validYears))) {
    invalidYears <- unique(years[!(years %in% validYears)])
    stop(paste0("Invalid Year(s) Specified: ", paste(sQuote(invalidYears), sep = "", collapse = ", "), "."))
  }

  linkURL <- "https://nces.ed.gov/datalab/onlinecodebook"

  txt <- c()
  txt <- c(txt, paste0(
    "Please manually download and extract the SPSS (*.sav) formatted NHES study data files from the NCES Online Codebook to a folder on your local system or network. ",
    "After the following steps are completed, the ", dQuote("readNHES"), " function can be used to read in the data. ",
    "See help page (?readNHES) for more details."
  ))
  txt <- c(txt, "\n")

  txt <- c(txt, paste0("\t", "1) Launch the NCES Online Codebook web URL (", linkURL, ") in your web browser."))

  txt <- c(txt, paste0("\t", "2) Read through the NCES data usage agreement and click the ", dQuote("Agree To Terms Above"), " button to continue."))

  txt <- c(txt, paste0("\t", "3) Find your study (NHES) and click the appropriate studies ", dQuote("year"), " link you wish to download."))

  txt <- c(txt, paste0("\t", "4) Next, click on the ", dQuote("Download"), " button to see a list of available data formats."))

  txt <- c(txt, paste0("\t", "5) Select the ", dQuote("SPSS"), " data file format, which will be compatible with EdSurvey."))

  txt <- c(txt, paste0("\t", "6) To begin the Zip file download click the ", sQuote("Download ____ SPSS Dataset"), " button and follow your browsers prompts to save the file."))

  txt <- c(txt, paste0(
    "\t", "7) Locate your downloaded zip file (*.zip) container and use an extraction program to extract the folder's file contents. ",
    "It is recommended to extract the SPSS (*.sav) files to an easy-to-remember folder path based on the study and year (e.g., ", pasteItems(c(sQuote("~/NHES/2019/"), sQuote("~/NHES/2016/"))), ")."
  ))

  txt <- c(txt, "\n\n")


  txt <- paste0(paste(paste(txt, collapse = "\n\n"), collapse = "\n"), "\n\n")
  eout(txt)

  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL, ") in your browser:  "))

  if (tolower(trimws(nav)) == "y") {
    browseURL(linkURL)
  }

  return(invisible(NULL))
}
