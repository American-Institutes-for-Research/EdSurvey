#' @title Instructions for Downloading and Unzipping ICILS Files
#'
#' @description Provides instructions to download ICILS data to be processed in \code{readICILS}.
#' 
#' @param years an integer vector indicating the study year. Valid year is 2013 only.
#' @author Tom Fink
#' 
#' @example man/examples/downloadICILS.R
#' @seealso \code{\link{readICILS}}
#' @importFrom utils browseURL
#' @export
downloadICILS <- function(years=c(2013)) {
  if(is.null(years)){
    stop(paste0("The argument ", sQuote("years"), " must not be null."))
  }
  
  if(any(!(years %in% c(2013)))){
    stop(paste0("The argument ", sQuote("years"), " must have a value of ", sQuote("2013"), "."))
  }
  
  linkURL <- "https://www.iea.nl/data-tools/repository"
  
  txt <- c()
  txt <- c(txt, paste0("Please manually download and extract the SPSS (*.sav) formatted ICILS 2013 study data files from the IEA Data Repository to a folder on your local system or network. ",
                       "After the following steps are completed, the ", dQuote("readICILS"), " function can be used to read in the data. ",
                       "See help page (?readICILS) for more details."))
  txt <- c(txt, "\n")
  
  txt <- c(txt, paste0("\t", "1) Launch the IEA Data Repository web URL (", linkURL ,") in your web browser."))
  
  txt <- c(txt, paste0("\t", "2) Find your study (ICILS) and click the appropriate studies ", dQuote("year"), " link you wish to download."))
  
  txt <- c(txt, paste0("\t", "3) Next, click on the link ", dQuote("SPSS Data & Documentation"), " to download the SPSS (*.sav) files, which are compatible with the ", dQuote("readICILS()"), " function in EdSurvey. ",
                       "Follow your web browser's prompts to download the resulting *.zip file to a folder location you can find later."))
  
  txt <- c(txt, paste0("\t", "4) Locate your downloaded zip file (*.zip) container and use an extraction program to extract the folder's file contents. ",
                       "It is recommended to extract the SPSS (*.sav) files to an easy-to-remember folder path based on the study and year (e.g., for Microsoft Windows OS, ", sQuote("C:/EdSurveyData/ICILS/2013/"), ")."))
  
  txt <- c(txt, "\n")
  
  txt <- paste0(paste(paste(txt, collapse = "\n\n"),collapse="\n"),"\n\n")
  eout(txt)
  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL ,") in your browser:  "))
  
  if(tolower(trimws(nav))=="y"){
    browseURL(linkURL)
  }
  
  return(invisible(NULL))
}
