#' @title Instructions for Downloading and Unzipping CivED or ICCS Files
#'
#' @description Provides instructions to download CivED or ICCS data to be processed in \code{readCivEDICCS}.   
#' 
#' @param years an integer vector indicating the study year. Valid years are 1999, 2009, and 2016.
#' @author Tom Fink
#' 
#' @example man/examples/downloadICCS.R
#' @seealso \code{\link{readCivEDICCS}}
#' @importFrom utils browseURL
#' @export
downloadCivEDICCS <- function(years=c(1999, 2009, 2016)) {
  
  if(is.null(years)){
    stop(paste0("The argument ", sQuote("years"), " must not be null."))
  }
  
  if(any(!(years %in% c(1999, 2009, 2016)))){
    stop(paste0("The argument ", sQuote("years"), " must have values of only ", sQuote("1999"), ", ",  sQuote("2009"), ", or ", sQuote("2016"), "."))
  }
  
  linkURL <- "https://www.iea.nl/data-tools/repository"
  
  txt <- c()
  txt <- c(txt, paste0("Please manually download and extract the SPSS (*.sav) formatted CivED 1999, ICCS 2009, or ICCS 2016 study data files from the IEA Data Repository to a folder on your local system or network. ",
                       "After the following steps are completed, the ", dQuote("readCivEDICCS"), " function can be used to read in the data. ",
                       "See help page (?readCivEDICCS) for more details."))
  txt <- c(txt, "\n")
  
  txt <- c(txt, paste0("\t", "1) Launch the IEA Data Repository web URL (", linkURL ,") in your web browser."))
  
  txt <- c(txt, paste0("\t", "2) Find your study (CivED or ICCS) and click the appropriate studies ", dQuote("year"), " link you wish to download."))
  
  txt <- c(txt, paste0("\t", "3) Next, click on the link ", dQuote("SPSS Data & Documentation"), " to download the SPSS (*.sav) files, which are compatible with the ", dQuote("readCivEDICCS()"), " function in EdSurvey. ",
                       "Follow your web browser's prompts to download the resulting *.zip file to a folder location you can find later."))
  
  txt <- c(txt, paste0("\t", "4) Locate your downloaded zip file (*.zip) container and use an extraction program to extract the folder's file contents. ",
                       "It is recommended to extract the SPSS (*.sav) files to an easy-to-remember folder path based on the study and year (e.g., for Microsoft Windows OS, ", pasteItems(sQuote(c("C:/EdSurveyData/CivED/1999/", "C:/EdSurveyData/ICCS/2009/", "C:/EdSurveyData/ICCS/2016/"))), ")."))
  
  txt <- c(txt, "\n")

  
  txt <- paste0(paste(paste(txt, collapse = "\n\n"),collapse="\n"),"\n\n")
  eout(txt)
  
  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL ,") in your browser:  "))
  
  if(tolower(trimws(nav))=="y"){
    browseURL(linkURL)
  }
  
  return(invisible(NULL))
}
