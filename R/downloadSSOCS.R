#' @title Instructions for Downloading and Unzipping SSOCS Files
#'
#' @description Provides instructions to download School Survey on Crime and Safety (SSOCS) data in SAS (*.sas7bdat) format
#'              for use with the \code{readSSOCS} function.
#'              The data originates from the SSOCS Data Products website at \href{https://nces.ed.gov/surveys/ssocs/data_products.asp}{nces.ed.gov}.
#'              This function works for the following school year datasets: 2000 (1999--2000), 2004 (2003--2004), 2006 (2005--2006), 
#'              2008 (2007--2008), 2010 (2009--2010), 2016 (2015--2016), and 2018 (2017--2018).
#'
#' @param years an integer vector of the study years to download. Valid years are as follows: 
#' 2000, 2004, 2006, 2008, 2010, 2016, 2018 (see description).  The instructions are the same for each year, this is for reference only.
#'                
#' @note The year parameter value is shortened to the ending year of the school year (e.g., 2006 refers to the 2005--2006 school year data).
#' Manually downloading the data files is required to fulfill the data usage agreement.
#'
#' @author Tom Fink
#' @seealso \code{\link{readSSOCS}}
#' @example man/examples/downloadSSOCS.R
#' @importFrom utils download.file
#' @export
downloadSSOCS <- function(years=c(2000, 2004, 2006, 2008, 2010, 2016, 2018)) {

  validYears <- c(2000, 2004, 2006, 2008, 2010, 2016, 2018)

  if(is.null(years)){
    stop(paste0("The argument ", sQuote("years"), " must not be null."))
  }
  
  if(any(!(years %in% validYears))){
    stop(paste0("Invalid Year(s). Please select from the valid years: ", pasteItems(sQuote(validYears)), "."))
  }
  
  linkURL <- "https://nces.ed.gov/surveys/ssocs/data_products.asp"
  
  txt <- c()
  txt <- c(txt, paste0("Please manually download and extract the SAS Data (*.sas7bdat) formatted SSOCS study data files from the study product's page 
                       to a folder on your local system or network. ",
                       "After the following steps are completed, the ", dQuote("readSSOCS"), " function can be used to read in the data. ",
                       "See help page (?readSSOCS) for more details."))
  txt <- c(txt, "\n")
  
  txt <- c(txt, paste0("\t", "1) Launch the SSOCS Data Product web URL (", linkURL ,") in your web browser."))
  
  txt <- c(txt, paste0("\t", "2) Locate the SAS Data format for the study years of interest.  
                       Note that some years of data files have multiple data formats bundled together as one file."))
  
  txt <- c(txt, paste0("\t", "3) Once the appropriate study year and format is found click the link and follow the on-screen prompts."))
  
  txt <- c(txt, paste0("\t", "4) Some of the download links will be a direct link to the file, others will open another window with additional links. 
                       If prompted to agree to terms, be sure to accept.  Note that none of the SAS scripts or setup files are required."))
  
  txt <- c(txt, paste0("\t", "5) Locate your downloaded zip file (*.zip) container and use an extraction program to extract the folder's file contents. ",
                       "It is recommended to extract the data files to an easy-to-remember folder path based on the study and year (e.g., ", pasteItems(c(sQuote("~/SSOCS/2018/"), sQuote("~/SSOCS/2016/"))), ")."))
  
  txt <- c(txt, "\n\n")
  
  
  txt <- paste0(paste(paste(txt, collapse = "\n\n"),collapse="\n"),"\n\n")
  eout(txt)
  
  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL ,") in your browser:  "))
  
  if(tolower(trimws(nav))=="y"){
    browseURL(linkURL)
  }
  
  return(invisible(NULL))
}
