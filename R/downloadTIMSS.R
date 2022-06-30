#' @title Download and Unzip TIMSS Files
#'
#' @description Uses an Internet connection to download TIMSS data.
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 2003, 2007, 2011, 2015, and 2019 data.
#'
#' @param root a character string indicating the directory where the TIMSS
#'             data should be stored. Files are placed in a
#'             subdirectory named TIMSS/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 2003, 2007, 2011,
#'              2015, and 2019.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' @author Tom Fink
#' @seealso \code{\link{readTIMSS}}
#' @example man/examples/downloadTIMSS.R
#' @importFrom utils download.file
#' @export
downloadTIMSS <- function(root, years=c(2003, 2007, 2011, 2015, 2019), cache=FALSE, verbose=TRUE) {
  fixTimeout()
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  #normalize path before testing:: file.path will remove trailing separator if present
  root <- suppressWarnings(file.path(normalizePath(root, winslash = "/")))
  if(!dir.exists(root)){
    stop(paste0("The argument ", sQuote("root"), " must be a valid path."))
  }
  
  validYears <- c(2003, 2007, 2011, 2015, 2019)
  if(length(years) > 1) {
    for(yi in years) {
      downloadTIMSS(years=yi, root=root, cache=cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d2003 <- c("https://timssandpirls.bc.edu/timss2003i/PDF/t03_spss_1.zip",
             "https://timssandpirls.bc.edu/timss2003i/PDF/t03_spss_2.zip")

  d2007 <- c("https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G4_1.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G4_2.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_1.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_2.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_3.zip")

  d2011 <- c("https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt4.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_ItemParameters.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_ItemInformation.zip")

  d2015 <- c("https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt4.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/TN15_SPSSData.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_IRTItemParameters.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_ItemInformation.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_IRTItemParameters.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_ItemInformation.zip")
  
  d2019 <- c("https://timss2019.org/international-database/downloads/T19_G4_SPSS%20Data.zip",
             "https://timss2019.org/international-database/downloads/T19_G8_SPSS%20Data.zip",
             "https://timss2019.org/international-database/downloads/T19_G4_Item%20Information.zip",
             "https://timss2019.org/international-database/downloads/T19_G8_Item%20Information.zip",
             "https://timss2019.org/international-database/downloads/T19_G4_IRT%20Item%20Parameters.zip",
             "https://timss2019.org/international-database/downloads/T19_G8_IRT%20Item%20Parameters.zip")

  if(!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }
  
  #check if base folder exists first
  if(!dir.exists(file.path(root, "TIMSS"))){
    dir.create(file.path(root, "TIMSS"))
  }
  yroot <- file.path(root, "TIMSS", paste0(year)) #build yroot with file.path to avoid issues with separators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  d <- get(paste0("d",year))
  for(di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if(!file.exists(file.path(yroot,bn))) {
      # download zip and catch any errors
      tryCatch(download.file(d[di],file.path(yroot,bn), quiet = !verbose, cacheOK = FALSE),
                   error = function(e){
                     stop(paste0("Error downloading file at URL: ", sQuote(d[di]), ". ",
                                 "Message: ", e))
                   })
    } else {
      if(verbose==TRUE){
        cat(paste0("Found downloaded ", year ," TIMSS file ",sQuote(bn),".\n"))
      }
    }
    lst <- unzip(file.path(yroot,bn), list=TRUE) # just lists the files
    
    if(verbose==TRUE){
      cat(paste0("Unzipping ", year ," TIMSS files from ",sQuote(bn),".\n"))
    }
    
    for(i in 1:nrow(lst)) {
      if(!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
        
        if(verbose==TRUE){
          cat(paste0("  Unzipping ",sQuote(lst$Name[i]),".\n"))
        }
        
        unzip(file.path(yroot,bn),files= lst$Name[i], exdir = yroot)
        if(basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
        }
      }
    }
  }
  if(cache){
    if(verbose==TRUE){
      cat("Caching ", year ," TIMSS files.\n")
    }
    
    notUsed <- readTIMSS(yroot, countries="*", gradeLvl=4, verbose=verbose)
    notUsed <- NULL
    notUsed <- readTIMSS(yroot, countries="*", gradeLvl=8, verbose=verbose)
    notUsed <- NULL
    return(invisible(NULL))
  }
  
  return(invisible(NULL))
}

