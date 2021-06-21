#' @title Download and Unzip HSLS Files
#'
#' @description Uses an Internet connection to download HSLS data. 
#'              Data come from \href{https://nces.ed.gov/edat/}{nces.ed.gov} zip files. This
#'              function works for 2009 data.
#'
#' @param root a character string indicating the directory where the HSLS
#'             data should be stored. Files are placed in a
#'             subdirectory named HSLS/[year].
#' @param years an integer vector of the assessment years to download. Valid year is 2009 only.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'                
#' @author Tom Fink
#' @seealso \code{\link{readHSLS}}
#' @example man\examples\downloadHSLS.R
#' @importFrom utils download.file
#' @export
downloadHSLS <- function(root, years=c(2009), cache=FALSE, verbose=TRUE) {
  fixTimeout()
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  #normalize path before testing:: file.path will remove trailing seperator if present
  root <- suppressWarnings(file.path(normalizePath(root, winslash = "/")))
  if(!dir.exists(root)){
    stop(paste0("The argument ", sQuote("root"), " must be a valid path."))
  }
  
  validYears <- c(2009)
  if(length(years) > 1) {
    for(yi in years) {
      downloadHSLS(years=yi, root=root, verbose=verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }
  
  d2009 <- c("https://nces.ed.gov/EDAT/Data/Zip/HSLS_2016_v1_0_SPSS_Datasets.zip")
  
  if(!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }
  
  #check if base folder exists first
  if(!dir.exists(file.path(root, "HSLS"))){
    dir.create(file.path(root, "HSLS"))
  }
  yroot <- file.path(root, "HSLS", paste0(year)) #build yroot with file.path to avoid issues with seperators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  
  d <- get(paste0("d",year))
  for(di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if(!file.exists(file.path(yroot,bn))) {
      # download
      tryCatch(download.file(d[di],file.path(yroot,bn), quiet = !verbose, cacheOK = FALSE),
               error = function(e){
                 stop(paste0("Error downloading file at URL: ", sQuote(d[di]), ". ",
                             "Message: ", e))
               })
    } else { #end if(!file.exists(file.path(yroot,bn)))
      if(verbose){
        eout(paste0("Found downloaded ", year ," HSLS file ",sQuote(bn),"."))
      }
    }# end else for if(!file.exists(file.path(yroot,bn)))
    
    if(grepl("\\.zip$", bn, ignore.case = TRUE)){
      
      lst <- unzip(file.path(yroot,bn), list=TRUE) # just lists the files
      
      if(verbose){
        eout(paste0("Unzipping ", year ," HSLS files from ",sQuote(bn),"."))
      }
      
      for(i in 1:nrow(lst)) {
        if(!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
          
          if(verbose){
            eout(paste0("  Unzipping ",sQuote(lst$Name[i]),"."))
          }
          
          
          unzip(file.path(yroot,bn), files=lst$Name[i], exdir = yroot)
          if(basename(lst$Name[i]) != lst$Name[i]) {
            file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
          }#end if(basename(lst$Name[i]) != lst$Name[i])
        }#end if(!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i])
      }#end for(i in 1:nrow(lst))
    }#end if(grepl("\\.zip$", bn, ignore.case = TRUE))
  }#end for(di in 1:length(d))
  
  if(cache){
    if(verbose){
      eout("Caching ", year ," ELS files.")
    }
    
    if(year==2009){
      notUsed <- readHSLS(path = yroot, filename = "hsls_16_student_v1_0.sav", verbose = verbose)
      notUsed <- NULL #clear memory space
      
      notUsed <- readHSLS(path = yroot, filename = "hsls_09_school_v1_0.sav", verbose = verbose)
      notUsed <- NULL #clear memory space
    }
    
    notUsed <- NULL #clear memory space
  }#end if(cache)
  
  return(invisible(NULL))
}
