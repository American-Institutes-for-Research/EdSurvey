#' @title Download and Unzip TALIS Files
#'
#' @description Uses an Internet connection to download TALIS data.
#'              Data come from \href{https://www.oecd.org/education/talis/}{OECD TALIS site} international zip files. This
#'              function works for 2008, 2013,and 2018 data.
#' 
#' @param root a character string indicating the directory where the TALIS
#'             data should be stored. Files are placed in a
#'             subdirectory named TALIS/[year].
#' @param years a numeric value indicating the assessment year. Available years are 2008, 2013, and 2018.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @author Tom Fink and Trang Nguyen
#' @seealso \code{\link{readTALIS}}
#' @importFrom utils download.file
#' @example  man/examples/downloadTALIS.R
#' @export
downloadTALIS <- function(root, years=c(2008, 2013, 2018), cache=FALSE, verbose=TRUE) {
  fixTimeout()
  #validation checks
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  validYears <- c(2008, 2013, 2018)
  if(length(years) > 1) {
    for(yi in years) {
      downloadTALIS(root=root, years=yi, cache=cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }
  
  if(!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }
  
  d2008 <- c("https://webfs.oecd.org/talis/SPSS_2008_international.zip")
  d2013 <- c("https://webfs.oecd.org/talis/SPSS_2013_international.zip")
  d2018 <- c("https://webfs.oecd.org/talis/SPSS_2018_international.zip")
  
  #check if base folder exists first
  if(!dir.exists(file.path(root, "TALIS"))){
    dir.create(file.path(root, "TALIS"))
  }
  yroot <- file.path(root, "TALIS", paste0(year)) #build yroot with file.path to avoid issues with seperators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  d <- get(paste0("d",year))
  
  for(di in 1:length(d)){
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
        cat(paste0("Found downloaded ", year ," TALIS file ",sQuote(bn),".\n"))
      }
    }
    lst <- unzip(file.path(yroot,bn), list=TRUE) # just lists the files
    
    if(verbose==TRUE){
      cat(paste0("Unzipping ", year ," TALIS files from ",sQuote(bn),".\n"))
    }
    
    for(i in 1:nrow(lst)){
      if(!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
        
        if(verbose==TRUE){
          cat(paste0("  Unzipping ",sQuote(lst$Name[i]),".\n"))
        }
        
        unzip(file.path(yroot,bn),files= lst$Name[i], exdir = yroot)
        if(basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
        }
      }
    }#end for(i in 1:nrow(lst))
  }#end for(di in 1:length(d))

  if(cache){
    
    if(year==2008){
      iscedLvls <- "b" #only has 'b' level
    }else{
      iscedLvls <- c("a", "b", "c")
    }
      
    for(iLvl in iscedLvls){
      unused <- readTALIS(yroot, countries = "*", isced = iLvl, dataLevel = "teacher", verbose = verbose)
      unused <- NULL
    }
  }#end if(cache)
    
  return(invisible(NULL))
}
