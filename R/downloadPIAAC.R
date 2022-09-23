#' @title Download and Unzip PIAAC Files
#'
#' @description Uses an Internet connection to download PIAAC data to a
#'              computer. Data come from the OECD website. 
#' 
#' @param root a character string indicating the directory where the PIAAC data should
#'             be stored. Files are placed in a folder named PIAAC/cycle [cycle number].
#' @param cycle a numeric value indicating the assessment cycle to download.
#'              Valid cycle is 1 only.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' 
#' @importFrom readxl read_excel
#' @importFrom utils browseURL
#' @author Eric Buehler, Paul Bailey, and Trang Nguyen
#' @example man/examples/downloadPIAAC.R
#' @export
downloadPIAAC <- function(root, cycle=1, cache=FALSE, verbose=TRUE) {
  fixTimeout()
  valid_cycles <- 1
  if (!cycle %in% valid_cycles) {
    stop("PIAAC is not available for this cycle.")
  }
  root <- normalizePath(root)
  root <- gsub("/$","", root)
  dirname <- file.path(root,"PIAAC")

  if(!dir.exists(dirname)) {
    dir.create(dirname)
  }
  yroot <- file.path(dirname, paste0("Cycle ",cycle))
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  if(cycle == 1) {
    data_files <- c("/piaac/puf-data/CSV/prgautp1.csv", "/piaac/puf-data/CSV/prgbelp1.csv",
                    "/piaac/puf-data/CSV/prgcanp1.csv", "/piaac/puf-data/CSV/prgchlp1.csv",
                    "/piaac/puf-data/CSV/prgczep1.csv", "/piaac/puf-data/CSV/prgdeup1.csv",
                    "/piaac/puf-data/CSV/prgdnkp1.csv", "/piaac/puf-data/CSV/prgecup1.csv",
                    "/piaac/puf-data/CSV/prgespp1.csv", "/piaac/puf-data/CSV/prgestp1.csv",
                    "/piaac/puf-data/CSV/prgfinp1.csv", "/piaac/puf-data/CSV/prgfrap1.csv",
                    "/piaac/puf-data/CSV/prggbrp1.csv", "/piaac/puf-data/CSV/prggrcp1.csv",
                    "/piaac/puf-data/CSV/prghunp1.csv", "/piaac/puf-data/CSV/prgirlp1.csv",
                    "/piaac/puf-data/CSV/prgisrp1.csv", "/piaac/puf-data/CSV/prgitap1.csv",
                    "/piaac/puf-data/CSV/prgjpnp1.csv", "/piaac/puf-data/CSV/prgkazp1.csv",
                    "/piaac/puf-data/CSV/prgkorp1.csv", "/piaac/puf-data/CSV/prgltup1.csv",
                    "/piaac/puf-data/CSV/prgmexp1.csv", "/piaac/puf-data/CSV/prgnldp1.csv",
                    "/piaac/puf-data/CSV/prgnorp1.csv", "/piaac/puf-data/CSV/prgnzlp1.csv",
                    "/piaac/puf-data/CSV/prgperp1.csv", "/piaac/puf-data/CSV/prgpolp1.csv",
                    "/piaac/puf-data/CSV/prgrusp1.csv", "/piaac/puf-data/CSV/prgsgpp1.csv",
                    "/piaac/puf-data/CSV/prgsvkp1.csv", "/piaac/puf-data/CSV/prgsvnp1.csv",
                    "/piaac/puf-data/CSV/prgswep1.csv", "/piaac/puf-data/CSV/prgturp1.csv",
                    "/skills/piaac/data/CSV_prgusap1.zip", "/piaac/puf-data/CSV/Prgusap1_2017.csv")
  }
  
  url0 <- "https://webfs.oecd.org"
  codebook <- 'https://www.oecd.org/skills/piaac/International%20Codebook_PIAAC%20Public-use%20File%20(PUF)%20Variables%20and%20Values.xlsx' 
  for (f in data_files) {
    fn <- basename(f)
    if(!file.exists(file.path(yroot,fn))) {
      # us12_14 download is a zip file and needs special processing
      if(f == "/skills/piaac/data/CSV_prgusap1.zip"){
        download.file(paste0("https://www.oecd.org/", f), file.path(yroot, fn), mode = "wb", method = "auto")
        unzip(file.path(yroot, fn), "prgusap1.csv", exdir = file.path(yroot))
      } else {
        download.file(paste0(url0, f), file.path(yroot, fn), mode = "w", method = "auto")
      }
    } else {
      if (verbose) {
        cat(paste0("Found downloaded cycle ",cycle, " PIAAC file ",fn, ".\n"))
      }
    }
  }
  fn <- "international-codebook.xlsx"
  if(!file.exists(file.path(yroot,fn))) {
    download.file(codebook,file.path(yroot,fn), mode = "wb")
  } else {
    if (verbose) {
      cat(paste0("Found downloaded cycle ",cycle," PIAAC codebook file ",fn, ".\n"))
    }
  }
  
  test <- tryCatch(read_excel(file.path(yroot,fn), sheet = 1, progress = FALSE),
                   error = function(cond) {
                     cache <<- FALSE
                     cat(paste0("The downloaded codebook file is corrupt. You need to manually download the codebook at the given link: ",sQuote(codebook)," to the folder ", sQuote(yroot),".\n"))
                     nav <- readline(prompt = "Please enter 'Y' if you wish to launch this URL in your browser: ")
                     if (tolower(trimws(nav)) == "y") {
                       browseURL(codebook)
                     }
                   })
  if (cache) {
    notUsed <- readPIAAC(yroot, countries = "*", verbose = verbose)
    notUsed <- NULL
  }
}
