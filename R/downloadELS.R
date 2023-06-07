#' @title Download and Unzip ELS Files
#'
#' @description Uses an Internet connection to download ELS data.
#'              Data come from \href{https://nces.ed.gov/edat/}{nces.ed.gov} zip files. This
#'              function works for 2002 data.
#'
#' @param root a character string indicating the directory where the ELS
#'             data should be stored. Files are placed in a
#'             subdirectory named ELS/[year].
#' @param years an integer vector of the assessment years to download. Valid year is 2002 only.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @author Tom Fink
#' @seealso \code{\link{readELS}}
#' @example man\examples\downloadELS.R
#' @importFrom utils download.file
#' @export
downloadELS <- function(root, years = c(2002), cache = FALSE, verbose = TRUE) {
  fixTimeout()
  if (is.null(root)) {
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if (length(unlist(root)) != 1) {
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }

  # normalize path before testing:: file.path will remove trailing seperator if present
  root <- suppressWarnings(file.path(normalizePath(root, winslash = "/")))
  if (!dir.exists(root)) {
    stop(paste0("The argument ", sQuote("root"), " must be a valid path."))
  }

  validYears <- c(2002)
  if (length(years) > 1) {
    for (yi in years) {
      downloadELS(years = yi, root = root, cache = cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d2002 <- c(
    "https://nces.ed.gov/EDAT/Data/Zip/ELS_2002-12_PETS_v1_0_Student_SPSS_Datasets.zip",
    "https://nces.ed.gov/EDAT/Data/Zip/ELS_2002-12_PETS_v1_0_Student_BRR_SPSS_Datasets.zip",
    "https://nces.ed.gov/EDAT/Data/Zip/ELS_2002-12_PETS_v1_0_Other_SPSS_Datasets.zip"
  )

  if (!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }

  # check if base folder exists first
  if (!dir.exists(file.path(root, "ELS"))) {
    dir.create(file.path(root, "ELS"))
  }
  yroot <- file.path(root, "ELS", paste0(year)) # build yroot with file.path to avoid issues with seperators
  if (!dir.exists(yroot)) {
    dir.create(yroot)
  }

  d <- get(paste0("d", year))
  for (di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if (!file.exists(file.path(yroot, bn))) {
      # download
      tryCatch(download.file(d[di], file.path(yroot, bn), quiet = !verbose, cacheOK = FALSE),
        error = function(e) {
          stop(paste0(
            "Error downloading file at URL: ", sQuote(d[di]), ". ",
            "Message: ", e
          ))
        }
      )
    } else {
      if (verbose) {
        eout(paste0("Found downloaded ", year, " ELS file ", sQuote(bn), "."))
      }
    }

    if (grepl("\\.zip$", bn, ignore.case = TRUE)) {
      lst <- unzip(file.path(yroot, bn), list = TRUE) # just lists the files

      if (verbose) {
        eout(paste0("Unzipping ", year, " ELS files from ", sQuote(bn), "."))
      }

      for (i in 1:nrow(lst)) {
        if (!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
          if (verbose) {
            eout(paste0("  Unzipping ", sQuote(lst$Name[i]), "."))
          }

          unzip(file.path(yroot, bn), files = lst$Name[i], exdir = yroot)
          if (basename(lst$Name[i]) != lst$Name[i]) {
            file.rename(file.path(yroot, lst$Name[i]), file.path(yroot, basename(lst$Name[i])))
          }
        }
      } # end for(i in 1:nrow(lst))
    } # end if(grepl("\\.zip$", bn, ignore.case = TRUE))
  } # end for(di in 1:length(d))

  if (cache) {
    if (verbose) {
      eout("Caching ", year, " ELS files.")
    }

    if (year == 2002) {
      notUsed <- readELS(path = yroot, filename = "els_02_12_byf3pststu_v1_0.sav", wgtFilename = "els_02_12_byf3stubrr_v1_0.sav", verbose = verbose)
      notUsed <- NULL # clear memory space

      notUsed <- readELS(path = yroot, filename = "els_02_12_byf1sch_v1_0.sav", wgtFilename = NULL, verbose = verbose)
      notUsed <- NULL # clear memory space

      notUsed <- readELS(path = yroot, filename = "els_02_12_f2inst_v1_0.sav", wgtFilename = NULL, verbose = verbose)
      notUsed <- NULL # clear memory space

      notUsed <- readELS(path = yroot, filename = "els_02_12_f3inst_v1_0.sav", wgtFilename = NULL, verbose = verbose)
      notUsed <- NULL # clear memory space
    }

    notUsed <- NULL # clear memory space
  }

  return(invisible(NULL))
}
