#' @title Download and Unzip TIMSS Advanced Files
#'
#' @description Uses an Internet connection to download TIMSS Advanced data.
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 1995, 2008, and 2015 data.
#'
#' @param root a character string indicating the directory where the TIMSS Advanced
#'             data should be stored. Files are placed in a
#'             subdirectory named TIMSSAdv/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 1995, 2008, and 2015.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' @author  Tom Fink
#' @seealso \code{\link{readTIMSSAdv}}
#' @example man/examples/downloadTIMSSAdv.R
#' @importFrom utils download.file
#' @export
downloadTIMSSAdv <- function(root, years = c(1995, 2008, 2015), cache = FALSE, verbose = TRUE) {
  fixTimeout()
  if (is.null(root)) {
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if (length(unlist(root)) != 1) {
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }

  # normalize path before testing file.path will remove trailing seperator if present
  root <- suppressWarnings(file.path(normalizePath(root, winslash = "/")))
  if (!dir.exists(root)) {
    stop(paste0("The argument ", sQuote("root"), " must be a valid path."))
  }

  validYears <- c(1995, 2008, 2015)
  if (length(years) > 1) {
    for (yi in years) {
      downloadTIMSSAdv(years = yi, root = root, cache = cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d1995 <- c("https://timssandpirls.bc.edu/timss_advanced/downloads/TA95_SPSS_Data.zip")
  d2008 <- c("https://timssandpirls.bc.edu/timss_advanced/downloads/TA08_SPSS_Data.zip")
  d2015 <- c("https://timssandpirls.bc.edu/timss2015/advanced-international-database/downloads/TA15_SPSSData.zip")

  if (!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }
  # check if folder level exists
  if (!dir.exists(file.path(root, "TIMSSAdv"))) {
    dir.create(file.path(root, "TIMSSAdv"))
  }
  yroot <- file.path(root, "TIMSSAdv", paste0(year)) # build yroot with file.path to avoid issues with seperators
  if (!dir.exists(yroot)) {
    dir.create(yroot)
  }

  d <- get(paste0("d", year))
  for (di in seq_along(d)) {
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
      if (verbose == TRUE) {
        cat(paste0("Found downloaded ", year, " TIMSS Advanced file ", sQuote(bn), ".\n"))
      }
    }
    lst <- unzip(file.path(yroot, bn), list = TRUE) # just lists the files

    if (verbose == TRUE) {
      cat(paste0("Unzipping ", year, " TIMSS Advanced files from ", sQuote(bn), ".\n"))
    }

    for (i in 1:nrow(lst)) {
      if (!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
        if (verbose == TRUE) {
          cat(paste0("  Unzipping ", sQuote(lst$Name[i]), ".\n"))
        }

        unzip(file.path(yroot, bn), files = lst$Name[i], exdir = yroot)
        if (basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(file.path(yroot, lst$Name[i]), file.path(yroot, basename(lst$Name[i])))
        }
      }
    }
  }

  if (cache) {
    if (verbose == TRUE) {
      cat("Caching ", year, " TIMSS Advanced files.\n")
    }

    notUsed <- readTIMSSAdv(yroot, countries = "*", subject = "math", verbose = verbose)
    notUsed <- NULL # clear memory space

    notUsed <- readTIMSSAdv(yroot, countries = "*", subject = "physics", verbose = verbose)
    notUsed <- NULL # clear memory space

    return(invisible(NULL))
  }

  return(invisible(NULL))
}
