#' @title Download and Unzip ePIRLS Files
#'
#' @description Uses an Internet connection to download ePIRLS data.
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 2016 data.
#'
#' @param root a character string indicating the directory where the ePIRLS
#'             data should be stored. Files are placed in a
#'             subdirectory named ePIRLS/[year].
#' @param years an integer vector of the assessment years to download. Valid year is 2016 only.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @author Tom Fink
#' @seealso \code{\link{read_ePIRLS}}
#' @example man/examples/download_ePIRLS.R
#' @importFrom utils download.file
#' @export
download_ePIRLS <- function(root, years = c(2016), cache = FALSE, verbose = TRUE) {
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

  validYears <- c(2016)
  if (length(years) > 1) {
    for (yi in years) {
      download_ePIRLS(years = yi, root = root, cache = cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d2016 <- c("https://timssandpirls.bc.edu/pirls2016/international-database/downloads/eP16_SPSSData.zip")

  if (!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }

  # check if base folder exists first
  if (!dir.exists(file.path(root, "ePIRLS"))) {
    dir.create(file.path(root, "ePIRLS"))
  }
  # build yroot with file.path to avoid issues with seperators
  yroot <- file.path(root, "ePIRLS", paste0(year))
  if (!dir.exists(yroot)) {
    dir.create(yroot)
  }

  # keep this flexible to allow multiple years if ePIRLS will have a future follow up
  d <- get(paste0("d", year))

  # loop through each defined .zip file defined for the year
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
        cat(paste0("Found downloaded ", year, " ePIRLS file ", sQuote(bn), ".\n"))
      }
    }

    # just lists the files
    lst <- unzip(file.path(yroot, bn), list = TRUE)

    if (verbose) {
      cat(paste0("Unzipping ", year, " ePIRLS files from ", sQuote(bn), ".\n"))
    }

    for (i in 1:nrow(lst)) {
      # check that the file is not present in root folder OR that the file sizes are different indicating a file change/corruption
      if (!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
        if (verbose) {
          cat(paste0("  Unzipping ", sQuote(lst$Name[i]), ".\n"))
        }

        unzip(file.path(yroot, bn), files = lst$Name[i], exdir = yroot)

        # ensure the file is on the root directory and not in any subfolders if nested in the .zip file
        if (basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(file.path(yroot, lst$Name[i]), file.path(yroot, basename(lst$Name[i])))
        }
      }
    }
  }
  if (cache) {
    if (verbose == TRUE) {
      cat("Caching ", year, " ePIRLS files.\n")
    }
    # reading in the data causes the cache to be written.
    notUsed <- read_ePIRLS(yroot, countries = "*", verbose = verbose)
  }
  return(invisible(NULL))
}
