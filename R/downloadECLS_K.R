#' @title Download and Unzip ECLS_K Files
#'
#' @description Uses an Internet connection to download ECLS_K data.
#'              Data come from \href{https://nces.ed.gov/edat/}{nces.ed.gov} zip files. This
#'              function works for 1998 and 2011 data.
#'
#' @param root a character string indicating the directory where the ECLS_K
#'             data should be stored. Files are placed in a
#'             subdirectory named ECLS_K/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 1998 and 2011.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details
#' Beginning for the ECLS_K 2011 Study Grade 5 data files, the \code{ChildK5p.zip} source data file is a \code{DEFLATE64} compressed zip file.
#' This means that the user must manually extract the contained \code{childK5p.dat} file using an external zip
#' program capable of handling \code{DEFLATE64} zip format. As existing R functions are unable to handle this zip format natively.
#'
#' @author Tom Fink
#' @seealso \code{\link{readECLS_K1998}} and \code{\link{readECLS_K2011}}
#' @example man\examples\downloadECLS_K.R
#' @importFrom utils download.file
#' @export
downloadECLS_K <- function(root, years = c(1998, 2011), cache = FALSE, verbose = TRUE) {
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

  validYears <- c(1998, 2011)
  if (length(years) > 1) {
    for (yi in years) {
      downloadECLS_K(years = yi, root = root, cache = cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d1998 <- c(
    "https://nces.ed.gov/edat/data/zip/ECLSK_1998-99_v1_0_ASCII_Datasets.zip",
    "https://nces.ed.gov/edat/data/zip/ECLSK_1998-99_v1_0_CodeBook_Layout.zip"
  )

  d2011 <- d <- c(
    "https://nces.ed.gov/ecls/data/2019/ChildK5p.zip",
    "https://nces.ed.gov/ecls/data/2019/ECLSK2011_K5PUF.sps"
  )

  if (!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }

  # check if base folder exists first
  if (!dir.exists(file.path(root, "ECLS_K"))) {
    dir.create(file.path(root, "ECLS_K"))
  }
  yroot <- file.path(root, "ECLS_K", paste0(year)) # build yroot with file.path to avoid issues with seperators
  if (!dir.exists(yroot)) {
    dir.create(yroot)
  }

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
      if (verbose) {
        cat(paste0("Found downloaded ", year, " ECLS_K file ", sQuote(bn), ".\n"))
      }
    }

    if (grepl("\\.zip$", bn, ignore.case = TRUE)) {
      lst <- unzip(file.path(yroot, bn), list = TRUE) # just lists the files

      if (verbose) {
        cat(paste0("Unzipping ", year, " ECLS_K files from ", sQuote(bn), ".\n"))
      }

      for (i in 1:nrow(lst)) {
        # check that the file is not present in root folder OR that the file sizes are different indicating a file change/corruption
        if (!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
          if (verbose) {
            cat(paste0("  Unzipping ", sQuote(lst$Name[i]), ".\n"))
          }

          tryCatch(unzip(file.path(yroot, bn), files = lst$Name[i], exdir = yroot, unzip = getOption("unzip")),
            warning = function(w) {
              if (w$message == "zip file is corrupt") {
                message("Zip format for file ", file.path(yroot, bn), " is not supported. Users need to manually unzip this file. See ?downloadECLS_K for more information.")
              } else {
                warning(w)
              }
            }
          )

          if (basename(lst$Name[i]) != lst$Name[i]) {
            file.rename(file.path(yroot, lst$Name[i]), file.path(yroot, basename(lst$Name[i])))
          }
        }
      }
    } # end if(grepl("\\.zip$", bn, ignore.case = TRUE))
  } # end for(di in seq_along(d))

  if (cache) {
    if (verbose) {
      cat("Caching ", year, " ECLS_K files.\n")
    }

    # filename and layoutFilename will use default function parameters
    if (year == 1998) {
      notUsed <- readECLS_K1998(path = yroot, verbose = verbose)
    }
    if (year == 2011) {
      notUsed <- tryCatch(readECLS_K2011(path = yroot, verbose = verbose),
        error = function(e) {
          warning("Caching is unavailable for ECLS-K 2011 dataset.  See ?downloadECLS_K documentation for further details.")
        }
      )
    }
  }

  return(invisible(NULL))
}
