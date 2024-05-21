#' @title Download and Unzip PIRLS Files
#'
#' @description Uses an Internet connection to download PIRLS data.
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 2001, 2006, 2011, 2016, and 2021 data.
#'
#' @param root a character string indicating the directory where the PIRLS
#'             data should be stored. Files are placed in a
#'             subdirectory named PIRLS/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 2001, 2006, 2011, 2016, and 2021.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @author Tom Fink
#' @seealso \code{\link{readPIRLS}}
#' @example man\examples\downloadPIRLS.R
#' @importFrom utils download.file
#' @export
downloadPIRLS <- function(root, years = c(2001, 2006, 2011, 2016, 2021), cache = FALSE, verbose = TRUE) {
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

  validYears <- c(2001, 2006, 2011, 2016, 2021)
  if (length(years) > 1) {
    for (yi in years) {
      downloadPIRLS(years = yi, root = root, cache = cache, verbose = verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }

  d2001 <- c("https://timssandpirls.bc.edu/pirls2001i/Pirls2001Database/pirls_2001_spssdata.zip")

  d2006 <- c("https://timssandpirls.bc.edu/PDF/PIRLS2006_SPSSData.zip")

  d2011 <- c(
    "https://timssandpirls.bc.edu/pirls2011/downloads/P11_SPSSData_pt1.zip",
    "https://timssandpirls.bc.edu/pirls2011/downloads/P11_SPSSData_pt2.zip"
  )

  d2016 <- c(
    "https://timssandpirls.bc.edu/pirls2016/international-database/downloads/P16_SPSSData_pt1.zip",
    "https://timssandpirls.bc.edu/pirls2016/international-database/downloads/P16_SPSSData_pt2.zip",
    "https://timssandpirls.bc.edu/pirls2016/international-database/downloads/PL16_SPSSData.zip"
  )
  
  d2021 <- c(
    "https://pirls2021.org/data/downloads/P21_Data_SPSS.zip"
  )

  if (!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }

  # check if base folder exists first
  if (!dir.exists(file.path(root, "PIRLS"))) {
    dir.create(file.path(root, "PIRLS"))
  }
  yroot <- file.path(root, "PIRLS", paste0(year)) # build yroot with file.path to avoid issues with seperators
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
        cat(paste0("Found downloaded ", year, " PIRLS file ", sQuote(bn), ".\n"))
      }
    }


    lst <- unzip(file.path(yroot, bn), list = TRUE) # just lists the files

    if (verbose == TRUE) {
      cat(paste0("Unzipping ", year, " PIRLS files from ", sQuote(bn), ".\n"))
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
      cat("Caching ", year, " PIRLS files.\n")
    }

    notUsed <- readPIRLS(yroot, countries = "*", verbose = verbose)
    notUsed <- NULL # clear memory space

    return(invisible(NULL))
  }

  return(invisible(NULL))
}
