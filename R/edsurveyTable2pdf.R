#' @title PDF File From an edsurveyTable
#'
#' @description Produces the LaTeX code and compiles to a PDF file from the \code{edsurveyTable} results.
#'
#' @param data the result of a call to \code{\link{edsurveyTable}}
#' @param formula a formula of the form \code{LHS ~ RHS} to cast the \code{edsurveyTable}
#'                results from long format to wide format. This formula takes the form
#'                \code{LHS ~ RHS} (e.g., \code{var1 + var2 ~ var3}).
#'                The order of the entries in the formula is essential.
#' @param caption character vector of length one or two containing the table's caption or title.
#'                If the length is two, the second item is the \dQuote{short caption} used when LaTeX generates
#'                a \code{List of Tables}.
#'                Set to \code{NULL} to suppress the caption. Default value is \code{NULL}.
#' @param filename a character string containing filenames and paths. By default (\code{filename = ""}),
#'                 table will be saved in the working directory (\code{getwd()}).
#'                 Use \code{filename = "CONSOLE"} to
#'                 print LaTeX code in R console without generating a PDF file.
#' @param toCSV a character string containing filenames and paths of .csv table output.
#'              \code{""} indicates no .csv output. \code{toCSV} is
#'              independent to \code{filename}, so both
#'              a csv file and PDF file would be generated if both \code{filename}
#'              and \code{toCSV} were specified.
#' @param returnMeans a logical value set to \code{TRUE} (the default) to generate a PDF with
#' the \code{MEAN} and \code{SE(MEAN)}. It is set to \code{FALSE} to generate a PDF with
#' the \code{PCT} and \code{SE(PCT)}. See Value
#' in \code{\link[EdSurvey]{edsurveyTable}}.
#' @param estDigits an integer indicating the number of decimal places to be used for estimates.
#' Negative values are allowed. See Details.
#' @param seDigits an integer indicating the number of decimal places to be used for standard errors.
#' Negative values are allowed.
#'
#' @details
#'
#' Rounding to a negative number of digits means rounding to a power of 10,
#' so, for example, \code{estDigits = -2} rounds estimates to the nearest hundred.
#'
#' @note
#' For more details, see the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-LaTeXtables.pdf}{\emph{Producing \code{LaTeX} Tables From \code{edsurveyTable} Results With \code{edsurveyTable2pdf}}}.
#'
#' @example /man/examples/edsurveyTable2pdf.R
#' @author Huade Huo
#'
#' @importFrom tools texi2pdf
#' @importFrom data.table dcast
#' @importFrom xtable xtable
#'
#' @export
edsurveyTable2pdf <- function(data,
                              formula,
                              caption = NULL,
                              filename = "",
                              toCSV = "",
                              returnMeans = TRUE,
                              estDigits = 2,
                              seDigits = 3) {
  # Test incoming data
  if (!inherits(data, c("edsurveyTable", "edsurveyTableList"))) {
    stop(paste0(
      sQuote("data"),
      " must be an edsurveyTable or edsurveyTableList"
    ))
  }

  preambleList <- c(
    "\\documentclass[10pt]{article}",
    "\\usepackage{amsmath,amssymb,amsfonts}",
    "\\usepackage[usenames,dvipsnames]{xcolor}",
    "\\usepackage{pifont}",
    "\\usepackage[defaultsans]{opensans}"
  )

  preamble <- paste(preambleList, collapse = "\n")
  # Read data frame inside edsurveyTable result
  edsurveyTableDF <- data$data
  edsurveyTableVars <- all.vars(data$formula)

  # Test if variables in formula exist in edsurveyTable
  if (!all(all.vars(formula) %in% edsurveyTableVars)) {
    stop(paste0(
      "Could not find variable ",
      paste(all.vars(formula)[!all.vars(formula) %in% edsurveyTableVars],
        collapse = ", "
      )
    ))
  }

  # Paste estimates and SEs
  for (i in 1:length(edsurveyTableDF)) {
    if (returnMeans) {
      edsurveyTableDF$textValue[i] <- paste(
        formatC(
          round(edsurveyTableDF$MEAN[i],
            digits = estDigits
          ),
          format = "f", digits = estDigits
        ),
        " (",
        formatC(
          round(edsurveyTableDF$"SE(MEAN)"[i],
            digits = seDigits
          ),
          format = "f", digits = seDigits
        ),
        ")",
        sep = ""
      )
    } else {
      edsurveyTableDF$textValue[i] <- paste(
        formatC(
          round(edsurveyTableDF$PCT[i],
            digits = estDigits
          ),
          format = "f", digits = estDigits
        ),
        " (",
        formatC(
          round(edsurveyTableDF$"SE(PCT)"[i],
            digits = seDigits
          ),
          format = "f", digits = seDigits
        ),
        ")",
        sep = ""
      )
    }
  }

  # Reshape to from long to wide
  edsurveyTableDF <- dcast(as.data.table(edsurveyTableDF), formula, value.var = "textValue")

  # Replace variable names to variable labels
  for (i in 1:length(names(edsurveyTableDF))) {
    if (!is.null(attr(data$data[[names(edsurveyTableDF)[i]]], "label"))) {
      names(edsurveyTableDF)[i] <- attr(data$data[[names(edsurveyTableDF)[i]]], "label")
    }
  }

  # Save table as csv
  # Also provide csv location to user
  if (toCSV != "") {
    write.csv(edsurveyTableDF, file = toCSV)
    cat(paste("% .csv table saved in ", toCSV, "\n", sep = ""))
  }

  # Create a temp file for tex file
  # Save temp file name for renaming (@param filename)
  tempFile <- tempfile()
  texTempFile <- paste(tempFile, ".tex", sep = "")
  pdfTempFile <- paste(basename(tempFile), ".pdf", sep = "")

  # Provide output location to user
  if (filename == "CONSOLE") {
    cat(paste("% LaTeX script for EdSurvey Table:\n"))
  } else if (filename != "") {
    cat(paste("% Output table will be saved in ", filename, "\n", sep = ""))
  } else {
    cat(paste("% Output table will be saved in ", getwd(),
      "/", pdfTempFile, "\n",
      sep = ""
    ))
  }

  table <- print(xtable(edsurveyTableDF, caption = caption),
    include.rownames = FALSE,
    caption.placement = "top",
    comment = FALSE
  )

  # Paste header, table, and end of document together
  document <- paste(preamble,
    "\\begin{document}",
    table,
    "\\end{document}",
    sep = "\n"
  )

  if (filename != "CONSOLE") {
    write(document, file = texTempFile)

    # Compile LaTeX file
    texi2pdf(texTempFile, clean = TRUE)

    # Moving file from getwd() to user selected path
    if (filename != "") {
      file.rename(
        from = pdfTempFile,
        to = filename
      )
    }
  }
}
