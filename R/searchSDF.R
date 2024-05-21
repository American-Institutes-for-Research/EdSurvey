#' @title EdSurvey Codebook Search
#'
#' @description Retrieves variable names and labels for an \code{edsurvey.data.frame},
#' a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}
#' using character string matching.
#'
#' @param string     a vector of character strings to search for in the database connection object (\code{data}).
#'                   The function will search the codebook
#'                   for a matching character string using regular expressions. When a
#'                   string has several elements, all must be present for a
#'                   variable to be returned.
#' @param data       an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'                   an \code{edsurvey.data.frame.list}
#' @param fileFormat a character vector indicating the data source to search for variables.
#'                   The default \code{NULL} argument searches all codebooks.
#' @param levels     a logical value; set to \code{TRUE} to return a snapshot of the levels in
#'                   an \code{edsurvey.data.frame}
#' @return           a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}} that shows the variable names, labels,
#'                   and levels (if applicable) from an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame} based on a matching character string
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\searchSDF.R
#' @export
searchSDF <- function(string, data, fileFormat = NULL, levels = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    call0 <- match.call()
    resl <- lapply(data$data, function(li) {
      call0$data <- li
      tryCatch(eval(call0),
        error = function(cond) {
          message(paste("An error occurred while working on a dataset. Excluding results from this dataset."))
          message(cond)
          # returning zero allows unlist and other operations that remove e.g. NULLs
          return(0)
        },
        warning = function(w) {
          return(1)
        }
      )
    })
    res <- data.frame()
    for (i in seq_along(resl)) {
      ires <- resl[[i]]
      covs <- data$covs
      covn <- colnames(covs)
      if (inherits(ires, "data.frame")) {
        if (nrow(ires) > 0) {
          newCol <- paste(covs[i, covn], collapse = ";")
          ires[ , newCol] <- "*"
          if (nrow(res) == 0) {
            res <- ires
          } else {
            res <- merge(res, ires, all = TRUE)
          }
        }
      }
    }
    res[is.na(res)] <- ""
    return(res)
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL
  # get the dataList
  if (inherits(sdf, c("edsurvey.data.frame"))) {
    dataList <- sdf$dataList
  } else {
    warning(paste0(
      "Searched for string(s) ", paste(sQuote(string), collapse = ", "),
      " only in this light.edsurvey.data.frame. To search the full data set, change ",
      paste0(sQuote("data")), " argument to the edsurvey.data.frame."
    ))
    dataList <- attributes(sdf)$dataList
  }

  if (is.null(fileFormat)) {
    # bind and search fileFormats in both student and school if not defined
    labelsFile <- do.call("rbind", lapply(dataList, function(dl) {
      ff <- dl$fileFormat
      ff$fileFormat <- dl$levelLabel
      subset(ff, !(variableName %in% dl$ignoreVars))
    }))
    # remove irrelevant columns from light.edsurvey.data.frame
    if (!inherits(sdf, c("edsurvey.data.frame"))) {
      labelsFile <- labelsFile[(toupper(labelsFile$variableName) %in% toupper(colnames(sdf))), ]
    }
  } else {
    fileFormat <- tolower(fileFormat)
    names(dataList) <- tolower(names(dataList))
    if (!all(fileFormat %in% names(dataList))) {
      stop(paste0("The ", sQuote("fileFormat"), " argument must either be one of ", paste(dQuote(names(dataList)), collapse = " or "), "."))
    }

    dlIdx <- which(names(dataList) %in% fileFormat, arr.ind = TRUE) # get appropriate data levels
    dataList <- dataList[dlIdx]
    labelsFile <- do.call("rbind", lapply(dataList, function(dl) {
      ff <- dl$fileFormat
      ff$fileFormat <- dl$levelLabel
      subset(ff, !(variableName %in% dl$ignoreVars))
    }))
  } # end else for if(is.null(fileFormat))
  for (si in string) {
    # apply each string
    labelsFile <- labelsFile[grepl(si, labelsFile$variableName, ignore.case = TRUE) |
      grepl(si, labelsFile$Labels, ignore.case = TRUE), ]
  }

  if (is.data.frame(labelsFile) & nrow(labelsFile) == 0) {
    # return a warning if there are no variables in the fileFormat that match the string
    # searched
    strg <- ifelse(length(string) > 1, "strings", "string")
    warning(paste0("There are no variables containing the ", strg, " ", pasteItems(sQuote(string)), " in this edsurvey.data.frame or light.edsurvey.data.frame."))
    return(NULL)
  }
  # return variables that match the string searched
  labelsFile$variableName <- tolower(labelsFile$variableName)
  labelsFile$Levels <- NA
  if (levels == TRUE) {
    # return levels of each of the variables
    if (length(string) == 1 && string == "") {
      stop(paste0("The argument ", sQuote("string"), " must be nonempty to return variable levels."))
    }
    # create a dataframe with file formatting information for selected variables
    varsData <- labelsFile[ , c("variableName", "Labels", "labelValues", "Levels", "fileFormat")]
    if ("light.edsurvey.data.frame" %in% class(sdf) == TRUE) {
      varsData <- varsData[varsData$variableName %in% colnames(sdf), ]
    }
    # remove duplicates such as linking variables
    varsData <- varsData[!duplicated(varsData$variableName), ]
    for (i in seq_along(varsData$variableName)) {
      # return levels of each of the variables; involves splitting and appending levels from the
      # fileFormat file. Note that some variable don't have levels and are read in as ''
      if (varsData$labelValues[[i]] != "") {
        levelsInfo <- levelsSDF(varsData$variableName[[i]], sdf)[[varsData$variableName[[i]]]]
        varLevels <- paste0(levelsInfo$level, ". ", levelsInfo$labels)
        varLevelsSplit <- c()
        for (ii in seq_along(varLevels)) {
          x <- varLevels[[ii]]
          varLevelsSplit <- c(varLevelsSplit, x)
        }
        varLevelsSplitPaste <- paste(varLevelsSplit, collapse = "; ")
        varsData$Levels[[i]] <- varLevelsSplitPaste
      } else {
        varsData$Levels[[i]] <- paste0(NA)
      }
    } # end for (i in seq_along(varsData$variableName))
    varsData <- varsData[ , c("variableName", "Labels", "Levels", "fileFormat")]
    class(varsData) <- c("searchSDF", "data.frame")
  } else { # end if (levels == TRUE)
    # variable levels aren't returned
    labelsFile$variableName <- tolower(labelsFile$variableName)
    varsData <- labelsFile[ , c("variableName", "Labels", "fileFormat")]
    varsData <- data.frame(varsData, stringsAsFactors = FALSE, row.names = NULL)
    # remove duplicates such as linking variables
    varsData <- varsData[!duplicated(varsData$variableName), ]
  } # end else for if (levels == TRUE)
  varsData[] <- lapply(varsData, as.character)
  return(varsData)
}

# @author Michael Lee
#' @method print searchSDF
#' @export
print.searchSDF <- function(x, ...) {
  class(x) <- "data.frame"
  cols <- colnames(x)
  if ("Levels" %in% cols) {
    x[] <- lapply(x, as.character)
    for (i in seq_along(unique(x$variableName))) {
      # loop print function over each unique variable returned in searchSDF
      cat(paste("Variable: ", tolower(x[i, "variableName"]), "\n", sep = "")) # paste the variable name
      cat(paste("Label: ", x[i, "Labels"], "\n", sep = "")) # paste the label name
      if (x$Levels[i] == "NA") {
        cat(paste("\n", sep = ""))
      } else {
        cat(paste("Levels (Lowest level first):\n ", sep = ""))
        labs <- lapply(x$Levels, strsplit, split = ";")
        for (ii in seq_along(labs[[i]][[1]])) {
          # for each unique level, paste the level number and name
          cat(paste("    ", labs[[i]][[1]][ii], "\n", sep = ""))
        }
      }
    }
  } else {
    x[] <- lapply(x, as.character)
    cat(paste(x))
  }
}
