#' @title Summary Codebook
#'
#' @description Retrieves variable names, variable labels, and value labels for an
#'              \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data            an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'                        an \code{edsurvey.data.frame.list}
#' @param fileFormat      a character string indicating the data source to search for variables.
#'                        The default \code{NULL} argument searches all available codebooks in the database connection object.
#' @param labelLevels     a logical value; set to \code{TRUE} to return a snapshot of the label levels in
#'                        an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}. When set to \code{FALSE}
#'                        (the default), label levels are removed.
#' @param includeRecodes  a logical value; set to \code{TRUE} to return value labels that have been recoded in
#'                        an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}. When set to \code{FALSE}
#'                        (the default), only the original value labels are included in the returned \code{data.frame}.
#' @return                a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}} that shows the variable names, variable labels, value labels,
#'                        value levels (if applicable), and the file format data source from an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'                        or an \code{edsurvey.data.frame.list}
#'
#' @author Michael Lee and Paul Bailey
#' @example man/examples/showCodebook.R
#' @export
showCodebook <- function(data, fileFormat = NULL, labelLevels = FALSE, includeRecodes = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(), data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL

  # bind all fileFormats into a list object
  if (inherits(sdf, c("edsurvey.data.frame"))) {
    dataList <- sdf$dataList
  } else {
    dataList <- attributes(sdf)$dataList
  }

  # include only the data fileFormats that exist in the data connection
  dataList <- dataList[!vapply(dataList, is.null, FUN.VALUE=logical(1))]

  # create an empty data.frame (vars) that will contain our variable information
  vars <- data.frame()
  # if fileFormat is NULL, retrieve all available fileFormats from connection via getAttributes and append
  if (is.null(fileFormat)) {
    for(i in seq_along(dataList)) {
      iVar <- data.frame(dataList[[i]]$fileFormat, fileFormat = names(dataList)[i])
      iVar <- subset(iVar, !iVar$variableName %in% dataList[[i]]$ignoreVars) #remove ignoreVars from result
      vars <- rbind(vars, iVar)
    }
  } else {
    # fileFormat is defined (must be a combination of student, school, or teacher)
    fileFormat <- tolower(fileFormat)
    if (!all(fileFormat %in% tolower(names(dataList)))) {
      stop(paste0("The ", sQuote("fileFormat"), " argument must either be one or more of ", paste(dQuote(names(dataList)), collapse = " or "), "."))
    }
    names(dataList) <- tolower(names(dataList))

    # retrieve all available fileFormats from connection via getAttributes and append
    for(i in seq_along(fileFormat)) {
      iVar <-  data.frame(dataList[[fileFormat[i]]]$fileFormat, fileFormat = fileFormat[i])
      iVar <- subset(iVar, !iVar$variableName %in% dataList[[fileFormat[i]]]$ignoreVars) #remove ignoreVars from result
      vars <- rbind(vars, iVar)
    }
  }

  if (is.data.frame(vars) & nrow(vars) == 0) {
    # return a warning if there is no codebook information available for this data
    warning(paste0("No codebook information available for this data."))
    return(NULL)
  } else {
    # lower the variable names
    vars$variableName <- tolower(vars$variableName)

    # return only variables relevant to codebook
    varsData <- vars[ , c("variableName", "Labels", "labelValues", "fileFormat")]
    if ("light.edsurvey.data.frame" %in% class(sdf) == TRUE) {
      varsData <- varsData[varsData$variableName %in% colnames(sdf), ]
    }

    # function used to include recoded levels to the database connection done by the user via recode.sdf
    parseLevelRecodes <- function(data, variableName, variableLevel) {
      pasteLevels <- function(...) {
        paste(..., sep = "=", collapse = "^")
      }
      for (i in 1:nrow(data)) {
        if (data[[variableLevel]][i] != "") {
          varLevels <- levelsSDF(data[[variableName]][i], data = sdf, showOmitted = FALSE, showN = FALSE)[[1]]
          varLevels <- do.call(pasteLevels, varLevels)
          data$labelValueRecodes[i] <- varLevels
        } else {
          data$labelValueRecodes[i] <- ""
        }
      }
      data
    }

    # the codebook output includes recoded levels (using recode.sdf) done by the user, adding a column "labelValueRecodes" to the returned data.frame
    if (includeRecodes) {
      varsData <- parseLevelRecodes(varsData, variableName = "variableName", variableLevel = "labelValues")
    }

    # if label levels aren't returned AND output should include recodes, parse out the "^" and the value level and replace it with "; "
    if (all(includeRecodes & !labelLevels)) {
      varsData$labelValueRecodes <- as.character(lapply(strsplit(varsData$labelValueRecodes, "^", fixed = TRUE), function(x) {
        paste(vapply(strsplit(x, "\\="), function(z) {
          z[2]
        }, FUN.VALUE=character(1) ), collapse = "; ")
      }))
    }

    # if label levels aren't returned, parse out the "^" and the value level and replace it with "; "
    if (!labelLevels) {
      varsData$labelValues <- as.character(lapply(strsplit(varsData$labelValues, "^", fixed = TRUE), function(x) {
        paste(vapply(strsplit(x, "=", fixed = TRUE), function(z) {
          z[2]
        }, FUN.VALUE=character(1)), collapse = "; ")
      }))
    }

    # if labelLevels are returned, parse out the "^" and replace it with "; "
    if(labelLevels) {
      if(includeRecodes) {
        varsData$labelValueRecodes <- gsub("^", "; ", varsData$labelValueRecodes, fixed=TRUE)
      } 
      varsData$labelValues <- gsub("^", "; ", varsData$labelValues, fixed=TRUE)
    } 
  }
  if (version$major %in% "3" && "fileFormat" %in% colnames(varsData)) {
    varsData$fileFormat <- as.character(varsData$fileFormat)
  }
  return(varsData)
}
