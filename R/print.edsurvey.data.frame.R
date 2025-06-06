#' @title EdSurvey Metadata Summary
#'
#' @description Prints metadata regarding an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#'
#' @param x             an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @param printColnames a logical value; set to \code{TRUE} to see all column names in the \code{edsurvey.data.frame}
#'                      or the \code{edsurvey.data.frame.list}
#' @param use_es_round a logical; round the output per \code{\link{es_round}} function
#' @param round_n function used to round sample n-sizes. See \code{\link{es_round}}
#' @param ... these arguments are not passed anywhere and are included only for compatibility
#'
#' @author Michael Lee and Paul Bailey
#' @method print edsurvey.data.frame
#' @aliases print.edsurvey.data.frame.list
#' @export
print.edsurvey.data.frame <- function(x, printColnames = FALSE, use_es_round=getOption("EdSurvey_round_output"), round_n=getOption("EdSurvey_round_n_function"), ...) {
  if (!inherits(x, "edsurvey.data.frame")) {
    stop(paste0(sQuote("x"), " must be an edsurvey.data.frame"))
  } 
  dm <- dim(x)
  if(use_es_round) {
    lst <- list(dm1=dm[1])
    if(missing("round_n")){
      round_n <- getOption("EdSurvey_round_n_function")
    }
    lst <- es_round_with(lst, "dm1", round_n, ...)
    dm[1] <- lst$dm[1]
  }
  parenText <- paste0(pasteItems(x$subject))
  if ("gradeLevel" %in% names(x) && length(x$gradeLevel) > 0) {
    parenText <- paste0(parenText, "; ", pasteItems(x$gradeLevel))
  }
  surveyText <- x$survey
  if ("assessmentCode" %in% names(x) && length(x$assessmentCode) > 0) {
    surveyText <- paste0(surveyText, " ", pasteItems(x$assessmentCode))
  }
  txt <- paste0(
    "edsurvey.data.frame for ", x$year, " ", surveyText, " (",
    parenText, ") in ", x$country, "\n"
  )
  txt <- paste0(txt, "\n")
  eout(txt)

  eout(paste0("Dimensions: ", dm[1], " rows and ", dm[2], " columns.\n"))

  if (printColnames) {
    eout("Column names:\n")
    print(names(x$data))
  }
  cat("\n")

  if (length(x$weights) > 0) {
    showWeights(x, verbose = FALSE)
  }

  if (length(x$pvvars) > 0) {
    cat("\n")
    showPlausibleValues(x, verbose = FALSE)
  }

  if (length(x$omittedLevels) > 0) {
    cat("\n")
    eout(
      paste0(
        "Omitted Levels: ",
        pasteItems(sQuote(unlist(x$omittedLevels))),
        "\n"
      ),
      exdent = nchar("Omitted Levels: ")
    )
  }

  # Describe user in put conditions
  if (length(x$userConditions) > 0) {
    cat("\n")
    eout("User Conditions:\n")
    description <- x$userConditions
    eout(paste0(paste0("  ", description, collapse = "\n"), "\n"))
  }

  if (length(x$defaultConditions) > 0) {
    cat("\n")
    eout("Default Conditions:\n")
    description <- x$defaultConditions
    eout(paste0("  ", description, "\n"))
  }

  if (length(x$recodes) > 0) {
    cat("\n")
    eout("Recodes:\n")
    description <- x$recodes
    eout(paste0("  ", description, "\n"))
  }

  al <- getAttributes(x, "achievementLevels", errorCheck = FALSE)
  ### Handle more than 1 achievement level scales
  if (is.list(al)) {
    for (ali in seq_along(al)) {
      eout("\nAchievement Levels:\n")
      eout(paste0(names(al)[ali], ": \n"))
      if (length(al[[ali]]) == 0) {
        eout("  Achievement levels for this subject are not defined. \n")
      } else {
        noms <- names(al[[ali]])
        for (i in seq_along(al[[ali]])) {
          post <- paste(rep(" ", 1 + max(nchar(noms)) - nchar(noms[i])), collapse = "")
          eout(paste0("  ", noms[i], ":", post, sprintf("%.2f", al[[ali]][i]), "\n"))
        }
      }
    }
  } else {
    if (length(al) > 0) {
      noms <- names(al)
      eout("\nAchievement Levels:\n")
      for (i in seq_along(al)) {
        post <- paste(rep(" ", 1 + max(nchar(noms)) - nchar(noms[i])), collapse = "")
        eout(paste0("  ", noms[i], ":", post, al[i], "\n"))
      }
    }
  }
}

#' @export
#' @method print edsurvey.data.frame.list
print.edsurvey.data.frame.list <- function(x, printColnames = FALSE, ...) {
  li <- length(x$data)
  eout(paste0("an edsurvey.data.frame.list with ", li, " elements\n"))
  eout("covs:\n")
  print(x$covs)
  for (i in 1:li) {
    eout(paste0("\n\nElement ", i, "\n\n"))
    print(x$data[[i]])
  }
}
