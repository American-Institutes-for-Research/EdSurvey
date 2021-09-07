#' @title EdSurvey Metadata Summary
#'
#' @description Prints metadata regarding an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' 
#' @param x             an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @param printColnames a logical value; set to \code{TRUE} to see all column names in the \code{edsurvey.data.frame}
#'                      or the \code{edsurvey.data.frame.list}
#' @param ... these arguments are not passed anywhere and are included only for compatibility
#' 
#' @author Michael Lee and Paul Bailey
#' @method print edsurvey.data.frame
#' @aliases print.edsurvey.data.frame.list
#' @export
print.edsurvey.data.frame <- function(x, printColnames = FALSE, ...) {
  if (!inherits(x, "edsurvey.data.frame")) {
    stop(paste0(sQuote("x"), " must be an edsurvey.data.frame"))
  }
  dm <- dim(x)
  txt <- paste0("edsurvey.data.frame for ", x$year, " ", x$survey, " (",
                pasteItems(x$subject),") in ", x$country, "\n")
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
    eout(paste0("Omitted Levels: ",
                pasteItems(sQuote(unlist(x$omittedLevels))),
                "\n"),
         exdent=nchar("Omitted Levels: "))
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

  al <- getAttributes(x, "achievementLevels")
  ### Handle more than 1 achievement level scales
  if(is.list(al)) { 
    for (ali in 1:length(al)) {
      eout("\nAchievement Levels:\n")
      eout(paste0(names(al)[ali],": \n"))
      if(length(al[[ali]]) == 0) {
        eout("  Achievement levels for this subject is not defined this year. \n") 
      } else {
        noms <- names(al[[ali]])
        for(i in 1:length(al[[ali]])) {  
          post <- paste(rep(" ",1+max(nchar(noms)) - nchar(noms[i])), collapse="")
          eout(paste0("  ", noms[i], ":", post, sprintf("%.2f", al[[ali]][i]), "\n"))
        }  
      }
    }
  } else {
    if(length(al) > 0) {
      noms <- names(al)
      eout("\nAchievement Levels:\n")
      for(i in 1:length(al)) {  
        post <- paste(rep(" ",1+max(nchar(noms)) - nchar(noms[i])), collapse="")
        eout(paste0("  ",noms[i],":",post, al[i], "\n"))
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
  for(i in 1:li) {
    eout(paste0("\n\nElement ",i,"\n\n"))
    print(x$data[[i]])
  }
}
