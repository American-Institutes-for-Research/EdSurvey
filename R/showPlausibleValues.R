#' @title Plausible Value Variable Names
#'
#' @description Prints a summary of the subject scale or subscale and the associated variables for their
#'              plausible values for an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#' @param verbose a logical value; set to \code{TRUE} to get the variable names for plausible values.
#'                Otherwise, prints only the subject scale or subscale names for variables
#'                that use plausible values.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\showPlausibleValues.R
#' @export
showPlausibleValues <- function(data, verbose = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    itterateESDFL(match.call(),data)
    return(invisible(NULL))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  # Return every plausible value via data$pvvars
  pvvars <- getAttributes(data, "pvvars")
  pvNames <- names(pvvars)
  txt <- paste0("There are ", length(pvNames), " subject scale(s) or subscale(s) in this edsurvey.data.frame:\n")
  eout(txt)
  if(is.null(attributes(pvvars)$default))
  attributes(pvvars)$default <- "" # should not resolve to a test
  if (length(pvNames) == 0) {
    return(invisible(NULL))
  }
  for (i in 1:length(pvNames)) {
    pvi <- pvvars[[i]]
	pvn <- lapply(names(pvi)[grep("[Vv]arnames", names(pvi))], function(name) {
	  length(pvi[[name]])
	})
	npv <- sum(unlist(pvn))
    txt <- paste0("  ", sQuote(names(pvvars)[i]),
               " subject scale or subscale with ",
               npv, 
               " plausible values")
    if (attributes(pvvars)$default == pvNames[i]) {
      # if there is a default plausible value, return with paste ' (the default)'
      txt <- paste0(txt, " (the default).")
    } else {
      txt <- paste0(txt, ".")
    } # End of if statment if attributes(data$pvvars)$default == pvNames[i]
    writeLines(strwrap(txt,
                       width=getOption("width")*0.9,
                       exdent=2))
    if (verbose) {
      # if verbose = TRUE, return all plausible value details for each subject scale/subscale
      txt <- "The plausible value variables are: "
      pvi <- getPlausibleValue(pvNames[i], data)
      txt <- paste0(txt, pasteItems(paste0("'", pvi, "'")))
      eout(txt, indent=2)
    } # end of is statment: if verbrose 
    cat("\n")
  } # End of loop for i in 1:length(pvNames)
}

#' @title Get Plausible Value Variables
#'
#' @description Gets the set of variables on an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'              an \code{edsurvey.data.frame.list} associated with the given subject or subscale.
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'             an \code{edsurvey.data.frame.list}
#' @param var a character vector naming the subject scale or subscale
#'
#' @return a character vector of the set of variable names for the plausible values
#'
#' @details This function will return a set of plausible value names for variables that
#' \code{\link{hasPlausibleValue}} returns as true.
#'
#' @seealso \code{\link{showPlausibleValues}}, \code{\link{updatePlausibleValue}}
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\getPlausibleValue.R
#' @export
getPlausibleValue <- function(var, data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if(any(!hasPlausibleValue(var, data))) {
    stop("The ", sQuote("var"), " argument level of ", dQuote(var), " does not have plausible values.")
  }

  # get the list of attributes about PV variables
  pv <- getAttributes(data, "pvvars")
  # extract just the variable names (on the data) for the variables in question
  pvi <- lapply(var, function(vn) {
    if(grepl("_linking", vn, fixed=TRUE)) {
	  c(pv[[vn]]$estVarnames, pv[[vn]]$impVarnames, pv[[vn]]$sampVarnames)
    } else {
      pv[[vn]]$varnames
	}
  } )
  
  # turn them into a single vector and return; return a NULL warning if any of the pvs are NULL (they shouldn't ever be though)
  if(any(is.null(pvi))) {
    surveyPaste <- getAttributes(data, "survey")
    yearPaste <- getAttributes(data, "year")
    warning(paste(surveyPaste, yearPaste, "returns a NULL plausible value."))
    return(unname(unlist(pvi)))
  } else {
    return(unname(unlist(pvi)))
  }
}

#' @title Update Plausible Value Variable Names
#' @description Changes the name used to refer to a set of plausible values from \code{oldVar} to \code{newVar} in an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#' @param oldVar a character value indicating the existing name of the variable
#' @param newVar a character value indicating the new name of the variable
#'
#' @return an object of the same class as the \code{data} argument, with the name of
#'         the plausible value updated from \code{oldVar} to \code{newVar}
#'
#' @seealso \code{\link{getPlausibleValue}} and \code{\link{showPlausibleValues}}
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\updatePlausibleValue.R
#' @export
updatePlausibleValue <- function(oldVar, newVar, data) {
  if(oldVar == newVar) {
    return(data)
  }
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if(!hasPlausibleValue(oldVar, data)) {
    stop(paste0("The argument ", sQuote("oldVar"), " with value ", sQuote(oldVar) ," is not a plausible value variable."))
  }
  if(newVar %in% names(getAttributes(data, "pvvars"))) {
    stop(paste0("Plausible value variable name ", sQuote(newVar), " already in use. Try another name."))
  } 
  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so plausible value is returned in data$pvvars[var]
    names(data$pvvars)[names(data$pvvars) == oldVar] <- newVar
    if(attributes(data$pvvars)$default == oldVar) {
      attributes(data$pvvars)$default <- newVar
    }
  } else {
    # data is a light.edsurvey.data.frame, so plausible value is returned in
    # attributes(data)$pvvars[var]
    names(attributes(data)$pvvars)[names(attributes(data)$pvvars) == oldVar] <- newVar
    if(oldVar %in% attributes(attributes(data)$pvvars)$default) {
      attributes(attributes(data)$pvvars)$default <- newVar
    }
  }
  data
}

#' @title Plausible Value Test
#'
#' @description Returns a value indicating if this variable has associated plausible values in an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'         or an \code{edsurvey.data.frame.list}
#' @param var a character indicating the variable in question
#'
#' @return a Boolean (or vector when \code{var} is a vector) indicating if each element of \code{var} has
#'         plausible values associated with it
#'
#' @details This function returns \code{TRUE} only when the variable passed to it is the name for a set of plausible values but
#'          not if it is an individual plausible value from such a set. Thus, on the NAEP Primer, \code{composite} has plausible
#'          values (and so \code{TRUE} would be returned by this function), but any of the plausible values or variable names defined in
#'          the actual data (such as \code{"mrpcm1"} or \code{"dsex"}) are not.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\hasPlausibleValue.R
#' @export
hasPlausibleValue <- function(var, data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so return whether string is in data$pvvars
    return(var %in% names(data$pvvars))
  } else {
    # data is a light.edsurvey.data.frame, so return whether string is in attributes(data)$pvvars
    return(var %in% names(attributes(data)$pvvars))
  } # end of if statment: if data inherits edsurvey.data.frame
}
