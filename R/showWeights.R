#' @title Retrieve Weight Variables
#'
#' @description Prints a summary of the weights in an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or 
#'         an \code{edsurvey.data.frame.list}
#' @param verbose a logical value; set to TRUE to print the complete list of jackknife
#'                replicate weights associated with each full sample weight;
#'                otherwise, prints only the full sample weights
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\showWeights.R
#' @export
showWeights <- function(data, verbose = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    itterateESDFL(match.call(),data)
    return(invisible(NULL))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  #  1:i-th weight is returned in weights
  weights <- getAttributes(data, "weights")
  wgtNames <- names(weights)
  helper <- ifelse(length(wgtNames) == 1, "is", "are")
  s <- ifelse(length(wgtNames) == 1, "", "s")
  eout(paste0("There ", helper," ", length(wgtNames), " full sample weight",s," in this edsurvey.data.frame:\n"))
  for (i in 1:length(wgtNames)) {
    wgti <- weights[[i]]
    txt <- paste0(sQuote(names(weights)[i]), " with ", length(wgti$jksuffixes), " JK replicate weights")
    if (attributes(weights)$default == wgtNames[i]) {
      # if there is a default weight, return with paste ' (the default)'
      txt <- paste0(txt, " (the default).")
    } else {
      txt <- paste0(txt, ".")
    } # End of if/esle statment: if attributes(weights)$default == wgtNames[i]
    eout(txt, indent=2, exdent=2)

    if (verbose) {
      # if verbose = TRUE, return the jackknife replicate weights using the default weight
      eout(paste0("Jackknife replicate weight variables associated with the full sample weight ",sQuote(names(weights)[i]),":\n"), indent=4, exdent=4)
      jki <- getWeightJkReplicates(wgtNames[i], data)
      eout(pasteItems(sQuote(jki)), indent=4, exdent=4)
    } # end of if statment: if verbrose 
    cat("\n")
  } #End of For loop: for i in 1:length(wgtNames)
}


#' @title Weight Test
#'
#' @description Returns logical values indicating whether a vector of variables is a weight for an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#' @param var a character vector of variables
#'
#' @return a logical vector of values indicating if each element of \code{var}
#'         is a weight
#'
#' @details Note that this function returns \code{TRUE} only when the \code{var} element is the name of the weight used
#'          for making estimates but not if it is one of the individual jackknife replicates.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\isWeight.R
#' @export
isWeight <- function(var, data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (inherits(data, c("edsurvey.data.frame"))) { # data is an edsurvey.data.frame, so return whether string is in data$weights
    return(var %in% names(data$weights))
  } else { # data is a light.edsurvey.data.frame, so return whether string is in attributes(data)$weights
    return(var %in% names(attributes(data)$weights))
  }
}

#' @title Retrieve the Jackknife Replicate Weights
#'
#' @description Returns the jackknife replicate weights on an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'              an \code{edsurvey.data.frame.list} associated with a weight variable.
#'
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}
#' @param var character indicating the name of the weight variable for which the
#'            jackknife replicate weights are desired
#'
#' @return a character vector of the jackknife replicate weights
#'
#' @example \man\examples\getWeightJkReplicates.R
#' @author Michael Lee and Paul Bailey
#' @export
getWeightJkReplicates <- function(var, data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if(!isWeight(var, data)) {
    stop("The ", sQuote("var"), " argument level of ", dQuote(var), " is not a weight.")
  }

  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so jkbase and jkreplicates returned in data
    return(paste0(data$weights[[var]]$jkbase, data$weights[[var]]$jksuffixes))
  } else {
    # data is a light.edsurvey.data.frame, so jkbase and jkreplicates returned in
    # attributes(data)
    return(paste0(attributes(data)$weights[[var]]$jkbase, attributes(data)$weights[[var]]$jksuffixes))
  }
}
