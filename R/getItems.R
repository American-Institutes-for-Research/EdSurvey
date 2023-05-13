#' @title Retrieve IRT Item Variable Names
#' 
#' @description Retrieves the IRT item variable names associated with construct names for use with \code{mml.sdf} function.
#' 
#' @param sdf an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} containing IRT information. Supports NAEP and TIMSS 2011, 2015, and 2019 studies only.
#' @param construct a character value (or vector) for which to return the associated item variable names. Default value is \code{NULL} which returns all IRT item variable names.
#'  Use the \code{showPlausibleValues} function to view construct details.
#' 
#' @return a character vector of the items names associated for the values in \code{construct}.
#' 
#' @note if \code{construct} is a vector, all item names will be returned for those constructs. Use \code{getAllItems} with \code{getData} when creating a \code{light.edsurvey.data.frame}, see example for use.
#'  
#' @author Tom Fink, Sun-Joo Lee, Eric Buehler, and Paul Bailey
#' @example \man\examples\getAllItems.R
#' @seealso \code{\link{mml.sdf}}
#' @export
getAllItems <- function(sdf, construct = NULL) {
  
  has_IRTAttributes(sdf, errorCheck = TRUE)
  
  pvvars <- names(getAttributes(sdf, "pvvars"))
  if(!all(construct %in% pvvars)){
    stop(paste0(dQuote("getAllItems"), " expects only contruct names, or a NULL value for all items. Use ", dQuote("showPlausibleValues"), " to view them."))
  }
  
  survey <- getAttributes(sdf, "survey")
  params <- list(sdf = sdf,
                 construct = construct)
  
  res <- NULL
  if(survey %in% "NAEP") {
    res <- do.call("getAllItems_NAEP", args = params, quote = TRUE)
  }
  if(survey %in% "TIMSS") {
    res <- do.call("getAllItems_TIMSS", args = params, quote = TRUE)
  }
  return(res)
}

#for TIMSS data
getAllItems_TIMSS <- function(sdf, construct){

  scalesDF <- getAttributes(sdf, "testData", errorCheck=FALSE) #not currently in use, but holds the test data
  dichotDF <- getAttributes(sdf, "dichotParamTab", errorCheck=FALSE)
  polyDF <- getAttributes(sdf, "polyParamTab", errorCheck=FALSE)
  
  resultFields <- c()
  if(is.null(construct)) {
    resultFields <- c(dichotDF$ItemID, polyDF$ItemID) #all items
  } else {
    #FOR NOW, return all Math items (as indicated by the 'm' starting varname char)
    if (any(grepl("^m", construct, ignore.case = TRUE))) {
      fld1 <- grep("^m", dichotDF$ItemID, ignore.case = TRUE, value = TRUE)
      fld2 <- grep("^m", polyDF$ItemID, ignore.case = TRUE, value = TRUE)
      resultFields <- c(resultFields, fld1, fld2)
    }
    
    #FOR NOW, return all Science items (as indicated by the 's' starting varname char)
    if (any(grepl("^s", construct, ignore.case = TRUE))) {
      fld1 <- grep("^s", dichotDF$ItemID, ignore.case = TRUE, value = TRUE)
      fld2 <- grep("^s", polyDF$ItemID, ignore.case = TRUE, value = TRUE)
      resultFields <- c(resultFields, fld1, fld2)
    }
  }# end if(is.null(construct))
  
  #filter for the specific fields in the data.frame
  resultFields <- resultFields[tolower(resultFields) %in% tolower(colnames(sdf))]
  return(resultFields)
}

#for NAEP data
getAllItems_NAEP <- function(sdf, construct){

  scalesDF <- getAttributes(sdf, "testData", errorCheck=FALSE) #not currently in use, but holds the test data
  dichotDF <- getAttributes(sdf, "dichotParamTab", errorCheck=FALSE)
  polyDF <- getAttributes(sdf, "polyParamTab", errorCheck=FALSE)
  
  resultFields <- c(dichotDF$ItemID, polyDF$ItemID)

  #filter for the specific fields in the data.frame
  resultFields <- resultFields[tolower(resultFields) %in% tolower(colnames(sdf))]
  
  return(resultFields)
}
