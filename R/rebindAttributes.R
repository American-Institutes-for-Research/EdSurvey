#' @title Copy Data Frame Attributes
#'
#' @description Many R functions strip attributes from data frame objects. This
#'              function assigns the attributes from the \code{attributeData} argument
#'              to the data frame in the \code{data} argument.
#' @param data a \code{data.frame}
#' @param attributeData an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}
#'                      that contains the desired attributes
#'
#' @return
#' a \code{data.frame} with a class of a \code{light.edsurvey.data.frame} containing
#' all elements of data and the attributes (except
#' \code{names} and \code{row.names}) from \code{attributeData}
#'
#' @example man/examples/rebindAttributes.R
#' @author Paul Bailey and Trang Nguyen
#' @export
rebindAttributes <- function(data, attributeData) {
  checkDataClass(attributeData, c("edsurvey.data.frame", "light.edsurvey.data.frame"), argument = "attributeData")
  if (inherits(attributeData, "light.edsurvey.data.frame")) {
    attrList <- attributes(attributeData)
    attrN <- names(attrList)
    attrN <- attrN[!attrN %in% c("names", "row.names", "class")]
    data <- as.data.frame(data)
    data <- lightUnclassCols(data)
    class(data) <- c("light.edsurvey.data.frame", "data.frame")
    for (a in attrN) {
      attr(data, a) <- attrList[[a]]
    }
  } else {
    class(attributeData) <- "list"
    # get the names of the attributes
    attrN <- names(attributeData)
    # exclude the "data" attribute
    attrN <- attrN[attrN != "data"]
    # add every other attribute to "data"
    data <- as.data.frame(data)
    data <- lightUnclassCols(data)
    class(data) <- c("light.edsurvey.data.frame", "data.frame")
    for (a in attrN) {
      attr(data, a) <- attributeData[[a]]
    }
    # reset userConditions to remove recode (because its already applied)
    userConditions <- getAttributes(data, "userConditions", errorCheck = FALSE)
    data <- setAttributes(data, "userConditions", userConditions[which(!names(userConditions) %in% "recode")])
  }
  return(data)
}
