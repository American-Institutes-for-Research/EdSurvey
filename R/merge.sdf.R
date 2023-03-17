#' @name merge.edsurvey.data
#' @title EdSurvey Merge
#'
#' @description Takes a \code{data.frame} or a \code{light.edsurvey.data.frame} and merges with a \code{edsurvey.data.frame} into it's internal data cache.
#'
#' @param x a \code{edsurvey.data.frame}. The \code{x} object is retained and has y values stored in the internal cache in memory.  
#'          \code{x} also supports \code{light.edusrvey.data.frame} objects if \code{y} is a \code{data.frame} or \code{light.edsurvey.data.frame} object.
#' @param y either a \code{light.edsurvey.data.frame} or a \code{data.frame}
#' @param by the column name(s) to perform the data merge operation.  If differing column names between the \code{x} and \code{y} objects, use the \code{by.x} and \code{by.y} arguments.
#' @param by.x the column name(s) to perform the data merge operation for the \code{x} object.  Defaults to \code{by} value.
#' @param by.y the column name(s) to perform the data merge operation for the \code{y} object.  Defaults to \code{by} value.
#' @param \dots arguments passed to merge, note that \code{all.x} will always be \code{TRUE} (the data on the \code{edsurvey.data.frame} will always be kept) and \code{all.y} will always be \code{FALSE} to avoid adding data not on the \code{edsurvey.data.frame}.
#' @return a merged data set the same object type as \code{x}. For \code{edsurvey.data.frame} objects then resulting merged data is stored in the objects internal data cache.
#'
#' @seealso \ifelse{latex}{\code{merge}}{\code{\link[base]{merge}}} 

#' @example man/examples/merge.edsurvey.data.frame.R
#' @author Tom Fink
#'
#' @method merge edsurvey.data.frame
#' @export
merge.edsurvey.data.frame <- function(x, y, by = "id", by.x = by, by.y = by, ...){

  checkDataClass(x, c("edsurvey.data.frame", "light.edsurvey.data.frame"), argument = "x")
  checkDataClass(y, c("light.edsurvey.data.frame", "data.frame"), argument = "y")
  
  if(!all(by.x %in% colnames(x))){
    xVar <- by.x[which(!(by.x %in% colnames(x)))]
    stop(paste0(pasteItems(xVar), " fields not found in ", dQuote(x), " object."))
  }
  if(!all(by.y %in% colnames(y))){
    yVar <- by.y[which(!(by.y %in% colnames(y)))]
    stop(paste0(pasteItems(yVar), " fields not found in ", dQuote(y), " object."))
  }
  
  # setup the call to merge
  args <- list(...)
  args$all.x <- TRUE
  args$all.y <- FALSE
  args$by.x <- by.x
  args$by.y <- by.y
  # y is handled in one way, how x is handled depends on x
  args$y <- y

  #perform light.edsurvey.data.frame merge
  if(inherits(x, "light.edsurvey.data.frame")){
    args$x <- as.data.frame(x)
    res <- do.call(merge, args, quote = TRUE) #base merge, must set quote=TRUE to properly eval args
    res <- rebindAttributes(res, x)
    return(res)
  }
  
  cacheLvl <- x$cacheDataLevelName
  fM0 <- x$dataList[[cacheLvl]]$forceMerge
  x$dataList[[cacheLvl]]$forceMerge <- TRUE
  # this puts the merge var on the cache if it isn't already
  if(!all(by.x %in% colnames(x$cache))) {
    x[ , by.x] <- x[ , by.x]
  }
  
  args$x <- as.data.frame(x$cache)
  colNamesX <- colnames(args$x) #store here to know what new additional fields were added to the cache after merge
  mg <- do.call(merge, args, quote = TRUE) #base merge, must set quote=TRUE to properly eval args
  
  x$dataList[[cacheLvl]]$forceMerge <- fM0 #toggle back
  
  #consistency check with the cache size to ensure it hasn't adjusted
  if(nrow(mg) > nrow(x$cache)) {
    stop("Merged data contains multiple ID values causing data expansion and cannot be merged.  Consider creating a derived column instead.")
  }
  #reorder
  x$cache <- mg[order(mg$ROWID), ]
  return(x)
}

#' @method merge light.edsurvey.data.frame
#' @export
merge.light.edsurvey.data.frame <- function(x, y, by = "id", by.x = by, by.y = by, ...){
  cl <- match.call()
  # swap x and y if y is an edsurvey.data.frame
  if(inherits(y, "edsurvey.data.frame")) {
    x0 <- cl$x 
    cl$x <- cl$y
    cl$y <- x0
    byx0 <- cl$by.x
    cl$by.x <- cl$by.y
    cl$by.y <- byx0
    if("suffixes" %in% names(cl)) {
      cl$suffixes <- rev(cl$suffixes)
    }
  }

  cl[[1]] <- quote(merge.edsurvey.data.frame) #use quote function here as the action, not a string value!
  return(eval(cl, envir = parent.frame()))
}

