# @author Trang Nguyen and Paul Bailey
#' @importFrom utils find
#' @method subset edsurvey.data.frame.list
#' @export
subset.edsurvey.data.frame.list <- function(x, subset, inside=FALSE, drop = FALSE, ...) {
  if(!inherits(x, c("edsurvey.data.frame.list"))) {
    stop(paste0("The argument ",sQuote("x"), " must be an ", dQuote("edsurvey.data.frame.list"), "."))
  }

  if(inside) {
    if(inherits(subset, "character")) {
      subset <- parse(text=subset)[[1]]
    }
    subset_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the subset will not change as that
    # variable is updated.
    # add it to the user subsets

    # parse the subset
    # substitute in variables that are available in the current environment
    subset_call <- substitute(subset)
    iparse <- function(iparseCall, subsetEnv, iparseDepth=1) {
      # for each element
      for(iparseind in 1:length(iparseCall)) {
        # if it is a name
        if(class(iparseCall[[iparseind]]) %in% c("name")) {
          iparseCall_c <- as.character(iparseCall[[iparseind]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if(! iparseCall_c %in% colnames(x)) {
            if(length(find(iparseCall_c)) > 0) {
              if (iparseCall[[iparseind]] == "%in%" || is.function(iparseCall[[iparseind]]) || is.function(get(iparseCall_c, find(iparseCall_c)))) {
                ev <- eval(substitute(iparseCall[[iparseind]]), parent.frame())
              } else {
                # get the variable
                ev <- eval(iparseCall[[iparseind]], parent.frame())
              }
              iparseCall[[iparseind]] <- ev
            } #end if length(find(iparseCall_c)) > 0)
            # but, if dynGet returns, use that instead
            iparsedg <- dynGet(iparseCall_c, ifnotfound="", minframe=1L)
            # if dynGet found something
            if(any(iparsedg != "")) {
              iparseCall[[iparseind]] <- iparsedg
            }
          } # end if(!call_c)
        } # end if(class(iparseCall[[iparseind]]) %in% c("name")) {
        if(class(iparseCall[[iparseind]]) %in% "call") {
          # if this is a call, recursively parse that
          iparseCall[[iparseind]] <- iparse(iparseCall[[iparseind]], subsetEnv, iparseDepth = iparseDepth + 1)
        } #end of if statment: if class(iparseCall[[i]]) %in% "call"
      } # end of for loop: i in 1:length(iparseCall)
      iparseCall
    } # End of fucntion: iparse
    subset_call <- iparse(subset_call, parent.frame())
  } # Enf of if esle statmet: if inside is true 

  res <- x
  subsetVars <- all.vars(subset_call)
  res$datalist <- lapply(1:length(x$datalist), function(dataListi) {
    dataList_li <- x$datalist[[dataListi]]
    # check whether the variable exists the edsurvey.data.frame
    for (dataList_v in subsetVars) {
      if (!dataList_v %in% colnames(dataList_li)) {
        warning(paste0("Variable ", sQuote(dataList_v), "is not found in the data ",sQuote(x$covs[dataListi,]),"."))
        return(NULL)
      }
    }
    dataList_li[["userConditions"]] <- c(dataList_li[["userConditions"]], list(subset_call))
    dataList_li
  })
  
  # Remove NULL element
  if (drop) {
    index_removed <- which(sapply(res$datalist,
                                  function(i) {
                                    return(is.null(i) || nrow(i) == 0) }))
  } else {
    index_removed <- which(sapply(res$datalist, is.null))
  }
 
 
  if (length(index_removed) > 0) {
    res$datalist[index_removed] <- NULL
    res$covs <- res$covs[-index_removed,names(res$covs),drop=FALSE]
    row.names(res$covs) <- NULL
  }
  
  # if there is no element left
  if (length(res$datalist) == 0) {
    res <- NULL
  }
  if (length(res$datalist) == 1) {
    res <- res$datalist[[1]]
  }
  res
} # end of fuction subset.edsurvey.data.frame
