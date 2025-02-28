#' @title Modify Variable Names
#'
#' @description Renames variables in an \code{edsurvey.data.frame},
#' a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#' This function often is used when users want to conduct a gap analysis across
#'  years but variable names differ across two years of data.
#'
#' @param x an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'          or an \code{edsurvey.data.frame.list}
#' @param oldnames a character vector of old variable names
#' @param newnames a character vector of new variable names to replace the
#'                 corresponding old names
#' @param avoid_duplicated a logical value to indicate whether to avoid renaming the
#'                         variable if the corresponding new name already exists in the data.
#'                         Defaults to \code{TRUE}.
#'
#' @details All variable names are coerced to lowercase to comply with
#'          the \code{EdSurvey} standard.
#'
#' @return an object of the same class as \code{x} with new variable names
#' @usage rename.sdf(x, oldnames, newnames, avoid_duplicated = TRUE)
#' @export rename.sdf
#'
#' @seealso \code{\link{gap}}
#' @author Trang Nguyen
#' @example man/examples/rename.sdf.R
rename.sdf <- function(x,
                       oldnames,
                       newnames,
                       avoid_duplicated = TRUE) {
  # Preparing/ checking arguments
  checkDataClass(x, c("edsurvey.data.frame.list", "edsurvey.data.frame", "light.edsurvey.data.frame"))
  if (length(oldnames) != length(newnames)) {
    stop("Length of old variable names is not equal to length of new variable names. ")
  }

  if (inherits(x, "edsurvey.data.frame") || inherits(x, "light.edsurvey.data.frame")) {
    # list of elements that is involved in the rename process
    userConditions <- getAttributes(x, "userConditions", errorCheck = FALSE)
    pvvars <- getAttributes(x, "pvvars", errorCheck = FALSE)
    weights <- getAttributes(x, "weights")
    psuVarList <- getAttributes(x, "psuVar", errorCheck = FALSE)
    stratumVarList <- getAttributes(x, "stratumVar", errorCheck = FALSE)
    defaultTaylorVar <- TRUE
    if (is.null(psuVarList) || psuVarList == "") {
      psuVarList <- vapply(weights, function(w) w$psuVar, FUN.VALUE=character(1))
      defaultTaylorVar <- FALSE
    }
    if (is.null(stratumVarList) || stratumVarList == "") {
      stratumVarList <- vapply(weights, function(w) w$stratumVar, FUN.VALUE=character(1))
      defaultTaylorVar <- FALSE
    }
    if (inherits(x, "edsurvey.data.frame")) {
      varnames <- colnames(x)
    } else {
      varnames <- colnames(x)
    }

    # grab the dataList and needed objects within the dataList
    dataList <- getAttributes(x, "dataList")
    lafObj <- lapply(dataList, function(dl) {
      dl$lafObject
    })
    fileFormat <- lapply(dataList, function(dl) {
      dl$fileFormat
    })
    parentMergeVars <- lapply(dataList, function(dl) {
      dl$parentMergeVars
    })
    mergeVars <- lapply(dataList, function(dl) {
      dl$mergeVars
    })
    ignoreVars <- lapply(dataList, function(dl) {
      dl$ignoreVars
    })

    if (any(grepl("_linking", oldnames, fixed = TRUE))) {
      stop("Cannot rename a PV variable with _linking in the name. These are reserved for linking error.")
    }
    if (any(grepl("_linking", newnames, fixed = TRUE))) {
      stop("Cannot rename a PV variable with _linking in the name. These are reserved for linking error.")
    }
    for (vari in seq_along(oldnames)) {
      # to avoid duplicates after the operation
      if (newnames[vari] %in% c(varnames, names(weights), names(pvvars))) {
        if (avoid_duplicated) {
          warning(paste0("Variable name ", sQuote(newnames[vari]), " already exists in the data. Not renaming the variable to avoid duplicates."))
          next
        } else {
          warning(paste0("Variable name ", sQuote(newnames[vari]), " already exists in the data. Renaming the variable to ", sQuote(paste0(newnames[vari], "_2")), ". "))
          newnames[vari] <- paste0(newnames[vari], "_2")
        }
      }
      ## get specific name of old variable
      varn <- oldnames[vari]

      # change variable name in userConditions list
      if (!is.null(userConditions) && length(userConditions) > 0) {
        for (i in seq_along(userConditions)) {
          if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
            names(userConditions[[i]]) <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], names(userConditions[[i]]))
          } else {
            condition <- userConditions[[i]]
            userConditions[[i]] <- replaceVars(condition, oldnames[vari], newnames[vari])
          }
        } # end (for(i in seq_along(userConditions)))
      } # end if (!is.null(userConditons))

      # change pvvars
      if (!is.null(pvvars) & length(pvvars) > 0) {
        if (oldnames[vari] %in% names(pvvars)) {
          names(pvvars)[names(pvvars) == oldnames[vari]] <- newnames[vari]
          attr(pvvars, "default") <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], attr(pvvars, "default"))
        }
      }

      # change weights
      if (oldnames[vari] %in% names(weights)) {
        names(weights)[names(weights) == oldnames[vari]] <- newnames[vari]
        attr(weights, "default") <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], attr(weights, "default"))
        if (oldnames[vari] %in% varnames) {
          varnames[varnames == oldnames[vari]] <- newnames[vari]
        }
      }

      # change stratumVar and psuVar
      if (length(psuVarList) > 0) {
        if (oldnames[vari] %in% psuVarList) {
          if (defaultTaylorVar) {
            setAttributes(x, "psuVar", newnames[vari])
          } else {
            weights[[which(psuVarList == oldnames[vari])]]$psuVar <- newnames[vari]
          }
        }
      }

      if (length(stratumVarList) > 0) {
        if (oldnames[vari] %in% stratumVarList) {
          if (defaultTaylorVar) {
            setAttributes(x, "stratumVar", newnames[vari])
          } else {
            weights[[which(stratumVarList == oldnames[vari])]]$stratumVar <- newnames[vari]
          }
        }
      }

      if (!oldnames[vari] %in% varnames) {
        # do nothing
      }

      ## change name in LaF Objects as well as the LaF 'column_names' attribute which is used when the file connection is open/closed
      if (!is.null(lafObj) && length(lafObj) > 0) {
        for (i in seq_along(lafObj)) {
          if (varn %in% names(lafObj[[i]])) {
            names(lafObj[[i]])[names(lafObj[[i]]) == oldnames[vari]] <- newnames[vari]
            attr(lafObj[[i]], "column_names") <- names(lafObj[[i]]) # ensure the column_names is changed as well or if connection reopened/closed the col name will be lost!
          }
        }
      }

      varnames[varnames == oldnames[vari]] <- newnames[vari]

      # if the name is one of the plausible values
      if (!is.null(pvvars) & length(pvvars) > 0) {
        for (pvi in seq_along(pvvars)) {
          pvvars[[pvi]]$varnames <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], pvvars[[pvi]]$varnames)
        }
      }

      #
      if (!is.null(fileFormat) && length(fileFormat) > 0) {
        for (i in seq_along(fileFormat)) {
          if (varn %in% fileFormat[[i]]$variableName) {
            fileFormat[[i]]$variableName[fileFormat[[i]]$variableName == varn] <- newnames[vari]
          }
        }
      }

      ## update parentMergeVars
      if (!is.null(parentMergeVars) && length(parentMergeVars) > 0) {
        for (i in seq_along(parentMergeVars)) {
          parentMergeVars[[i]] <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], parentMergeVars[[i]])
        }
      }

      # update the mergeVars
      if (!is.null(mergeVars) && length(mergeVars) > 0) {
        for (i in seq_along(mergeVars)) {
          mergeVars[[i]] <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], mergeVars[[i]])
        }
      }

      # update the ignoreVars
      if (!is.null(ignoreVars) && length(ignoreVars) > 0) {
        for (i in seq_along(ignoreVars)) {
          ignoreVars[[i]] <- gsub(paste0("\\b", oldnames[vari], "\\b"), newnames[vari], ignoreVars[[i]])
        }
      }
    } # end (for(vari in seq_along(oldnames)))

    # replace all of attributes
    if (inherits(x, "light.edsurvey.data.frame")) {
      names(x) <- varnames
    }

    # update dataList from our modified object copies
    newDataList <- x$dataList

    if (!is.null(newDataList) && length(newDataList) > 0) {
      for (i in seq_along(newDataList)) {
        newDataList[[i]]$lafObject <- lafObj[[i]]
        names(newDataList[[i]]$lafObject) <- names(lafObj[[i]])

        newDataList[[i]]$fileFormat <- fileFormat[[i]]
        newDataList[[i]]$parentMergeVars <- parentMergeVars[[i]]
        newDataList[[i]]$mergeVars <- mergeVars[[i]]
        newDataList[[i]]$ignoreVars <- ignoreVars[[i]]
      }
    }

    # update return object attributes from our modified object copies
    if (!is.null(userConditions)) {
      x <- setAttributes(x, "userConditions", userConditions)
    }
    if (!is.null(pvvars)) {
      x <- setAttributes(x, "pvvars", pvvars)
    }
    if (!is.null(weights)) {
      x <- setAttributes(x, "weights", weights)
    }
    if (!is.null(newDataList)) {
      x <- setAttributes(x, "dataList", newDataList)
    }

    return(x)
  } else if (inherits(x, "edsurvey.data.frame.list")) {
    # assuming that variable names are consistent through the data list
    for (i in seq_along(x$datalist)) {
      x$data[[i]] <- rename.sdf(x$data[[i]], oldnames, newnames)
      x$datalist[[i]] <- x$data[[i]]
    }
    return(x)
  }
}

replaceVars <- function(condition, oldname, newname) {
  for (ci in seq_along(condition)) {
    if (class(condition[[ci]]) %in% "name" && length(condition[[ci]]) == 1 && as.character(condition[[ci]]) %in% oldname) {
      condition[[ci]] <- as.name(newname)
    }
    if (class(condition[[ci]]) %in% "call" || length(condition[[ci]]) > 1) {
      condition[[ci]] <- replaceVars(condition[[ci]], oldname, newname)
    }
  } # end for(ci in seq_along(conditon))
  condition
}
