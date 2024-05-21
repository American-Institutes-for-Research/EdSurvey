#' @title Print Levels and Labels
#'
#' @description Retrieve the levels and labels of a variable from an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param varnames a vector of character strings to search for in the database connection object (\code{data})
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#' @param showOmitted a Boolean indicating if omitted levels should be shown
#' @param showN a Boolean indicating if (unweighted) \emph{n}-sizes should be shown for each response level
#'
#' @author Michael Lee and Paul Bailey
#' @example  man/examples/levelsSDF.R
#' @export
levelsSDF <- function(varnames, data, showOmitted = TRUE, showN = TRUE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(), data))
  }
  # show levels only if varnames is valid
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if (any(!varnames %in% colnames(data))) {
    warning(paste0("Could not find variable(s) ", pasteItems(dQuote(varnames), final = "or")))
    varnames <- varnames[varnames %in% colnames(data)]
    if (length(varnames) == 0) {
      return()
    }
  }


  # check recode attribute in userConditions, and only include those recodes that are applicable to varnames called in levelsSDF
  userConditions <- getAttributes(data, "userConditions")
  recode <- userConditions[which(names(userConditions) %in% "recode")]

  if (length(recode) > 0) {
    recode <- unlist(recode, recursive = FALSE)
    names(recode) <- tolower(gsub("^recode.", "", names(recode)))
    # Keep only recodes that among varnames
    recode <- recode[names(recode) %in% varnames]
  }

  if (inherits(data, c("light.edsurvey.data.frame"))) {
    varnames <- toupper(varnames)
    labelsFile <- showCodebook(data, labelLevels = TRUE)
    labelsFile$labelValues <- gsub("\\; ", "^", labelsFile$labelValues)
  } else {
    varnames <- toupper(varnames)
    labelsFile <- do.call('rbind', lapply(data$dataList, function(dl){
                                          ff <- dl$fileFormat
                                          ff$fileFormat <- dl$levelLabel
                                          subset(ff, !(variableName %in% dl$ignoreVars))}))
  }
  labelsFile$variableName <- toupper(labelsFile$variableName)
  vars <- subset(labelsFile, labelsFile$variableName %in% varnames)

  levelsData <- list()
  for (i in unique(varnames)) {
    varDF <- vars[vars$variableName == i, ]
    varLevels <- unlist(strsplit(varDF["labelValues"][[1]], "^", fixed = TRUE))
    # if variable is in the list of recode, need to change the level result
    levelsData[[i]] <- c(varLevels)
  }

  # only recode labels/levels if there are recodes in the user conditions
  if (length(recode) > 0) {
    # if a variable level has been recoded, need to remove the label used to identify it when printing the updated result
    levelsDataRecode <- list()
    for (i in seq_along(recode)) {
      ni <- toupper(names(recode[i]))
      levs <- c()
      for (j in seq_along(unlist(levelsData[ni]))) {
        levs <- c(levs, strsplit(levelsData[[ni]], "=")[[j]][1])
      }

      labs <- c()
      for (j in seq_along(unlist(levelsData[ni]))) {
        labs <- c(labs, strsplit(levelsData[[ni]], "=")[[j]][2])
      }

      from <- recode[[i]]$from
      to <- recode[[i]]$to

      badFrom <- c() # levels with incorrect recodes
      if (is.numeric(to)) {
        if (!to %in% levs) {
          labs <- c(labs, as.character(to)) # since there are no labels provided, we will use character format of levels
          levs <- c(levs, to)
        }
        toNum <- to
        to <- labs[levs == to]
      } else {
        if (!to %in% labs) {
          labs <- c(labs, to)
          levs <- as.numeric(levs)
          toNum <- max(levs, na.rm = TRUE) + 1
          levs <- c(levs, toNum)
        } else {
          toNum <- levs[which(to %in% labs)]
        }
      } # end if (is.numeric(to))
      # after the code above, to is always a character label

      # from can be a vector of mixed numeric and character values
      # fromNum: numeric values in from
      # fromChar: character values in from
      suppressWarnings(fromNum <- as.numeric(from)) # numeric from variables
      fromChar <- from[is.na(fromNum)] # character from variables
      # numeric from variables
      fromNum <- fromNum[!is.na(fromNum)]

      # changing tmp according to numeric values of from
      if (length(fromNum) > 0) {
        if (any(!fromNum %in% levs)) {
          # add any missing levels to missing list
          badFrom <- fromNum[!fromNum %in% levs]
        }
        labs <- labs[!levs %in% setdiff(fromNum, toNum)]
        levs <- levs[!levs %in% setdiff(fromNum, toNum)]
      }
      # changing tmp according to character values of from
      if (length(fromChar) > 0) {
        if (any(!fromChar %in% labs)) {
          badFrom <- c(badFrom, fromChar[!fromChar %in% labs])
        }
        levs <- levs[!labs %in% setdiff(fromChar, to)]
        labs <- labs[!labs %in% setdiff(fromChar, to)]
      }
      varLevels <- paste0(levs, "=", labs)

      # if variable is in the list of recode, need to change the level result
      levelsData[[ni]] <- c(varLevels)
    } # for (i in seq_along(recode))
  } # if (length(recode) > 0)
  names(levelsData) <- tolower(names(levelsData))

  lnames <- names(levelsData)
  levelsData <- lapply(levelsData, function(x) {
    labs <- strsplit(x, split = "=", fixed = TRUE)
    levels <- unlist(lapply(labs, function(z) {
      z[1]
    }))
    labels <- unlist(lapply(labs, function(z) {
      paste0(z[-1], sep = "", collapse = "=")
    })) # be sure to handle special cases where a label has a '=' in the label itself
    return(data.frame(level = levels, labels = labels))
  })

  # add omitted
  if (showOmitted) {
    omittedL <- getAttributes(data, "omittedLevels")
    levelsData <- lapply(levelsData, function(x) {
      if (nrow(x) == 0) {
        return(x)
      } else {
        # add omitted column based on if the label is an omitted level
        x$omitted <- x$labels %in% omittedL
        return(x)
      }
    })
  }
  # add n sizes
  if (showN) {
    dat <- getData(data = data, varnames = names(levelsData), dropOmittedLevels = FALSE)
    lnames <- names(levelsData)
    levelsData <- lapply(lnames, function(x) {
      if (nrow(levelsData[[x]]) == 0) {
        return(levelsData[[x]])
      } else {
        ns <- table(dat[ , x])
        ld <- levelsData[[x]]
        ld$n <- 0
        for (i in 1:nrow(ld)) {
          if (ld$labels[i] %in% names(ns)) {
            ld$n[i] <- ns[ld$labels[i] == names(ns)]
          }
        }
        return(ld)
      }
    })
    names(levelsData) <- lnames
  }

  class(levelsData) <- c("levelsSDF")
  if (length(levelsData) == 0) {
    # return a warning if there are no variables in the data$fileFormat or llevels attribute of
    # the data.frame that match the string searched
    warning(paste0("There are no variables with the string(s) ", sQuote(varnames), " in the codebook for this ", class(data)[[1]], "."))
  } else {
    # we are returning something
    if (inherits(data, c("light.edsurvey.data.frame"))) {
      warning("These codes were taken when getData was called and may have been modified since then.")
    }
  }
  return(levelsData)
}

# @author Michael Lee and Paul Bailey
#' @method print levelsSDF
#' @export
print.levelsSDF <- function(x, ..., use_es_round=getOption("EdSurvey_round_output")) {
  if(use_es_round) {
    x <- es_round(x, ...)
  }
  if (length(x) > 0) {
    for (i in seq_along(x)) {
      cat(paste0("Levels for Variable '", tolower(names(x[i])), "' (Lowest level first):\n"))
      if (nrow(x[[i]]) == 0) {
        # if there is no level info return an NA
        cat(paste0("    ", paste0(NA), "\n"))
      } else {
        # the data.frame for this variable
        xi <- x[[i]]
        # reformat omitted to * vs blank string
        if ("omitted" %in% colnames(xi)) {
          xi$omitted <- ifelse(xi$omitted, "*", "")
        }
        # print row by row
        for (ii in 1:nrow(xi)) {
          cat(paste0("    ", xi$level[ii], ". ", xi$label[ii]))
          if ("omitted" %in% colnames(xi)) {
            cat(xi$omitted[ii])
          }
          if ("n" %in% colnames(xi)) {
            cat(paste0(" (n = ", xi$n[ii], ")"))
          }
          cat("\n")
        }
        if ("omitted" %in% colnames(xi)) {
          if (sum(xi$omitted %in% "*") > 0) { # there is at least one omitted
            cat("    NOTE: * indicates an omitted level.\n")
          }
        }
      } # End of if/else: nrow(x[[i]]) == 0
    } # End of loop: i in seq_along(x)
  } # End of If statment: if legth of x is greater than 0
}
