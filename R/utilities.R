attachHere <- function(x, e = parent.frame()) {
  for (i in seq_along(x)) {
    assign(x = names(x)[i], value = x[[i]], envir = e)
  }
}

# if returNumberOfPSU is TRUE, returns the psu and stratum vars, after checking they're on the data
PSUStratumNeeded <- function(returnNumberOfPSU, data) {
  if (returnNumberOfPSU) {
    # Get stratum and PSU variable
    stratumVar <- getAttributes(data, "stratumVar")
    psuVar <- getAttributes(data, "psuVar")
    if (all(c(stratumVar, psuVar) %in% c(names(data))) | all(c(stratumVar, psuVar) %in% colnames(data))) {
      assign("stratumVar", stratumVar, -1)
      assign("psuVar", psuVar, -1)
      return(c(stratumVar, psuVar))
    } else {
      if ("JK1" %in% c(stratumVar, psuVar)) {
        # no PSU and stratum variables
        return(NULL)
      }
      stop("could not find needed vars.")
    }
  }
  return(NULL)
}

# utilities functions
setupMulticore <- function(multiCore, numberOfCores, verbose) {
  if (multiCore == TRUE) {
    if (verbose > 0) {
      message("Starting parallel processing.")
    }
    # check doParallel
    if (requireNamespace("doParallel")) {
      detCores <- parallel::detectCores()
      # set numberOfCores default if not provided
      if (is.null(numberOfCores)) {
        numberOfCores <- detCores * .75
      }
      # check that they aren't using too many cores
      if (numberOfCores > detCores) {
        defaultCores <- detCores * .75
        warning(paste0(
          sQuote(numberOfCores), " is greater than number of avaliable cores,",
          sQuote(detCores), " setting number of cores to default of ",
          sQuote(defaultCores)
        ))
        numberOfCores <- defaultCores
      }
    } else {
      multiCore <- FALSE
      message(paste0("Unable to find package doParallel, setting multiCore to FALSE. Install the ", dQuote("doParallel"), " package to use multiCore option."))
    }
    # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18119
    # R on OS X bug that prevents parallel
    if (grepl("Darwin", Sys.info()[1]) &&
      getRversion() <= "4.1.0") {
      multiCore <- FALSE
      message("Upgrade to R 4.1.1 or higher to use multiCore on Mac OS.")
    }
  }
  return(list(multiCore = multiCore, numberOfCores = numberOfCores))
}

# multiCoreSetup is the result of a call to setupMulticore
# verbose allows this function to use message to print the number of cores to the screen
# ExitDepth says what depth of parent.frame should this clean up the cluster when it exits.
#           numbers less than 1 lead to no cluster stop being called. Numbers over 1 increase the parent depth
startMulticore <- function(multiCoreSetup, verbose = FALSE, ExitDepth = 2) {
  if (multiCoreSetup$multiCore) {
    # check if cluster is already running
    if (nrow(showConnections()) == 0) {
      cores <- round(multiCoreSetup$numberOfCores, 0) # use 75 percent of cores
      cl <- parallel::makeCluster(cores)
      if (verbose >= 1) {
        message(paste0("Starting cluster with ", cl, " cores"))
      }
      doParallel::registerDoParallel(cl, cores = cores)
      # stop cluster before any exit
      if (ExitDepth > 0) {
        pf <- parent.frame(n = ExitDepth)
        assign("cl", cl, envir = pf)
        do.call(what = "on.exit", args = alist(parallel::stopCluster(cl)), envir = pf)
      }
    }
  } # end if(multiCore)
  return(NULL)
}

getAllTaylorVars <- function(data) {
  res <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
  res <- res[!res == ""]
  wgts <- getAttributes(data, "weights")
  for (wi in wgts) {
    if (!is.null(wi$stratumVar)) {
      res <- c(res, wi$stratumVar)
    }
    if (!is.null(wi$psuVar)) {
      res <- c(res, wi$psuVar)
    }
  }
  return(unique(res))
}

# This function returns default PSU variable name or a PSU variable associated with a weight variable
#' @rdname edsurvey-class
#' @export
getPSUVar <- function(data, weightVar = attributes(getAttributes(data, "weights"))[["default"]]) {
  # retrieve all the attributes we need to determine warnings
  defaultWeight <- attributes(getAttributes(data, "weights"))[["default"]]
  allWeights <- getAttributes(data, "weights")
  psuVar <- getAttributes(data, "psuVar")

  if (is.null(allWeights)) {
    return(NULL)
  }

  # user specifies weightVar to be NULL; remind users of showWeights function
  if (is.null(weightVar)) {
    # if there is a default weight, print it in warning
    if (!is.null(defaultWeight)) {
      if (defaultWeight != "") {
        return(warning(paste0("Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument or replace the ", sQuote("weightVar"), " argument with the default weight: ", sQuote(defaultWeight), ".")))
        # if there is a default psuVar that is not not null, specify in the warning
      } else if (!is.null(psuVar)) {
        return(warning(paste0("The data has more than one weight variable without a default. Either remove the ", sQuote("weightVar"), " argument entirely or use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
        # if the default psuVar is null, direct users to showWeights
      } else {
        return(warning(paste0("The data may have more than one weight variable without a default. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
      }
    }
  }

  # if the specified weightVar is a valid weight and the psuVar is not null, return it
  if ((weightVar %in% names(allWeights) & !is.null(psuVar)) | (weightVar == defaultWeight & !is.null(psuVar))) {
    return(psuVar)
  }

  # if the specified weightVar is not a valid weight, return a warning and direct to showWeights
  if (!weightVar %in% names(allWeights) & weightVar != "") {
    if (defaultWeight == "" & is.null(psuVar)) {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
    } else {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Either remove the ", sQuote("weightVar"), " argument entirely or use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
    }
  }

  # if the specified weightVar is an empty string, return a warning
  if (weightVar == "") {
    return(warning(paste0("The data may have more than one weight variable without a default. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
  }

  # if the specified weight has no default psuVar and/or we need to check whether the weight is valid, retrieve via getWeightByName
  weights <- getWeightByName(data, weightVar)
  return(weights$psuVar)
}

# This function returns default stratum variable name or a PSU variable associated with a weight variable
#' @rdname edsurvey-class
#' @export
getStratumVar <- function(data, weightVar = attributes(getAttributes(data, "weights"))[["default"]]) {
  # retrieve all the attributes we need to determine warnings
  defaultWeight <- attributes(getAttributes(data, "weights"))[["default"]]
  allWeights <- getAttributes(data, "weights")
  stratumVar <- getAttributes(data, "stratumVar", errorCheck = FALSE)

  if (is.null(allWeights)) {
    return(NULL)
  }

  # user specifies weightVar to be NULL; remind users of showWeights function
  if (is.null(weightVar)) {
    # if there is a default weight, print it in warning
    if (!is.null(defaultWeight)) {
      if (defaultWeight != "") {
        return(warning(paste0("Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument or replace the ", sQuote("weightVar"), " argument with the default weight: ", sQuote(defaultWeight), ".")))
        # if there is a default stratumVar that is not not null, specify in the warning
      } else if (!is.null(stratumVar)) {
        return(warning(paste0("The data has more than one weight variable without a default. Either remove the ", sQuote("weightVar"), " argument entirely or use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
        # if the default stratumVar is null, direct users to showWeights
      } else {
        return(warning(paste0("The data may have more than one weight variable without a default. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
      }
    }
  }

  # if the specified weightVar is a valid weight and the stratumVar is not null, return it
  if ((weightVar %in% names(allWeights) & !is.null(stratumVar)) | (weightVar == defaultWeight & !is.null(stratumVar))) {
    return(stratumVar)
  }

  # if the specified weightVar is not a valid weight, return a warning and direct to showWeights
  if (!weightVar %in% names(allWeights) & weightVar != "") {
    if (defaultWeight == "" & is.null(stratumVar)) {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
    } else {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Either remove the ", sQuote("weightVar"), " argument entirely or use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
    }
  }

  # if the specified weightVar is an empty string, return a warning
  if (weightVar == "") {
    return(warning(paste0("The data may have more than one weight variable without a default. Use the ", sQuote("showWeights"), " function to populate the ", sQuote("weightVar"), " argument.")))
  }

  # if the specified weight has no default stratumVar and/or we need to check whether the weight is valid, retrieve via getWeightByName
  weights <- getWeightByName(data, weightVar)
  return(weights$stratumVar)
}

# This function returns an attribute list of a given weight that includes:
# 1. jkbase
# 2. jksuffix
# 3. PSU variable (optional)
# 4. Stratum variable (optional)
getWeightByName <- function(data, weightVar) {
  weights <- getAttributes(data, "weights")
  weights <- weights[which(names(weights) == weightVar)]

  # Validate weightVar
  if (length(weights) == 0) {
    stop("The data does not have any weight called ", sQuote(weightVar), ".")
  }
  if (length(weights) > 1) {
    warning(paste0("The data has more than one weight variable called ", sQuote(weightVar), ". Using the first such weight."))
  }
  weights <- weights[[1]]
  return(weights)
}

# turns a vector into a properyly formatted list for showing the user
# @param vector a vector of items to paste
# @param final the word to put after the serial comma and the final element
# @
# examples:
# pasteItems(c())
# # NULL
# pasteItems(c("A"))
# # [1] "A"
# pasteItems(c("A", "B"))
# # [1] "A and B"
# pasteItems(c("A", "B", "C"))
# # [1] "A, B, and C"
# @author Paul Bailey
pasteItems <- function(vector, final = "and") {
  # no need to do anything if there is one or fewer elements
  if (length(vector) <= 1) {
    return(vector)
  }
  if (length(vector) == 2) {
    return(paste0(vector[1], " ", final, " ", vector[2]))
  }
  v <- vector[-length(vector)]
  f <- vector[length(vector)]
  return(paste0(paste(v, collapse = ", "), ", ", final, " ", f))
}

eout <- function(text, exdent = 2, indent = 0) {
  txto <- strwrap(text,
    width = getOption("width") * 0.9,
    indent = indent,
    exdent = exdent
  )
  writeLines(txto)
}

fixPath <- function(path) {
  dir <- dirname(path)
  base <- basename(path)
  flist <- list.files(dir)
  if (!dir.exists(dir)) {
    stop(paste0("Could not find directory ", dQuote(dir), "."))
  }
  if (base %in% flist) {
    # this is a good file path, return it
    return(file.path(dir, base))
  }
  if (tolower(base) %in% tolower(flist)) {
    return(file.path(dir, flist[which(tolower(flist) %in% tolower(base))]))
  }
  stop(paste0("Could not find file ", dQuote(file.path(dir, base)), "."))
}

fixTimeout <- function() {
  newtimeo <- max(60 * 60, options()$timeout)
  options(timeout = newtimeo)
}

cartFactor <- function(fa, fb) {
  if (!inherits(fa, "factor")) {
    fa <- factor(fa)
  }
  if (!inherits(fb, "factor")) {
    fb <- factor(fb)
  }
  df <- expand.grid(levels(fb), levels(fa))
  return(paste0(df$Var2, ":", df$Var1))
}

checkTaylorVars <- function(psuVar, stratumVar, wgt, varMethod = "t", returnNumberOfPSU = FALSE) {
  if (is.null(psuVar)) {
    if (returnNumberOfPSU & varMethod == "t") {
      stop(paste0("Cannot find primary sampling unit variable for weight ", sQuote(wgt), ". Try setting the ", dQuote("varMethod"), " argument to ", dQuote("jackknife"), " and ", dQuote("returnNumberOfPSU"), " to ", dQuote("FALSE"), "."))
    }
    if (!returnNumberOfPSU & varMethod == "t") {
      stop(paste0("Cannot find primary sampling unit variable for weight ", sQuote(wgt), ". Try setting the ", dQuote("varMethod"), " argument to ", dQuote("jackknife"), "."))
    }
    if (returnNumberOfPSU & varMethod != "t") {
      warning(paste0("Cannot find primary sampling unit variable for weight ", sQuote(wgt), ". Setting ", dQuote("returnNumberOfPSU"), " to ", dQuote("FALSE"), "."))
      returnNumberOfPSU <- FALSE
    }
    psuVar <- ""
  }
  if (is.null(stratumVar)) {
    if (varMethod == "t") {
      stop(paste0("Cannot find stratum variable for weight ", sQuote(wgt), ". Try setting the ", dQuote("varMethod"), " argument to ", dQuote("jackknife"), "."))
    }
    # set to a dummy variable, not on the data
    stratumVar <- ""
  }
  if ("JK1" %in% stratumVar & varMethod == "t") {
    varMethod <- "j"
    warning("Cannot use Taylor series estimation on a one-stage simple random sample.")
  }
  return(list(psuVar = psuVar, stratumVar = stratumVar, varMethod = varMethod, returnNumberOfPSU = returnNumberOfPSU))
}

# parse a condition
# @argument iparseCall a substituted call
# @argument iparseDepth start at 1, used by iparse to know the current depth
# @argument x the data set, must return colnames with the names of valid columns
#
# functions can be called with column names in quotes, unquoted, or as dynamic variables
# iparse resolves dynamic variables from a call and returns the variables in a unified format
#
# example calls that iparse helps all resolve to the same solution
# cor.sdf("b017451", "b003501", sdf)
# cor.sdf(b017451, b003501, sdf)
# b17 <- "b017451"
# b35 <- "b003501"
# cor.sdf(b17, b35, sdf)
#
# it is also helpful in resolving variables passed to function calls in the subset functions
#


#>   lm10D <- lm.sdf(composite ~ dsex + b017451, sdf, weightVar=c("origwt"))
# Error in if (as.character(iparseCall) %in% unlist(colnames(x))) { :
#  the condition has length > 1

iparse <- function(iparseCall, iparseDepth = 1, x) {
  # if this is just a character (before substitute was called) return it
  Xcols <- getXCols(x)
  if (iparseDepth == 1 && length(iparseCall) == 1) {
    skip <- tryCatch(inherits(eval(iparseCall), "character"),
      error = function(e) {
        FALSE
      },
      warning = function(w) {
        FALSE
      }
    )
    if (skip) {
      if (as.character(iparseCall) %in% Xcols) {
        return(iparseCall)
      } else {
        return(eval(iparseCall))
      }
    }
  }
  # if it is still a character, return it
  if (iparseDepth == 1 && length(iparseCall) == 1 && inherits(iparseCall, "character")) {
    return(iparseCall)
  }
  # for each element
  for (iparseind in 1:length(iparseCall)) {
    # if it is a name
    unlistOnExit <- FALSE
    if (inherits(iparseCall, "name")) {
      unlistOnExit <- TRUE
      iparseCall <- list(iparseCall)
    }
    if (inherits(iparseCall[[iparseind]], "name")) {
      iparseCall_c <- as.character(iparseCall[[iparseind]])
      # if it is not in the data and is in the parent.frame, then substitue it now.
      # unlist on colnames makes it general to
      if (!iparseCall_c %in% Xcols) {
        if (length(find(iparseCall_c)) > 0) {
          if (iparseCall[[iparseind]] == "%in%" || is.function(iparseCall[[iparseind]]) || is.function(get(iparseCall_c, find(iparseCall_c)))) {
            iparseEval <- eval(substitute(iparseCall[[iparseind]]), parent.frame())
            if (iparseCall_c == "c") {
              idx <- 2:length(iparseCall) # omit the first 'c' argument
              res <- character(0) # character result
              # we want to omit evaluating the 'c', but evaluate all of the internal items
              for (ii in idx) {
                res <- c(res, iparse(iparseCall[[ii]], iparseDepth = iparseDepth + 1, x)) # use c here in case of multiple nested levels since we don't have known lengths
              }
              return(res)
            }
          } else {
            # get the variable
            iparseEval <- eval(iparseCall[[iparseind]], parent.frame())
          }
          iparseCall[[iparseind]] <- iparseEval
        } # end if length(find(ccall_c)) > 0)
        # but, if dynGet returns, use that instead
        iparsedg <- dynGet(iparseCall_c, ifnotfound = "", minframe = 1L)
        # if dynGet found something
        if (any(iparsedg != "")) {
          iparseCall[[iparseind]] <- iparsedg
        }
      } # end if(!call_c)
    } # end if(inherits(iparseCall[[iparseind]], "name"))
    if (inherits(iparseCall[[iparseind]], "call")) {
      # if this is a call, recursively parse that
      iparseCall[[iparseind]] <- iparse(iparseCall[[iparseind]], iparseDepth = iparseDepth + 1, x = x)
      # if no vars are in colnames(x), evaluate the call right now
      if (any(!all.vars(iparseCall[[iparseind]]) %in% Xcols)) {
        # unclear if this tryCatch does anything, but it seems wise to keep it
        tryCatch(iparseTryResult <- eval(iparseCall[[iparseind]]),
          error = function(e) {
            co <- capture.output(print(iparseCall[[iparseind]]))
            stop(paste0("The condition ", dQuote(co), " cannot be evaluated."))
          }
        )
        # the condition resolves to NULL if, for example, it references
        # a list element that is not on the list. But the list itself is
        # on the parent.frame.
        if (is.null(iparseTryResult)) {
          co <- capture.output(print(iparseCall[[iparseind]]))
          stop(paste0("Condition ", dQuote(co), " cannot be evaluated."))
        }
        iparseCall[[iparseind]] <- iparseTryResult
      }
    } # end of if statment: if inherits(iparseCall[[i]], "call")
  } # end of for loop: i in 1:length(iparseCall)
  if (unlistOnExit) {
    iparseCall <- as.character(iparseCall[[1]])
  }
  iparseCall
} # End of fucntion: iparse

getXCols <- function(data) {
  res <- c()
  if (inherits(data, "edsurvey.data.frame.list")) {
    res <- unlist(itterateESDFL(match.call(), data))
    return(unique(res))
  }
  if (inherits(data, "edsurvey.data.frame") | inherits(data, "light.edsurvey.data.frame")) {
    res <- names(getAttributes(data, "pvvars", errorCheck = FALSE))
  }
  return(c(res, unlist(colnames(data))))
}

checkWeightVar <- function(data, weightVar) {
  if (is.null(weightVar)) {
    weightVar <- attributes(getAttributes(data, "weights"))$default
    if (min(nchar(weightVar)) == 0) {
      # no weight
      stop(paste0("There is no default weight variable for ", getAttributes(data, "survey"), " data, so the argument ", sQuote("weightVar"), " must be specified."))
    }
  }
  return(weightVar)
}

getMu <- function(x, w) {
  sum(x * w) / sum(w)
}

getS <- function(x, w, mu = NULL) {
  if (is.null(mu)) {
    mu <- getMu(x, w)
  }
  sqrt(sum(w * ((x - mu)^2)) / sum(w))
}
