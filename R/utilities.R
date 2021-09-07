# utilities functions

getAllTaylorVars <- function(data) {
  res <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
  res <- res[!res == ""]
  wgts <- getAttributes(data, "weights")
  for(wi in wgts) {
    if(!is.null(wi$stratumVar)) {
      res <- c(res, wi$stratumVar)
    }
    if(!is.null(wi$psuVar)) {
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


  if(is.null(allWeights)) {
    return(NULL)
  }

  # user specifies weightVar to be NULL; remind users of showWeights function
  if (is.null(weightVar)) {
    # if there is a default weight, print it in warning
    if(!is.null(defaultWeight)) {
      if(defaultWeight != "") {
        return(warning(paste0("Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument or replace the ",sQuote("weightVar"), " argument with the default weight: ",sQuote(defaultWeight), ".")))
      # if there is a default psuVar that is not not null, specify in the warning
      } else if(!is.null(psuVar)) {
        return(warning(paste0("The data has more than one weight variable without a default. Either remove the ",sQuote("weightVar"), " argument entirely or use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
      # if the default psuVar is null, direct users to showWeights
      } else {
        return(warning(paste0("The data may have more than one weight variable without a default. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
      }
    }
  }

  # if the specified weightVar is a valid weight and the psuVar is not null, return it
  if ((weightVar %in% names(allWeights) & !is.null(psuVar)) | (weightVar == defaultWeight & !is.null(psuVar))) {
    return(psuVar)
  }

  # if the specified weightVar is not a valid weight, return a warning and direct to showWeights
  if (!weightVar %in% names(allWeights) & weightVar != "") {
    if(defaultWeight == "" & is.null(psuVar)) {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
    } else {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Either remove the ",sQuote("weightVar"), " argument entirely or use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
    }
  }

  # if the specified weightVar is an empty string, return a warning
  if (weightVar == "") {
      return(warning(paste0("The data may have more than one weight variable without a default. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
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
  stratumVar <- getAttributes(data,"stratumVar")

  if(is.null(allWeights)) {
    return(NULL)
  }

  # user specifies weightVar to be NULL; remind users of showWeights function
  if (is.null(weightVar)) {
    # if there is a default weight, print it in warning
    if(!is.null(defaultWeight)) {
      if(defaultWeight != "") {
        return(warning(paste0("Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument or replace the ",sQuote("weightVar"), " argument with the default weight: ",sQuote(defaultWeight), ".")))
      # if there is a default stratumVar that is not not null, specify in the warning
      } else if(!is.null(stratumVar)) {
        return(warning(paste0("The data has more than one weight variable without a default. Either remove the ",sQuote("weightVar"), " argument entirely or use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
      # if the default stratumVar is null, direct users to showWeights
      } else {
        return(warning(paste0("The data may have more than one weight variable without a default. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
      }
    }
  }

  # if the specified weightVar is a valid weight and the stratumVar is not null, return it
  if ((weightVar %in% names(allWeights) & !is.null(stratumVar)) | (weightVar == defaultWeight & !is.null(stratumVar))) {
    return(stratumVar)
  }

  # if the specified weightVar is not a valid weight, return a warning and direct to showWeights
  if (!weightVar %in% names(allWeights) & weightVar != "") {
    if(defaultWeight == "" & is.null(stratumVar)) {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
    } else {
      return(warning(paste0(sQuote(weightVar), " is not a valid weight. Either remove the ",sQuote("weightVar"), " argument entirely or use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
    }
  }

  # if the specified weightVar is an empty string, return a warning
  if (weightVar == "") {
      return(warning(paste0("The data may have more than one weight variable without a default. Use the ",sQuote("showWeights"), " function to populate the ",sQuote("weightVar"), " argument.")))
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
  weights <- getAttributes(data,"weights")
  weights <- weights[which(names(weights) == weightVar)]

  # Validate weightVar
  if (length(weights) == 0) {
    stop("The data does not have any weight called ", sQuote(weightVar),".")
  }
  if (length(weights) > 1) {
    warning(paste0("The data has more than one weight variable called ", sQuote(weightVar),". Using the first such weight."))
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
pasteItems <- function(vector, final="and") {
  # no need to do anything if there is one or fewer elements
  if(length(vector) <= 1) {
    return(vector)
  }
  if(length(vector) == 2) {
    return(paste0(vector[1], " ", final, " ", vector[2]))
  }
  v <- vector[-length(vector)]
  f <- vector[length(vector)]
  return(paste0(paste(v, collapse=", "), ", ", final, " ", f))
}

eout <- function(text, exdent=2, indent=0) {
  txto <- strwrap(text,
                  width=getOption("width")*0.9,
                  indent=indent,
                  exdent=exdent)
  writeLines(txto)
}

fixPath <- function(path) {
  dir <- dirname(path)
  base <- basename(path)
  flist <- list.files(dir)
  if(!dir.exists(dir)) {
    stop(paste0("Could not find directory ", dQuote(dir), "."))
  }
  if(base %in% flist) {
    # this is a good file path, return it
    return(file.path(dir,base))
  }
  if(tolower(base) %in% tolower(flist)) {
    return(file.path(dir, flist[which(tolower(flist) %in% tolower(base))]))
  }
  stop(paste0("Could not find file ", dQuote(file.path(dir, base)), "."))
}

fixTimeout <- function() {
  newtimeo <- max(60*60, options()$timeout)
  options(timeout=newtimeo)
}

cartFactor <- function(fa, fb) {
  if(!inherits(fa, "factor")) {
    fa <- factor(fa)
  }
  if(!inherits(fb, "factor")) {
    fb <- factor(fb)
  }
  df <- expand.grid(levels(fb), levels(fa))
  return(paste0(df$Var2, ":", df$Var1))
}
