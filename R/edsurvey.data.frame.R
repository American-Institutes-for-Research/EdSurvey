# build and edsurvey.data.frame from all of the requisite parts.
# There is no need for an S3 object to have a constructor, but having one
# can help programmers (and maintainers) know that every edsurvey.data.frame
# has all the correct elements.

#' @title EdSurvey Class Constructors and Helpers
#' @rdname edsurvey-class
#' @description Two new classes in \code{EdSurvey} are described in this section: the \code{edsurvey.data.frame}
#'              and \code{light.edsurvey.data.frame}. The \code{edsurvey.data.frame}
#'              class stores metadata about survey data, and data are stored on the
#'              disk (via the \code{LaF} package), allowing gigabytes of data to be used easily on a machine otherwise
#'              inappropriate for manipulating large datasets.
#'              The \code{light.edsurvey.data.frame} is typically generated
#'              by the \code{getData} function and stores the data in a
#'              \code{data.frame}.
#'              Both classes use attributes to manage metadata and allow
#'              for correct statistics to be used in calculating results; the
#'              \code{getAttributes} acts as an accessor for these attributes, whereas
#'              \code{setAttributes} acts as a mutator for the attributes.
#'              As a convenience, \code{edsurvey.data.frame}
#'              implements the \code{$} function to extract a variable.
#'
#' @param data an \code{edsurvey.data.frame}
#' @param userConditions a list of user conditions that includes subsetting or recoding conditions
#' @param defaultConditions a list of default conditions that often are set for each survey
#' @param dataList a list of \code{dataListItem} objects to model the data structure of the survey
#' @param weights a list that stores information regarding weight variables. See Details.
#' @param pvvars a list that stores information regarding plausible values. See Details.
#' @param subject a character that indicates the subject domain of the given data
#' @param year a character or numeric that indicates the year of the given data
#' @param assessmentCode a character that indicates the code of the assessment.
#'                       Can be \code{National} or \code{International}.
#' @param dataType a character that indicates the unit level of the main data.
#'                 Examples include \code{Student}, \code{teacher}, \code{school},
#'                 \code{Adult Data}.
#' @param gradeLevel a character that indicates the grade level of the given data
#' @param achievementLevels a list of achievement-level categories and cutpoints
#' @param omittedLevels a list of default omitted levels for the given data
#' @param survey a character that indicates the name of the survey
#' @param country a character that indicates the country of the given data
#' @param psuVar a character that indicates the PSU sampling unit variable. Ignored when weights have \code{psuVar} defined.
#' @param stratumVar a character that indicates the stratum variable. Ignored when weights have \code{stratumVar} defined.
#' @param jkSumMultiplier a numeric value of the jackknife coefficient (used in calculating the jackknife replication estimation)
#' @param recodes a list of variable recodes of the given data
#' @param validateFactorLabels a Boolean that indicates whether the \code{getData} function needs to validate factor variables
#' @param forceLower a Boolean; when set to \code{TRUE}, will automatically lowercase variable names
#' @param reqDecimalConversion a Boolean; when set to \code{TRUE}, a \code{getData} call will multiply the raw file value by a decimal multiplier
#' @param cacheDataLevelName a character value set to match the named element in the \code{dataList} to utilize the data caching scheme.  See details.
#' @param fr2Path a character file location for NAEP assessments to identify the location of the codebook file in \code{fr2} format
#' @param x an \code{edsurvey.data.frame}
#' @param i a character, the column name to extract
#' @param attribute a character, name of an attribute to get or set
#' @param table an \code{edsurvey.data.frame} or \code{edsurvey.data.frame.list} where \code{x} is searched for
#' @param value outside of the assignment context, new value of the given \code{attribute}
#' @param weightVar a character indicating the full sample weights. Required in \code{getPSUVar} and \code{getStratumVar} when there is no default weight.
#' @param name a character vector of the column to edit
#' @param dim0 numeric vector of length two. To speed construction, the dimensions of the data can be provided
#' @param errorCheck logical; see Details
#'
#' @details
#'
#' The \code{weight} list has an element named after each weight variable name
#' that is a list with elements \code{jkbase} and \code{jksuffixes}. The
#' \code{jkbase} variable is a single character indicating the jackknife replicate
#' weight base name, whereas \code{jksuffixes} is a vector with one element for each
#' jackknife replicate weight. When the two are pasted together, they should form
#' the complete set of the jackknife replicate weights. The \code{weights} argument
#' also can have an attribute that is the default weight. If the primary sampling
#' unit and stratum variables change by weight, they also can be defined on the weight
#' list as \code{psuVar} and \code{stratumVar}. When this option is used, it overrides
#' the \code{psuVar} and \code{stratumVar} on the \code{edsurvey.data.frame},
#' which can be left blank. A weight must define only one of \code{psuVar}
#' and \code{stratumVar}.
#'
#' The \code{pvvars} list has an element for each subject or subscale score
#' that has plausible values. Each element is a list with a \code{varnames}
#' element that indicates the column names of the plausible values and an
#' \code{achievementLevel} argument that is a named vector of the
#' achievement-level cutpoints.
#'
#' An \code{edsurvey.data.frame} implements a unique data caching mechanism that allows users to create and merge data columns for flexibility.
#' This \code{cache} object is a single \code{data.frame} that is an element in the \code{edsurvey.data.frame}. To accommodate studies with complex data models
#' the cache can only support one data level at this time. The \code{cacheDataLevelName} parameter indicates which named element in the \code{dataList}
#' the cache is indicated. The default value \code{cacheDataLevelName = NULL} will set the first item in the \code{dataList} as the \code{cache} level for an \code{edsurvey.data.frame}.
#'
#'
#' @return
#' An object of class \code{edsurvey.data.frame} with the following elements:
#'
#' \emph{Elements that store data connections and data codebooks}
#'    \item{\code{dataList}}{a \code{list} object containing the surveys \code{dataListItem} objects}
#' \emph{Elements that store sample design and default subsetting information of the given survey data}
#'    \item{\code{userConditions}}{a list containing all user conditions, set using the \code{subset.edsurvey.data.frame} method}
#'    \item{\code{defaultConditions}}{the default subsample conditions}
#'    \item{\code{weights}}{a list containing the weights. See Details.}
#'    \item{\code{stratumVar}}{a character that indicates the default strata identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{\code{psuVar}}{a character that indicates the default PSU (sampling unit) identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{\code{pvvars}}{a list containing the plausible values. See Details.}
#'    \item{\code{achievementLevels}}{default achievement cutoff scores and names. See Details.}
#'    \item{\code{omittedLevels}}{the levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}}
#' \emph{Elements that store descriptive information of the survey}
#'    \item{\code{survey}}{the type of survey data}
#'    \item{\code{subject}}{the subject of the data}
#'    \item{\code{year}}{the year of assessment}
#'    \item{\code{assessmentCode}}{the assessment code}
#'    \item{\code{dataType}}{the type of data (e.g., \code{student} or \code{school})}
#'    \item{\code{gradeLevel}}{the grade of the dataset contained in the \code{edsurvey.data.frame}}
#' \emph{Elements used in \code{mml.sdf}}
#'    \item{\code{dichotParamTab}}{IRT item parameters for dichotomous items in a data frame}
#'    \item{\code{polyParamTab}}{IRT item parameters for polytomous items in a data frame}
#'    \item{\code{adjustedData}}{IRT item parameter adjustment information in a data frame}
#'    \item{\code{testData}}{IRT transformation constants in a data frame}
#'    \item{\code{scoreCard}}{item scoring information in a data frame}
#'    \item{\code{scoreDict}}{generic scoring information in a data frame}
#'    \item{\code{scoreFunction}}{a function that turns the variables with items in them into numeric scores}
#' @section EdSurvey Classes:
#' \code{edsurvey.data.frame} is an object that stores connection to data on the
#' disk along with important survey sample design information.
#'
#' \code{edsurvey.data.frame.list} is a list of \code{edsurvey.data.frame}
#' objects. It often is used in trend or cross-regional analysis in the
#' \code{\link{gap}} function. See \code{\link{edsurvey.data.frame.list}} for
#' more information on how to create an \code{edsurvey.data.frame.list}. Users
#' also can refer to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Trend.pdf}{\emph{Using EdSurvey for Trend Analysis}}
#' for examples.
#'
#' Besides \code{edsurvey.data.frame} class, the \code{EdSurvey} package also
#' implements the \code{light.edsurvey.data.frame} class, which can be used by both
#' \code{EdSurvey} and non-\code{EdSurvey} functions. More particularly,
#' \code{light.edsurvey.data.frame} is a \code{data.frame} that has basic
#' survey and sample design information (i.e., plausible values and weights), which
#' will be used for variance estimation in analytical functions. Because it
#' also is a base R \code{data.frame}, users can apply base R functions for
#' data manipulation.
#' See the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-getData.pdf}{\emph{Using the \code{getData} Function in EdSurvey}}
#' for more examples.
#'
#' Many functions will remove attributes from a data frame, such as
#' a \code{light.edsurvey.data.frame}, and the
#' \code{\link{rebindAttributes}} function can add them back.
#'
#' Users can get a \code{light.edsurvey.data.frame} object by using the
#' \code{\link{getData}} method with \code{addAttributes=TRUE}.
#'
#' @section Basic Methods for EdSurvey Classes:
#' \emph{Extracting a column from an \code{edsurvey.data.frame}}
#'
#' Users can extract a column from an \code{edsurvey.data.frame} object using \code{$} or \code{[]} like a normal data frame.
#'
#' \emph{Extracting and updating attributes of an object of class \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}}
#'
#' Users can use the \code{getAttributes} method to extract any attribute of
#' an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame}.
#' The \code{errorCheck} parameter has a default value of\code{TRUE}, which throws an error if an attribute is not found.
#' Setting \code{errorCheck = FALSE} will suppress error checking, and return \code{NULL} if an attribute can't be found.
#'
#' A \code{light.edsurvey.data.frame} will not have attributes related to data connection
#' because data have already been read in memory.
#'
#' If users want to update an attribute (i.e., \code{omittedLevels}), they can
#' use the \code{setAttributes} method.
#'
#' @seealso \code{\link{rebindAttributes}}
#' @example \man\examples\edsurvey-class.R
#'
#' @importFrom LaF close
#' @importFrom utils capture.output
#' @author Tom Fink, Trang Nguyen, and Paul Bailey
#' @export
edsurvey.data.frame <- function(userConditions,
                                defaultConditions,
                                dataList = list(),
                                weights,
                                pvvars,
                                subject,
                                year,
                                assessmentCode,
                                dataType,
                                gradeLevel,
                                achievementLevels,
                                omittedLevels,
                                survey,
                                country,
                                psuVar,
                                stratumVar,
                                jkSumMultiplier,
                                recodes = NULL,
                                validateFactorLabels = FALSE,
                                forceLower = TRUE,
                                reqDecimalConversion = TRUE,
                                fr2Path = NULL,
                                dim0 = NULL,
                                cacheDataLevelName = NULL) {
  if (is.list(achievementLevels)) {
    achievementLevels <- lapply(achievementLevels, function(l) {
      sort(l)
    })
  } else {
    # In the international assessment TALIS, there is no achievement level, account for that here
    if (!is.null(achievementLevels)) {
      achievementLevels <- sort(achievementLevels)
      # names can't be missing, but they can be length 0. Figure out if that is the case.
      if (min(nchar(names(achievementLevels))) == 0) {
        stop(paste0("The argument ", sQuote("achievementLevels"), " must have nonmissing names for each level."))
      }
      for (pvi in seq_along(pvvars)) {
        # setup achievement levels for all plausible value variables
        temp <- list(varnames = pvvars[[pvi]]$varnames)
        temp$achievementLevel <- achievementLevels
        pvvars[[names(pvvars)[pvi]]] <- temp
      }
    }
  } # closes else statment following if (is.list(achievementLevels))

  hasDim0 <- all(is.numeric(dim0)) # logical to indicate if dim0 was user provided in constructor
  if (!hasDim0) {
    dim0 <- c(0, 0) # temporary holder until we create the edsurvey.data.frame, then we will update it
  }

  if (is.null(cacheDataLevelName)) {
    cacheDataLevelName <- names(dataList)[1] # use first dataListItem level if not specified as default
  }

  if (forceLower) {
    i <- 1
    for (dl in dataList) {
      names(dl$lafObject) <- tolower(names(dl$lafObject))
      dl$fileFormat$variableName <- tolower(dl$fileFormat$variableName)
      dl$mergeVars <- tolower(dl$mergeVars)
      dl$parentMergeVars <- tolower(dl$parentMergeVars)

      dataList[[i]] <- dl
      i <- i + 1
    }
  }

  res <- list(
    userConditions = userConditions,
    defaultConditions = defaultConditions,
    dataList = dataList,
    weights = weights,
    pvvars = pvvars,
    subject = subject,
    year = year,
    assessmentCode = assessmentCode,
    dataType = dataType,
    gradeLevel = gradeLevel,
    achievementLevels = achievementLevels,
    omittedLevels = omittedLevels,
    survey = survey,
    country = country,
    psuVar = psuVar,
    stratumVar = stratumVar,
    jkSumMultiplier = jkSumMultiplier,
    recodes = recodes,
    dim0 = dim0, # update after it's reclassed to an edsurvey.data.frame (if not user supplied)
    validateFactorLabels = validateFactorLabels,
    cacheDataLevelName = cacheDataLevelName,
    reqDecimalConversion = reqDecimalConversion,
    fr2Path = fr2Path,
    dichotParamTab = NULL, # IRT Param
    polyParamTab = NULL, # IRT Param
    adjustedData = NULL, # IRT Param
    testData = NULL, # IRT Param
    scoreCard = NULL, # IRT Param
    scoreDict = NULL, # IRT Param
    scoreFunction = NULL # IRT scoring
  )
  if (is.null(res$year)) {
    res$year <- NA
  }
  # form cache, first grab dim
  ROWID <- 1
  # make cache
  res$cache <- data.frame(ROWID = ROWID)
  class(res) <- c("edsurvey.data.frame", "edsurvey.data")

  if (is.null(defaultConditions) || is.na(defaultConditions)) {
    cache <- data.frame(
      ROWID = 1:(dataList[[cacheDataLevelName]]$nrow),
      DEFAULT = TRUE
    )
  } else { # default conditions are supplied, calculate them here
    # use getData on this, as of yet incomplete edsurvey.data.frame
    # to get row IDs
    # supressWarnings because it is just about nrow
    checkDL <- dataList[[cacheDataLevelName]]
    checkVar <- checkDL$fileFormat$variableName[!(checkDL$fileFormat$variableName %in% checkDL$ignoreVars)][1]
    suppressWarnings(gd0 <- getData(res, varnames = checkVar, dropUnusedLevels = FALSE, dropOmittedLevels = FALSE, defaultConditions = FALSE))
    suppressWarnings(gd <- getData(res, varnames = checkVar, dropUnusedLevels = FALSE, dropOmittedLevels = FALSE))
    class(res) <- "list"
    cache <- data.frame(
      ROWID = 1:nrow(gd0),
      DEFAULT = 1:nrow(gd0) %in% rownames(gd)
    )
  }

  # change back to a list to allow assignment of the cache
  res$cache <- cache
  # return to edsurvey.data.frame
  class(res) <- c("edsurvey.data.frame", "edsurvey.data")

  if (hasDim0) {
    res$dim0 <- dim0
  } else {
    res$dim0 <- c(nrow(res), length(colnames(res)))
  }

  # we discovered that not closing a LaF connections leads to memory leaks.
  # so, close all of the connections when it is complete (constructed).
  for (item in dataList) {
    LaF::close(item$lafObject)
  }
  # check PSU/Stratum variables to see if 1) they are specified; if so, 2) they are defined in the data variables
  checkPS <- TRUE
  if (is.null(psuVar) || is.null(stratumVar)) {
    if (!all(c("psuVar", "stratumVar") %in% unlist(lapply(weights, names)))) {
      warning("PSU or Stratum variable(s) are undefined for this data. Taylor series variance estimation will not be possible.")
    }
    checkPS <- FALSE
  }
  if (checkPS) {
    # if the sampling method is not stratified (and so is JK1) then there are not PSU and stratum variables.
    if (any(!c(psuVar, stratumVar) %in% c("JK1", colnames(res)))) {
      warning("Cannot find both PSU and Stratum variables on data. Taylor series variance estimation will not be possible.")
    }
  }

  return(res)
}

#' @rdname edsurvey-class
#' @method $ edsurvey.data.frame
#' @export
"$.edsurvey.data.frame" <- function(x, i) {
  # for the attributes
  if (i %in% names(x)) {
    return(x[[i]])
  }

  success <- tryCatch(
    {
      z <- getData(x, varnames = i, dropUnusedLevels = FALSE, dropOmittedLevels = FALSE, drop = TRUE, returnJKreplicates = FALSE)
      TRUE
    },
    error = function(e) {
      warning(paste0("No object or data variable found named ", sQuote(i)))
      FALSE
    }
  )

  if (success) {
    return(z)
  } else {
    return(invisible(NULL))
  }
}

# @note The \code{dataListItem} function is a useful constructor for building the individual \code{dataList} heirarchy data model components of each \code{dataList} object of an \code{edsurvey.data.frame}.
#        It's recommended to always use this constructor, as it applies default values, calculations, validation checks, and to help for future functionality.
# @param lafObject an LaF object file that stores the data file connection and information.
# @param fileFormat a data.frame of the defined file format specifications of the data file.  See \code{edsurvey.data.frame} Details.
# @param levelLabel a character to describe the name of this \code{dataListItem}.  Must be unique from other \code{dataListItem} objects in the same \code{edsurvey.data.frame}.
# @param forceMerge a logical to determine if a \code{dataListItem} must always be merged in \code{getData}. A value \code{TRUE} causes the \code{dataListItem} to always be merged.
#                   Default is \code{FALSE}.
# @param parentMergeLevels a character which is of equal lengths to the \code{parentMergeVars} parameter specifying the \code{levelLabel} of the matching \code{parentMergeVars} variable.
# @param parentMergeVars a character which is of equal lengths to the \code{parentMergeLevels} and \code{mergeVars} parameter specifying the variable name the corresponding \code{parentMergeLevels}'s \code{dataListItem}
#                        to which will be merged to the \code{mergeVar} variable in this \code{dataListItem}.
# @param mergeVars a character which is of equal lengths to the \code{parentMergeLevels} and \code{parentMergeVars} parameter specifying this \code{dataListItem} variable name
#                        to which will be merged to the corresponding \code{parentMergeVars} variable in the level defined by \code{parentMergeLevels}.
# @param ignoreVars a character of variable name(s) to be ignored when testing if a \code{dataListItem} level should be merged or not.  Useful when \code{mergeVars} have matching names across \code{dataListItems}.
# @param isDimLevel a logical value of the \code{dataListItem} for which should be considered the 'full data N'.  Only one \code{dataListItem} in an \code{edsurvey.data.frame} should have a
#                   value of \code{TRUE}.  Default value is \code{FALSE}.
# @param conflictLevels a character vector of \code{levelLabels} that would cause a data merge/join conflict if they were returned alongside this \code{dataListItem} in a \code{getData} call.
#                         In certain situations data levels will be incompatible to be merged together, yet coexist in the same database and can be queried separately, but not joined together.
#                         Default value is \code{NULL} to indicate no data level merge/join conflicts.
#
dataListItem <- function(lafObject,
                         fileFormat,
                         levelLabel = "",
                         forceMerge = FALSE,
                         parentMergeLevels = NULL,
                         parentMergeVars = NULL,
                         mergeVars = NULL,
                         ignoreVars = NULL,
                         isDimLevel = FALSE,
                         conflictLevels = NULL) {
  if (!inherits(lafObject, "laf")) {
    stop(paste0("The ", dQuote("lafObject"), " must be a LaF object utilizing the LaF Package."))
  }

  if (length(parentMergeLevels) != length(parentMergeVars)) {
    stop(paste0("The lengths of ", dQuote("parentMergeLevels"), " and ", dQuote("parentMergeVars"), " must be equal."))
  }

  if (length(parentMergeVars) != length(mergeVars)) {
    stop(paste0("The lengths of ", dQuote("parentMergeVars"), " and ", dQuote("mergeVars"), " must be equal."))
  }

  res <- list(
    lafObject = lafObject,
    fileFormat = fileFormat,
    levelLabel = levelLabel,
    forceMerge = forceMerge,
    parentMergeLevels = parentMergeLevels,
    parentMergeVars = parentMergeVars,
    mergeVars = mergeVars,
    ignoreVars = ignoreVars,
    isDimLevel = isDimLevel,
    conflictLevels = conflictLevels,
    nrow = nrow(lafObject)
  )

  return(res)
}



#' @name subset
#' @title EdSurvey Subset
#' @aliases subset.edsurvey.data.frame.list subset.light.edsurvey.data.frame
#'
#' @description Subsets an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#' or a \code{light.edsurvey.data.frame}.
#'
#' @param x an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#'          or a \code{light.edsurvey.data.frame}
#' @param subset a logical expression indicating elements or rows to keep
#' @param inside set to \code{TRUE} to prevent the \code{substitute} condition
#'               from being called on it (see Details)
#' @param ... not used; included only for compatibility
#' @details Any variables defined on condition that are not references
#' to column names on the
#' \code{edsurvey.data.frame} and are part of the environment where
#' \code{subset.edsurvey.data.frame} was called will be evaluated
#' in the environment from which \code{subset.edsurvey.data.frame} was called.
#' Similar to the difficulty of using subset within a function call because of
#' the call to substitute on condition,
#' this function is difficult to use (with \code{inside} set to the default value of
#' \code{FALSE}) inside another function call.
#' See Examples for how to call this function from within another function.
#'
#' @return an object of the same class as \code{x}
#' @references
#' Wickham, H. (2014). \emph{Advanced R}. Boca Raton, FL: Chapman & Hall/CRC.
#'
#' @author Paul Bailey and Trang Nguyen
#' @example man\examples\subset.edsurvey.data.frame.R
#' @method subset edsurvey.data.frame
#' @export
subset.edsurvey.data.frame <- function(x, subset, ..., inside = FALSE) {
  if (!inherits(x, c("edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be an edsurvey.data.frame."))
  }

  # if there is a variable that is not in the data.frame, substitute any
  # value found in the parent.frame() right now.
  # This way, if the user adjusts a variable used in the subset, it will
  # have the value they would have expected from
  # when they called subset and the condition will not change as that
  # variable is updated.
  # add it to the user conditions
  if (inside) {
    if (inherits(subset, "name")) {
      subset <- eval(subset)
    }
    if (inherits(subset, "character")) {
      subset <- parse(text = subset)[[1]]
    }
    condition_call <- subset
  } else {
    # substitute in variables that are available in the current subsetEnvironment
    condition_call <- iparse(substitute(subset), x = x)
  } # Enf of if esle statmet: if imside is true
  # apply filter
  if (!all(all.vars(substitute(condition_call)) %in% colnames(x))) {
    condition_call <- iparse(condition_call, x = x)
  }
  x[["userConditions"]] <- c(x[["userConditions"]], list(condition_call))
  # test filter
  gg <- tryCatch(getData(x, c("ROWID", colnames(x)[1]), returnJKreplicates = FALSE),
    error = function(e) {
      subsetVars <- all.vars(condition_call)
      for (v in subsetVars) {
        if (!v %in% colnames(x)) {
          stop("Required variable ", sQuote(v), " is not found in the incoming data or on the environment.")
        }
      }
    },
    warning = function(w) {}
  )
  x$cache$DEFAULT <- ifelse(x$cache$ROWID %in% gg$ROWID, TRUE, FALSE)
  x
} # end of fuction subset.edsurvey.data.frame

#' @method [[ edsurvey.data.frame
#' @export
"[[.edsurvey.data.frame" <- function(x, i, j, ...) {
  # check for $ calls to list elements
  if (length(i) == 1) {
    if (i %in% names(x)) {
      class(x) <- "list"
      return(x[[i]])
    }
  }
  if (is.numeric(i)) {
    i <- colnames(x)[i]
  }
  suppressWarnings(
    z <- getData(x,
      varnames = i,
      dropUnusedLevels = FALSE, dropOmittedLevels = FALSE,
      addAttributes = TRUE, returnJKreplicates = FALSE
    )
  )
  z[j, ]
}

#' @method [<- edsurvey.data.frame
#' @export
"[<-.edsurvey.data.frame" <- function(x, i, j, ..., value) {
  value <- eval(value)
  if (!missing(i)) {
    i <- eval(i)
  }
  if (missing(j)) {
    stop("Assignmeht by row is not allowed on an edsurvey.data.frame. Try assigning a single column at a time.")
  }
  j <- eval(j)
  if (is.numeric(j)) {
    name <- colnames(x)[j]
  } else {
    name <- j
  }

  if (name %in% c("ROWID", "DEFAULT")) {
    stop("Colum names must not be reserved words (ROWID or DEFAULT).")
  }
  cl0 <- class(x)
  class(x) <- "list"
  if (is.null(value)) {
    x$cache[i, name] <- NULL
    class(x) <- cl0
    return(invisible(x))
  }
  # because of "mySDF$name[subset] <-" it is possible for a new factor to be valid
  # but for the below assignment to appear invalid, this clears the cache and
  # overwrite, which always works
  if (name %in% colnames(x$cache) && inherits(x$cache[ , name], "factor")) {
    x$cache[ , name] <- NULL
  }
  # cannot subset assign non-primative lfactors, so do not try
  if (inherits(value, "lfactor")) {
    value <- as.factor(value)
  }
  if (!missing(i)) {
    if (name %in% colnames(x$cache)) {
      # name already on the cache, we can directly assign a subset
      x$cache[x$cache$DEFAULT, name][i] <- value
    } else {
      # name not on the cache, we must assign the entire column
      DefaultInds <- (1:nrow(x$cache))[x$cache$DEFAULT]
      x$cache[DefaultInds[i], name] <- value
    }
  } else {
    x$cache[x$cache$DEFAULT, name] <- value
  }
  class(x) <- cl0
  invisible(x)
}

#' @method [ edsurvey.data.frame
#' @export
"[.edsurvey.data.frame" <- function(x, i, j, ...) {
  # check for $ calls to list elements
  if (missing(j)) {
    j <- colnames(x)
  }
  if (length(j) == 1) {
    if (j %in% names(x)) {
      class(x) <- "list"
      return(x[[j]])
    }
  }
  if (is.numeric(j)) {
    j <- colnames(x)[j]
  }
  suppressWarnings(
    z <- getData(x,
      varnames = j,
      dropUnusedLevels = FALSE, dropOmittedLevels = FALSE,
      addAttributes = TRUE, returnJKreplicates = FALSE
    )
  )
  z[i, ]
}


#' @rdname edsurvey-class
#' @method $<- edsurvey.data.frame
#' @export
"$<-.edsurvey.data.frame" <- function(x, name, value) {
  cl <- match.call()
  cl2 <- cl
  if (name %in% names(x)) {
    cl0 <- class(x)
    class(x) <- "list"
    x[[name]] <- value
    class(x) <- cl0
    invisible(x)
  } else {
    if (name %in% c("ROWID", "DEFAULT")) {
      stop("Colum names must not be reserved words (ROWID or DEFAULT).")
    }
    cl0 <- class(x)
    class(x) <- "list"
    if (is.null(value)) {
      x$cache[ , name] <- NULL
      class(x) <- cl0
      return(invisible(x))
    }
    # because of "mySDF$name[subset] <-" it is possible for a new factor to be valid
    # but for the below assignment to appear invalid, this clears the cache and
    # overwrite, which always works
    if (name %in% colnames(x$cache) && inherits(x$cache[ , name], "factor")) {
      x$cache[ , name] <- NULL
    }
    # cannot subset assign non-primative lfactors, so do not try
    if (inherits(value, "lfactor")) {
      value <- as.factor(value)
    }
    x$cache[x$cache$DEFAULT, name] <- value
    class(x) <- cl0
    invisible(x)
  }
}


#' @method + edsurvey.data
#' @export
"+.edsurvey.data" <- function(e1, e2) {
  if (inherits(e1, "edsurvey.data.frame") & inherits(e2, "edsurvey.data.frame")) {
    return(edsurvey.data.frame.list(list(e1, e2)))
  } else {
    if ((inherits(e1, "edsurvey.data.frame.list") & inherits(e2, "edsurvey.data.frame.list"))) {
      datalist <- c(e1$datalist, e2$datalist)
      return(edsurvey.data.frame.list(datalist))
    }
    if ((inherits(e1, "edsurvey.data.frame") | inherits(e1, "edsurvey.data.frame.list")) &
      (inherits(e2, "edsurvey.data.frame") | inherits(e2, "edsurvey.data.frame.list"))) {
      datalist <- list()
      if (inherits(e1, "edsurvey.data.frame.list")) {
        datalist <- c(e1$datalist, list(e2))
      }
      if (inherits(e2, "edsurvey.data.frame.list")) {
        datalist <- c(list(e1), e2$datalist)
      }
      return(edsurvey.data.frame.list(datalist))
    } else {
      stop(paste0(dQuote(.Generic), " not defined for ", pasteItems(dQuote(class(e1))), " and ", pasteItems(dQuote(class(e2)))))
    }
  }
}

#' @method == edsurvey.data
#' @export
"==.edsurvey.data" <- function(e1, e2) {
  equals.edsurvey.data(e1, e2, notFunction = identity)
}

#' @method != edsurvey.data
#' @export
"!=.edsurvey.data" <- function(e1, e2) {
  equals.edsurvey.data(e1, e2, notFunction = function(x) {
    return(!x)
  })
}

equals.edsurvey.data <- function(e1, e2, notFunction) {
  if (inherits(e1, "edsurvey.data.frame") & inherits(e2, "edsurvey.data.frame")) {
    return(notFunction(sameSurvey(e1, e2)))
  }
  if (inherits(e1, "edsurvey.data.frame.list") & inherits(e2, "edsurvey.data.frame.list")) {
    d1 <- e1$datalist
    d2 <- e2$datalist
    if (length(d1) != length(d2) & min(c(length(d1), length(d2))) != 1) {
      stop("Objects do not have the same number of elements in them.")
    }
    return(notFunction(unlist(lapply(1:length(d1), function(x) {
      sameSurvey(d1[[x]], d2[[x]])
    }))))
  }
  if ((inherits(e1, "edsurvey.data.frame") | inherits(e1, "edsurvey.data.frame.list")) &
    (inherits(e2, "edsurvey.data.frame") | inherits(e2, "edsurvey.data.frame.list"))) {
    # one is an edsurvey.data.frame, the other an edsurvey.data.frame.list
    if (inherits(e1, "edsurvey.data.frame.list")) {
      return(notFunction(unlist(lapply(e1$datalist, function(x) {
        sameSurvey(x, e2)
      }))))
    }
    if (inherits(e2, "edsurvey.data.frame.list")) {
      return(notFunction(unlist(lapply(e2$datalist, function(x) {
        sameSurvey(e1, x)
      }))))
    }
  }
  return(FALSE)
}


#' @rdname edsurvey-class
#' @export
setMethod(
  "%in%", signature(x = "edsurvey.data.frame"),
  function(x, table) {
    matchESDF(x, table)
  }
)

#' @rdname edsurvey-class
#' @export
setMethod(
  "%in%", signature(x = "edsurvey.data.frame.list"),
  function(x, table) {
    matchESDFL(x, table)
  }
)

matchESDF <- function(x, table, nomatch = NA_integer_, uncomparables = NULL) {
  if (inherits(table, "edsurvey.data.frame")) {
    sameSurvey(x, table)
  }
  if (inherits(table, "edsurvey.data.frame.list")) {
    dlist <- table$datalist
    for (i in 1:length(dlist)) {
      if (sameSurvey(x, dlist[[i]])) {
        return(TRUE)
      }
      return(FALSE)
    }
  }
  return(FALSE)
}


matchESDFL <- function(x, table, nomatch = NA_integer_, uncomparables = NULL) {
  # x is an edsurvey.data.frame.list
  dlistx <- x$datalist
  res <- rep(FALSE, length(dlistx))
  if (inherits(table, "edsurvey.data.frame")) {
    for (j in 1:length(dlistx)) {
      res[j] <- sameSurvey(dlistx[[j]], table)
    }
  }
  if (inherits(table, "edsurvey.data.frame.list")) {
    dlist <- table$datalist
    for (i in 1:length(dlist)) {
      for (j in 1:length(dlistx)) {
        res[j] <- res[j] || sameSurvey(dlistx[[j]], dlist[[i]])
      }
    }
  }
  return(res)
}
