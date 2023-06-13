#' @title Read Data to a Data Frame
#'
#' @description Reads in selected columns to a \code{data.frame} or a
#'              \code{light.edsurvey.data.frame}. On an \code{edsurvey.data.frame},
#'              the data are stored on disk.
#'
#' @param data an \code{edsurvey.data.frame} or
#'             a \code{light.edsurvey.data.frame}
#' @param varnames a character vector of variable names that will be returned.
#'                 When both \code{varnames} and
#'                 a \code{formula} are specified, variables associated with both are
#'                 returned. Set to \code{NULL} by default.
#' @param dropUnusedLevels a logical value. When set to the default value of
#'                         \code{TRUE}, drops unused levels of all factor
#'                         variables.
#' @param dropOmittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of all factor variables
#'                      that are specified in an \code{edsurvey.data.frame}. Use
#'                      \code{print} on an \code{edsurvey.data.frame} to see
#'                      the omitted levels. The omitted levels also can be
#'                      adjusted with \code{setAttributes}; see Examples.
#' @param defaultConditions a logical value. When set to the default value of
#'                          \code{TRUE}, uses the default conditions stored in
#'                           an \code{edsurvey.data.frame} to subset the data. Use
#'                          \code{print} on an \code{edsurvey.data.frame} to
#'                          see the default conditions.
#' @param drop a logical value. When set to the default value of \code{FALSE},
#'             when a single column is returned, it is still represented as a
#'             \code{data.frame} and is not converted to a vector.
#' @param formula a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}}.
#'                When included, \code{getData} returns data associated with
#'                all variables of the \code{formula}. When both \code{varnames} and a
#'                formula are specified, the variables associated with both are
#'                returned. Set to \code{NULL} by default.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as \code{recode} \code{=} \code{list(var1}
#'               \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to}
#'               \code{=} \code{"d"))}. See Examples.
#' @param includeNaLabel a logical value to indicate if \code{NA} (missing) values are
#'                       returned as literal \code{NA} values or as factor levels
#'                       coded as \code{NA}
#' @param addAttributes a logical value set to \code{TRUE} to get a
#'                      \code{data.frame} that can be used in calls to
#'                      other functions that usually would take an
#'                      \code{edsurvey.data.frame}. This \code{data.frame} also is called a \code{light.edsurvey.data.frame}.
#'                      See Description section in \code{\link{edsurvey.data.frame}} for
#'                      more information on \code{light.edsurvey.data.frame}.
#' @param returnJKreplicates a logical value indicating if JK replicate weights
#'                           should be returned. Defaults to \code{TRUE}.
#'
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}.
#'
#'
#' @details By default, an \code{edsurvey.data.frame} does not have data read
#' into memory until \code{getData} is called and returns a data frame.
#' This structure allows \code{EdSurvey} to have a minimal memory footprint.
#' To keep the footprint small, you need to limit \code{varnames} to just
#' the necessary variables.
#'
#' There are two methods of attaching survey attributes to a \code{data.frame}
#' to make it usable by the functions in the \code{EdSurvey} package (e.g., \code{lm.sdf}):
#' (a) setting the \code{addAttributes} argument to \code{TRUE} at in the call to \code{getData}
#' or (b) by appending the attributes to the data frame with \code{rebindAttributes}.
#'
#' When \code{getData} is called, it returns a data frame. Setting the
#' \code{addAttributes} argument to \code{TRUE} adds the survey attributes and
#' changes the resultant \code{data.frame} to a \code{light.edsurvey.data.frame}.
#'
#' Alternatively, a \code{data.frame} can be coerced into a \code{light.edsurvey.data.frame}
#' using \code{rebindAttributes}. See Examples in the \code{\link{rebindAttributes}} documentation.
#'
#' If both \code{formula} and \code{varnames} are populated, the
#' variables on both will be included.
#'
#' See the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-getData.pdf}{\emph{Using the \code{getData} Function in EdSurvey}}
#' for long-form documentation on this function.
#'
#' @return When \code{addAttributes} is \code{FALSE}, \code{getData} returns a
#' \code{data.frame} containing data associated with the requested
#' variables. When \code{addAttributes} is \code{TRUE}, \code{getData} returns a
#' \code{light.edsurvey.data.frame}.
#'
#' @seealso \code{\link{rebindAttributes}}, \code{\link{subset.edsurvey.data.frame}}
#'
#' @author Tom Fink, Paul Bailey, and Ahmad Emad
#' @example man\examples\getData.R
#' @importFrom LaF laf_open_fwf laf_open_csv
#' @importFrom data.table as.data.table merge.data.table
#' @export
getData <- function(data,
                    varnames = NULL,
                    drop = FALSE,
                    dropUnusedLevels = TRUE,
                    dropOmittedLevels = TRUE,
                    defaultConditions = TRUE,
                    formula = NULL,
                    recode = NULL,
                    includeNaLabel = FALSE,
                    addAttributes = FALSE,
                    returnJKreplicates = TRUE,
                    omittedLevels = deprecated()) {
  # check inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL

  if (!inherits(varnames, c("NULL", "character"))) stop("The ", sQuote("varnames"), " argument must be either NULL or a character vector.")
  if (!inherits(formula, c("NULL", "formula"))) stop("The ", sQuote("formula"), " argument must be either NULL or a formula.")
  if (!is.logical(drop)) stop("The ", sQuote("drop"), " argument must be logical.")
  if (!is.logical(dropUnusedLevels)) stop("The ", sQuote("dropUnusedLevels"), " argument must be logical.")

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "getData(omittedLevels)", "getData(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")

  if (!is.logical(defaultConditions)) stop("The ", sQuote("defaultConditions"), " argument must be logical.")
  if (!is.null(recode) & !is.list(recode)) stop(paste0("The ", sQuote("recode"), " argument must be a list."))
  if (is.null(varnames) & is.null(formula)) stop(paste0("At least one of ", sQuote("varnames"), " and ", sQuote("formula"), " must be not NULL."))

  # for edsurvey.data.frame.list, just return a list of results
  if (inherits(sdf, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(), sdf)
    if (addAttributes) {
      res <- edsurvey.data.frame.list(datalist = res, cov = data$covs)
    }
    return(res)
  }
  survey <- getAttributes(sdf, "survey")

  if (!inherits(sdf, "edsurvey.data.frame")) {
    # if this is a light.edsurvey.data.frame
    if (!missing(defaultConditions)) {
      warning(paste0("The argument ", sQuote("defaultConditions"), " is defined but will be ignored because the incoming data set is already a ", sQuote("light.edsurvey.data.frame"), "."))
    }
  }

  # LaF objects hold file format info and file connections.
  # Ensure the file connections are open for reading.
  # OS has limits on maximum open file connections so only open when needed, and close when not in use.
  on.exit(closeLaFConnections(sdf)) # ensure all connections get closed
  sdf <- openLaFConnections(sdf)

  # get variables from formula
  formulaVars <- all.vars(formula)
  varnames <- c(varnames, formulaVars)
  # Retrieve the default conditions
  dConditions <- NULL
  varNamesDefaults <- c()
  if (defaultConditions & !is.null(sdf$defaultConditions)) {
    if (inherits(sdf, "edsurvey.data.frame")) {
      dConditions <- sdf$defaultConditions[[1]]
    } else {
      dConditions <- attr(sdf, "defaultConditions")[[1]]
    }
    varNamesDefaults <- all.vars(dConditions)
  }

  # check if the variable names in recodes are in the data
  varRecodes <- c()
  if (!is.null(recode)) {
    varRecodes <- names(recode)
    # create a vector of bad recodes
    badRecodes <- recode[!varRecodes %in% colnames(sdf)]
    if (length(badRecodes) > 0) { # if there are elements on the vector
      warning(paste0("Recode variables ", paste(sQuote(badRecodes), collapse = ", "), " not found in dataset."))
    }
    sdf <- recode.sdf(sdf, recode)
    recode <- NULL
    # recode calls levelSDF which calls getData, which closes the LaF connections. This undoes that.
    sdf <- openLaFConnections(sdf)
  }

  # Retrieve user conditions
  userConditions <- getAttributes(sdf, "userConditions", errorCheck = FALSE)
  varNamesConditions <- c()
  for (i in seq_along(userConditions)) {
    if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
      # do nothing, these variables are not loaded in unless needed
    } else {
      condition <- userConditions[[i]]
      varNamesConditions <- c(varNamesConditions, all.vars(condition))
    }
  }

  # Using the conditions and default variables and varnames to read in data from LaF
  varNamesDefaults2 <- varNamesDefaults
  varNamesDefaults <- varNamesDefaults[which(varNamesDefaults %in% colnames(sdf))]
  varnamesAllConditions <- c(varNamesConditions, varNamesDefaults)
  vars <- c()
  v <- c()

  hasIRTAttr <- has_IRTAttributes(sdf, errorCheck = FALSE) # in mml.sdf.R file, no need to throw error if IRT not set
  hpv <- hasPlausibleValue(varnames, sdf) # Boolean vector that is TRUE when the variable has plausible values
  iw <- isWeight(varnames, sdf) # Boolean vector that is TRUE when the variable is a weight
  vars <- c(vars, varnames[!(iw | hpv)])
  vars_exclude_omitted <- c()

  if (any(hpv) || hasIRTAttr) { # if SDF has IRT attributes, we need to ensure those items are excluded from the 'omittedLevels' checks down the line
    if (any(hpv)) {
      pvs <- getPlausibleValue(varnames[hpv], sdf)
      vars_exclude_omitted <- c(vars_exclude_omitted, pvs[-1])
      vars <- c(vars, pvs)
    }
    if (hasIRTAttr) {
      if (any(hpv)) { # test if any vars are plausible value names
        items <- getAllItems(sdf, varnames[hpv]) # suitable for TIMSS for subject
      } else {
        items <- getAllItems(sdf, NULL) # suitable for NAEP, as all items are returned
      }
      vars_exclude_omitted <- c(vars_exclude_omitted, items)
    }
  } # if(any(hpv) || hasIRTAttr)

  # getAllPSUVar and getallStratumVar are defined in getData.R to return a vector of all the PSU & Stratum variables associated with the edsurvey.data.frame/light.edsurvey.data.frame
  vars_exclude_omitted <- c(vars_exclude_omitted, getAllPSUVar(sdf), getAllStratumVar(sdf))
  if (sum(iw) > 0 & returnJKreplicates == TRUE) {
    invisible(sapply(varnames[iw], function(x) {
      v <- getWeightJkReplicates(x, sdf)
      vars_exclude_omitted <<- c(vars_exclude_omitted, v[-1])
      vars <<- c(vars, v, x)
    }))
  }
  if (sum(iw) > 0 & returnJKreplicates == FALSE) {
    vars <- c(vars, varnames[iw])
  }
  # remove any duplicates (e.g. because part of default conditions and requested)
  varnames <- unique(vars)
  # expand varnamesAllConditions when it has a variable with PVs or is a weight
  # also make varnamesTotal
  vars <- c()
  v <- c()
  if (inherits(sdf, "edsurvey.data.frame")) {
    for (i in seq_along(varnamesAllConditions)) {
      if (hasPlausibleValue(varnamesAllConditions[i], sdf)) {
        v <- getPlausibleValue(varnamesAllConditions[i], sdf)
        vars <- c(vars, v)
      } else {
        if (isWeight(varnamesAllConditions[i], sdf)) {
          v <- getWeightJkReplicates(varnamesAllConditions[i], sdf)
          vars <- c(vars, varnamesAllConditions[i], v)
        } else {
          vars <- c(vars, varnamesAllConditions[i])
        }
      }
    } # end for(i in (1:length(varnamesAllConditions)))
    varnamesAllConditions <- vars
    varnamesTotal <- c(varnamesAllConditions, varnames, varRecodes)
  } else { # end if(inherits(sdf, "edsurvey.data.frame"))
    varnamesTotal <- c(varnames)
  } # end else for if(inherits(sdf, "edsurvey.data.frame"))
  varnamesTotal <- unique(varnamesTotal)
  # exclude variables in userConditions but not in called variables
  vars_exclude_omitted <- c(vars_exclude_omitted, varnamesAllConditions[!varnamesAllConditions %in% varnames])

  # find cache variables
  cachedVars <- varnamesTotal[varnamesTotal %in% colnames(sdf$cache)]
  hasCachedVars <- (length(cachedVars) > 0)
  uncachedVars <- varnamesTotal[!varnamesTotal %in% cachedVars]

  # this section is for retreiving the data from a LaF
  if (inherits(sdf, "edsurvey.data.frame")) {
    # prepare new objects for storing the merged data and label files. must be done outside the loop
    data <- NULL
    labelsFile <- NULL

    if (hasCachedVars) {
      cacheLevelLabel <- sdf$dataList[[sdf$cacheDataLevelName]]$levelLabel # the name used for the dataList  might be different than the 'levelLabel'
    }

    # for determining which data levels we did in fact merge for use later in function
    mergeLevels <- rep(FALSE, length = length(sdf$dataList))
    names(mergeLevels) <- sapply(sdf$dataList, function(dl) {
      dl$levelLabel
    })

    # the mergeVars list will be a list of variables at the named dataLevel in which will be needed for merging based on the resulting test in the loop below
    mergeVars <- vector("list", length = length(sdf$dataList))
    names(mergeVars) <- sapply(sdf$dataList, function(dl) {
      dl$levelLabel
    })

    # loop through dataList in reverse order to determine the highest levels that have requested variables.  mark those levels, and their required parent levels as 'TRUE'
    for (dlevel in rev(sdf$dataList)) {
      # check if the level requires to be merged either by:
      # 1) forceMerged Flag is TRUE.
      # 2) was previously flagged as being required for merge, ensure it has it's merge variables marked.
      # 3) is the cache data level and cache variables are requested.
      # 4) Has a variable requested by the user that is not contained in the levels 'ignoreVars' list.
      if (dlevel$forceMerge ||
        mergeLevels[names(mergeLevels) == dlevel$levelLabel] == TRUE ||
        (hasCachedVars && dlevel$levelLabel == cacheLevelLabel) ||
        any(dlevel$fileFormat$variableName[!dlevel$fileFormat$variableName %in% dlevel$ignoreVars] %in% uncachedVars)) {
        mergeLevels[names(mergeLevels) == dlevel$levelLabel | names(mergeLevels) %in% dlevel$parentMergeLevels] <- TRUE # specify which levels we need to specifically loop through/merge

        # store the specific 'parent' merge variables under the level (by name) based on 'parentMergeLevels' and 'parentMergeVars'
        for (pLevel in unique(dlevel$parentMergeLevels)) {
          mergeVars[[pLevel]] <- unique(c(mergeVars[[pLevel]], dlevel$parentMergeVars))
        }
      }
    } # end for(dlevel in rev(sdf$dataList))
    # loop through each data item and see if any variables are requested and process accordingly to the dataList rules

    # after determining which data levels are requested, check if there are any conflicting data/merge levels set in the 'conflictLevels' of the dataListItems
    dataConflicts <- list() # if any data conflicts store them for a robust error message
    for (dlevel in sdf$dataList) {
      if (dlevel$forceMerge || mergeLevels[names(mergeLevels) == dlevel$levelLabel] == TRUE) {
        dataConflictCheck <- dlevel$conflictLevels %in% names(mergeLevels[mergeLevels == TRUE])
        if (any(dataConflictCheck)) {
          # build error message list
          stop(paste0(
            "Requested variables between data level ",
            dQuote(dlevel$levelLabel), " and data level(s): ", paste0(dQuote(dlevel$conflictLevels[dataConflictCheck]), collapse = " & "),
            " create a merge conflict and cannot be returned together in a ", dQuote("getData"), " call.",
            " Use the ", dQuote("showCodebook()"), " function to determine which variables are in which file format."
          ))
        }
      }
    }

    # perform the actual data merging into one result set  after checks are complete
    for (dlevel in sdf$dataList) {
      if (dlevel$forceMerge || mergeLevels[names(mergeLevels) == dlevel$levelLabel] == TRUE) {
        laf <- dlevel$lafObject
        ff <- dlevel$fileFormat

        levelVars <- dlevel$fileFormat$variableName # get all variable names
        levelVars <- levelVars[!levelVars %in% dlevel$ignoreVars] # remove ignored variables
        levelVars <- levelVars[levelVars %in% uncachedVars] # determine which specific variables are requested by user

        parentMergeVars <- mergeVars[[dlevel$levelLabel]][] # grab any parent merge variables required from the initial scan that are stored for this level

        # test here if variables are found as it might be misspelled or missing
        if (length(levelVars) == 0 && length(parentMergeVars) == 0 && !(dlevel$forceMerge || (hasCachedVars && dlevel$levelLabel == cacheLevelLabel))) {
          next
        }

        varinds <- which(names(laf) %in% c(levelVars, dlevel$mergeVars, parentMergeVars), arr.ind = TRUE)
        if (length(varinds) == 0) {
          varinds <- 1
        }

        dataChunk <- laf[ , varinds, drop = FALSE] # retrieve the values from the LaF

        if (hasCachedVars && dlevel$levelLabel == cacheLevelLabel) {
          if (ncol(dataChunk) > 0) {
            # the cache data with same column names will have precedence!
            dataChunk <- dataChunk[ , !(names(dataChunk) %in% cachedVars), drop = FALSE]
            dataChunk <- cbind(dataChunk, sdf$cache[cachedVars])
          } else {
            dataChunk <- sdf$cache[cachedVars]
          }
        }

        if (is.null(data)) {
          data <- dataChunk
          labelsFile <- ff[varinds, ]
        } else {
          data$zzz___SORTORDER1 <- 1:nrow(data) # number the datarows to ensure we keep original source, we will reorder after merge
          dataChunk$zzz___SORTORDER2 <- 1:nrow(dataChunk)

          # prep as data.table objects with key indexes for performance.  setting keys will mess with sort ordering of the data
          data <- data.table::as.data.table(data, key = dlevel$parentMergeVars)
          dataChunk <- data.table::as.data.table(dataChunk, key = dlevel$mergeVars)

          # perform the data merge
          data <- data.table::merge.data.table(data, dataChunk, all.x = TRUE, all.y = FALSE, by.x = dlevel$parentMergeVars, by.y = dlevel$mergeVars, suffixes = c("", ".dupe"))

          # back to base data.frame
          data <- as.data.frame(data) # back to regular data.frame
          data <- data[ , names(data)[!grepl("\\.dupe$", names(data))], drop = FALSE]

          # check here to see if the variable names differed between the parent and level
          # if there is a mismatch we need to insert
          if (any(dlevel$parentMergeVars != dlevel$mergeVars)) {
            idx <- which(dlevel$parentMergeVars != dlevel$mergeVars, arr.ind = TRUE)
            # duplicate the column value based on the parent name::note that this will only ever apply to merge variables with varying names
            for (i in idx) {
              data[ , dlevel$mergeVars[i]] <- data[ , dlevel$parentMergeVars[i]]
            }
          } # end if(any(dlevel$parentMergeVars!=dlevel$mergeVars))
          ff <- ff[varinds, ] # subset just to the grabbed values
          ff <- ff[!(ff$variableName %in% labelsFile$variableName), ] # filter out values already on data.frame
          labelsFile <- rbind(labelsFile, ff)

          # reorder to ensure original row ordering intact after merge
          data <- data[order(data$zzz___SORTORDER1, data$zzz___SORTORDER2), , drop = FALSE]
          data$zzz___SORTORDER1 <- NULL
          data$zzz___SORTORDER2 <- NULL
        } # end if(is.null(data))
      } # if(dlevel$forceMerge || mergeLevels[names(mergeLevels)==dlevel$levelLabel]==TRUE)
    } # end endfor(dlevel in sdf$dataList)

    # tidy up and drop any columns not requested by user
    dataChunk <- NULL

    data <- data[ , (names(data) %in% varnamesTotal), drop = FALSE]

    if (!is.null(data)) {
      row.names(data) <- 1:nrow(data)
    }

    # validation check to ensure we have all the fields that were requested
    errorVars <- varnames[!(varnames %in% names(data))]
    if (sum(nchar(errorVars)) > 0) {
      sdf <- closeLaFConnections(sdf)
      stop(paste0("The following variable names are required for this call but not found in the data: ", paste(sQuote(errorVars), collapse = ", "), "."))
    }

    labelsFile <- labelsFile[order(labelsFile$Start), ]
    labelsFile <- labelsFile[labelsFile$variableName %in% uncachedVars, ]

    # Give warning to user that 'totwgt' (student level weight) may give incorrect results when teacher data is merged::first ensure there is a 'Teacher' level
    if (sdf$survey %in% c("TIMSS", "PIRLS", "TIMSS Advanced", "CivED") && any(names(data) == "totwgt") && any(grepl("teacher", names(mergeLevels), ignore.case = TRUE))) {
      if (mergeLevels[[grep("teacher", names(mergeLevels), ignore.case = TRUE)]] == TRUE) {
        warning("Teacher Data has been Merged.  The student level 'totwgt' weight variable may not produce correct results in analysis.  See documentation.")
      }
    } # end if (sdf$survey %in% c("TIMSS", "PIRLS", "TIMSS Advanced") && hasTeacherVars)

    #### ASSIGN THE LABEL VALUES TO THE MERGED/PROCESSED DATA.FRAME####
    # apply the decimal conversion prior to applying the labels AND the merge in case we have a decimal value used as the key in the key/value label or need to adjust any weights
    # PISA reads in csv files so no need to convert to decimal values
    if (sdf$reqDecimalConversion == TRUE) {
      data <- applyDecimalConversion(dataDF = data, labelsDF = labelsFile)
    }

    labels <- getValueLabelList(labelsFile) # get a named list of the value labels
    data <- applyValueLabels(data = data, lblList = labels, labelDF = labelsFile, esdf = sdf, includeNaLabel = includeNaLabel)

    # check if variable can be converted to numeric. Some "labels" are just numbers.
    # first check if any of the 'attributes' from the 'file format' object are set to '(A)', omit those.  The (A) comes from the SPSS file format (if there is one).
    if (any(tolower(colnames(labelsFile)) == "attributes")) {
      attrCol <- colnames(labelsFile)[tolower(colnames(labelsFile)) == "attributes"]
      skipVars <- tolower(labelsFile$variableName[tolower(labelsFile[[attrCol]]) %in% c("(a)")]) # the (A) denotes it's alphanumeric in SPSS formatting and can be skipped
      convToNumVars <- names(data)[!tolower(names(data)) %in% skipVars]
    } else {
      convToNumVars <- names(data)
    }

    for (var in convToNumVars) {
      # convToNum converts the variable to a number--if it will not cause data loss
      data[ , var] <- convToNum(data[ , var])
    }

    # Apply default conditions
    # first check if they are valid
    if (any(!varNamesDefaults2 %in% names(data))) {
      ind <- varNamesDefaults2[which(!varNamesDefaults2 %in% names(data))]
      warning(paste0(
        sQuote(ind),
        " from default conditions not found in data. Setting ",
        sQuote("defaultConditionts"), " to ", dQuote("FALSE"), "."
      ))
      defaultConditions <- FALSE
    }

    # then actually apply defaultConditions
    if (defaultConditions) {
      if (length(dConditions) > 0) {
        r <- eval(dConditions, data)
        data <- data[r, , drop = FALSE]
      }
    }

    # Apply user conditions (i.e. subset, recode)
    for (i in seq_along(userConditions)) {
      if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
        recode <- userConditions[[i]]
        if (!is.null(recode)) {
          # apply recodes
          for (i in 1:length(recode)) {
            if (names(recode)[i] %in% colnames(data)) {
              ni <- names(recode)[i]
              from <- recode[[i]]$from
              to <- recode[[i]]$to
              if (length(to) > 1) {
                stop(paste0("More than one 'To' value found in the ", sQuote(ni), " element of the 'recode' argument."))
              }

              badFrom <- c() # levels with incorrect recodes
              if (inherits(data[ , ni], "factor")) {
                newto <- to
                if (to %in% from) { # remove degenerate recode
                  from <- from[!from %in% to]
                }
                labs <- levels(data[ , ni]) # used for both lfactors and factors
                if (newto %in% labs) { # this is not a new label
                  newto <- NULL
                }
                tmp <- as.character(data[ , ni])
                if (inherits(data[ , ni], "lfactor")) { # it is an lfactor
                  levs <- llevels(data[ , ni])
                  # in case of lfactor:
                  # + from can be numeric or character
                  # + to can be numeric or character
                  # To simplify the code, if to is a numeric, we will coerce it to character
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
                      toNum <- max(levs, na.rm = TRUE) + 1
                      levs <- c(levs, toNum)
                    } else {
                      toNum <- levs[which(to %in% labs)]
                    }
                  }
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
                    tmp_numeric <- lfactors:::switchllevels(data[ , ni])
                    tmp[tmp_numeric %in% fromNum] <- to
                    if (any(!fromNum %in% levs)) {
                      # add any missing levels to missing list
                      badFrom <- fromNum[!fromNum %in% levs]
                    }
                    labs <- labs[!levs %in% setdiff(fromNum, toNum)]
                    levs <- levs[!levs %in% setdiff(fromNum, toNum)]
                  }
                  # changing tmp according to character values of from
                  if (length(fromChar) > 0) {
                    tmp[tmp %in% fromChar] <- to
                    if (any(!fromChar %in% labs)) {
                      badFrom <- c(badFrom, fromChar[!fromChar %in% labs])
                    }
                    levs <- levs[!labs %in% setdiff(fromChar, to)]
                    labs <- labs[!labs %in% setdiff(fromChar, to)]
                  }
                  # Now we need to call lfactors again to make sure levels are mapped correctly to modified character vectors
                  data[ , ni] <- lfactor(tmp, levels = levs, labels = labs, exclude = NULL)
                } else { # end if(inherits(x[ ,ni],"lfactor"))
                  # it is a base r factor so from and to have to be character
                  tmp[tmp %in% from] <- to
                  if (any(!from %in% labs)) {
                    # add any missing levels to missing list
                    badFrom <- c(badFrom, from[!from %in% labs])
                  }
                  if (!to %in% labs) {
                    labs <- c(labs, to)
                  }
                  data[ , ni] <- factor(tmp, levels = labs)
                }
              } else { # end if(inherits(x[ ,ni], "factor"))
                # recode for non factors
                if (any(!from %in% data[ , ni])) {
                  badFrom <- from[!from %in% data[ , ni]]
                }
                data[ , ni][data[ , ni] %in% from] <- to
              } # end else for if(inherits(data[ ,ni], "factor"))
              if (length(badFrom) > 0) {
                warning(paste0(
                  "When recoding, could not find the level(s) ",
                  pasteItems(dQuote(badFrom), final = "or"),
                  " in the variable ", dQuote(ni), "."
                ))
              }
            } # end if(names(recode)[i] %in% colnames(data))
          } # end for (i in 1:length(recode))
        } # end if(!is.null(recode))
      } else { # other userConditions are specified in subset
        condition <- userConditions[[i]]
        r <- eval(condition, data)
        r <- ifelse(is.na(r), FALSE, r) # remove NA
        data <- data[which(r), , drop = FALSE]
      }
    } # end for (i in seq_along(userConditions))


    # drop the omitted levels if TRUE
    if (dropOmittedLevels) {
      keep <- rep(0, nrow(data))
      for (i in 1:length(varnamesTotal)) {
        vari <- varnamesTotal[i]
        if (!vari %in% vars_exclude_omitted) {
          # omit data at these levels
          keep <- keep | (data[ , vari] %in% sdf$omittedLevels)
        }
      }
      if (any(!keep)) {
        # only omit if something gets omitted
        data <- data[!keep, , drop = FALSE]
      }
    }


    # call droplevels on data when dropUnusedLevels=TRUE
    if (dropUnusedLevels) {
      for (i in 1:length(varnamesTotal)) {
        if (is.factor(data[ , varnamesTotal[i]])) {
          data[ , varnamesTotal[i]] <- droplevels(data[ , varnamesTotal[i]])
        }
      }
    }
  } else { # end if(inherits(sdf, "edsurvey.data.frame"))
    missingVars <- varnames[!varnames %in% vars_exclude_omitted & !varnames %in% c(colnames(sdf))]
    if (length(missingVars) > 0) {
      sdf <- closeLaFConnections(sdf)
      stop(paste0("The following variable names are required for this call and are not on the incoming data ", pasteItems(dQuote(missingVars)), "."))
    }

    varnamesTotal <- varnames
    data <- sdf

    # drop omitted levels if TRUE
    if (dropOmittedLevels) {
      lev <- unlist(attributes(sdf)$omittedLevels)
      keep <- rep(0, nrow(data))
      for (i in 1:length(varnamesTotal)) {
        vari <- varnamesTotal[i]
        if (!vari %in% vars_exclude_omitted) {
          # omit data at these levels
          keep <- keep + (data[ , vari] %in% lev)
        }
      }
      if (sum(keep > 0) > 0) {
        # only omit if something gets omitted
        data <- data[keep == 0, , drop = FALSE]
      }
    }

    # dropUnusedLevels for a light.edsurvey.data.frame
    if (!missing(dropUnusedLevels)) {
      if (dropUnusedLevels) {
        for (i in 1:length(varnamesTotal)) {
          if (is.factor(data[ , varnamesTotal[i]])) {
            data[ , varnamesTotal[i]] <- droplevels(data[ , varnamesTotal[i]])
          }
        }
      }
    }
    data <- data[ , varnames, drop = FALSE]
  } # end else for if(inherits(sdf, "edsurvey.data.frame"))
  # now the variable 'data' has a data.frame in it

  sdf <- closeLaFConnections(sdf) # ensure we close the LaF connections
  if (addAttributes) {
    # the user requested a light.edsurvey.data.frame
    # these have everything in attributes (as well as the data already being read in)
    if (nrow(data) == 0) {
      warning("The requested dataset has 0 rows.")
    }
    data <- data[ , varnames, drop = drop]
    class(sdf) <- "list"
    # get the names of the attributes
    sdfnames <- names(sdf)
    # exclude the "data" attribute
    sdfnames <- sdfnames[!sdfnames %in% c("data", "cache")]
    # add every other attribute to "data"
    lapply(sdfnames, function(x) {
      dat <- get("data")
      if (!is.null(sdf[[x]])) {
        attr(dat, x) <- sdf[[x]]
      } else {
        attr(dat, x) <- "NULL"
      }
      data <<- dat
    })
    # reset userConditions to remove recode (because its already applied)
    class(data) <- c("light.edsurvey.data.frame", class(data))
    data <- setAttributes(data, "userConditions", userConditions[which(!names(userConditions) %in% "recode")])
    return(data)
  } # end if(addAttributes)
  if (nrow(data) == 0) {
    warning("The requested dataset has 0 rows.")
  }
  data <- data[ , varnames, drop = drop]

  return(data)
}

# convToNum converts the variable to a number--if it will not cause data loss
convToNum <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x3 <- trimws(x)
  x3[x3 == ""] <- NA
  suppressWarnings(x2 <- as.numeric(x3))
  if (sum(is.na(x2)) == sum(is.na(x3))) {
    # there are NAs/empty strings in the original data
    # in every pace where there are NAs in the numeric version
    # so return the numeric version
    return(x2)
  }
  # there are values other than the empty string that do not convert to numeric
  # so just keep the text
  return(x)
}

# openLaFConnections ensures any closed LaF connections to files are opened for gathering data
openLaFConnections <- function(sdf) {
  # record initial class and reset to list
  class0 <- class(sdf)
  class(sdf) <- "list"
  i <- 1
  for (item in sdf$dataList) {
    # establish LaF connections if they are not opened
    if (!is.null(item$lafObject)) { # ensure we have a LaF object here the student object
      if (item$lafObject@file_id < 0) { # test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.  Must supply 'column_types' as character vector in this instance
        if (item$lafObject@file_type == "fwf") {
          newLaF <- LaF::laf_open_fwf(
            filename = item$lafObject@filename,
            column_types = item$fileFormat$dataType,
            column_widths = item$lafObject@column_widths,
            column_names = item$lafObject@column_names,
            dec = item$lafObject@options$dec,
            trim = item$lafObject@options$trim
          )
        } else if (item$lafObject@file_type == "csv") {
          newLaF <- LaF::laf_open_csv(
            filename = item$lafObject@filename,
            column_types = item$fileFormat$dataType,
            column_names = item$lafObject@column_names,
            sep = item$lafObject@options$sep,
            dec = item$lafObject@options$dec,
            trim = item$lafObject@options$trim,
            skip = item$lafObject@options$skip
          )
        } else {
          stop("Unexpected LaF object type provided.  Expects the LaF object type of 'fwf' or 'csv'")
        }

        # must assign it to overall SDF instead of loop var otherwise it's pointing to copy in memory
        sdf$dataList[[i]]$lafObject <- newLaF
      }
    }

    i <- i + 1
  } # end for(item in sdf$dataList)
  # reset class
  class(sdf) <- class0
  return(sdf)
}

# openLaFConnections ensures any closed LaF connections to files are opened for gathering data
closeLaFConnections <- function(sdf) {
  # ensure each dataList item has an LaF_Obj and then test if the file connection is already closed (as indicated by -1 file_id)
  for (item in sdf$dataList) {
    if (!is.null(item$lafObject)) {
      if (!(item$lafObject@file_id < 0)) {
        LaF::close(item$lafObject)
      }
    }
  }

  return(sdf)
}

# returns a vector of all associated PSU variables for an edsurvey.data.frame
getAllPSUVar <- function(sdf) {
  psuVar <- getAttributes(sdf, "psuVar", errorCheck = FALSE)
  wgts <- getAttributes(sdf, "weights")

  wgtPSUVars <- sapply(wgts, function(x) {
    x$psuVar
  })

  retVals <- c(psuVar, wgtPSUVars)
  return(retVals[!is.null(retVals) & !is.na(retVals)])
}

# returns a vector of all associated stratum variables for an edsurvey.data.frame
getAllStratumVar <- function(sdf) {
  stratumVar <- getAttributes(sdf, "stratumVar")
  wgts <- getAttributes(sdf, "weights")

  wgtStratumVars <- sapply(wgts, function(x) {
    x$stratumVar
  })

  retVals <- c(stratumVar, wgtStratumVars)
  return(retVals[!is.null(retVals) & !is.na(retVals)])
}

# this will return a named list object of the value labels.
# e.g., list(varA = list(keys = c(1, 2, 3), values = "Item 1", "Item 2", "Item 3"))
getValueLabelList <- function(labelsDF, varNameColumn = "variableName", valLblColumn = "labelValues") {
  labels <- list()

  # filter to only records that have a value label defined
  labelsDF <- labelsDF[!is.na(labelsDF[[valLblColumn]]) & (nchar(labelsDF[[valLblColumn]]) > 0), ]
  if (nrow(labelsDF) == 0) {
    return(labels) # return empty list
  }

  # get our variable names and the value label strings to parse
  # value labels will have a '^' as the major separator and the first '=' is the secondary separator
  # e.g., 0=Item 1^1=Item 2^2=Item 3^99=Missing
  varNames <- labelsDF[[varNameColumn]]
  valLbls <- labelsDF[[valLblColumn]]

  for (iDF in seq_len(nrow(labelsDF))) {
    keysTemp <- c()
    keys <- c()
    values <- c()

    variable <- varNames[iDF]
    keysTemp <- c(keysTemp, strsplit(valLbls[iDF], "^", fixed = TRUE)[[1]])

    if (length(keysTemp) > 0) {
      for (j in seq_along(keysTemp)) {
        tokens <- strsplit(keysTemp[j], "=", fixed = TRUE)

        keys <- c(keys, tokens[[1]][1])
        # in case the label has a true '=' symbol, we will then need to re-join all but the first element
        temp <- paste0(tokens[[1]][-1], collapse = "=")
        if (temp == "" | is.na(temp)) {
          temp <- "label unknown"
        }
        values <- c(values, temp)
      } # end for (j in seq_along(keysTemp))

      labels[[variable]] <- list(keys = keys, values = values)
    } # end if(length(keysTemp) > 0)
  } # end for(i in seq_len(labelsDF))

  return(labels)
}

# apply the named 'labels' list to the data.frame, this will build the factors and lfactors for the data, then reassign the data to that variable
applyValueLabels <- function(data, lblList, labelDF, esdf, includeNaLabel = FALSE) {
  # labelled is TRUE/FALSE for PISA only

  lblNames <- names(lblList)

  for (i in seq_along(lblNames)) {
    vari <- lblNames[i]
    if (!vari %in% names(data)) {
      next
    }

    lvls <- lblList[[i]]$keys
    lvlsAllNumeric <- isAllNumeric(lvls)
    lbls <- lblList[[i]]$values

    dataIsNumeric <- isAllNumeric(data[[vari]])

    if (length(unique(lvls)) != length(lvls)) {
      warning(paste0("Duplicate variable label key ", dQuote(i), " in variable ", vari, "."))
    }

    # in TIMSS files there are some variables that are real / integer and there is one omitted/invalid code
    # in PIAAC, there is variable that has Missing in value labels but it shouldn't be omitted
    if (esdf$survey != "PIAAC" && all(lbls %in% esdf$omittedLevels)) { # generally these are 99999/999997 etc
      if (lvlsAllNumeric && dataIsNumeric) {
        data[[vari]] <- as.numeric(data[[vari]])
        lvls <- as.numeric(lvls)
        # fixes any rounding issues comparing source data to the key value (e.g., value of 999998.99999999 need to be compared to key value of 999999)
        # if all of the defined levels are 'omitted', then generally this data is continuous so the omittedLevels need to be set to NA to return the expected (non-omitted) data properly
        data[round(data[[vari]], 8) %in% lvls, vari] <- NA
      } else {
        data[data[[vari]] %in% lvls, vari] <- NA
      }
    } else if (!is.null(labelDF$labelled) && !labelDF$labelled[labelDF$variableName == vari]) {
      # fileFormat has missing values and labelled columns
      data[data[[vari]] %in% lvls, vari] <- NA
    } else { # this is a truly labeled file
      if (length(unique(lbls)) != length(lbls)) {
        tab <- table(lbls)
        dnames <- names(tab[tab > 1])
        needNewLabels <- lbls %in% dnames
        if (any(needNewLabels)) {
          warning(paste0("Updating labels on ", sQuote(vari), " because there are multiples of the label ", sQuote(dnames), "."))
        }
        lbls[needNewLabels] <- paste(lbls[needNewLabels], 1:(sum(needNewLabels)), sep = ":")
      }

      # this is legacy for PISA, would like to remove this if possible!
      isPISA_IDVar <- esdf$survey == "PISA" && grepl("id", vari, ignore.case = TRUE)

      # determine if the field keys are numeric, but the value labels are saved as character (e.g., 001, 010)
      # convert those to numeric values if the data is also numeric type
      if (lvlsAllNumeric && dataIsNumeric && !isPISA_IDVar) {
        lvls <- as.numeric(lvls)
        data[[vari]] <- as.numeric(data[[vari]]) # ensures data matches the levels as both numeric
      }

      if (includeNaLabel) {
        if (sum(is.na(data[[vari]])) > 0) {
          lvls <- c(NA, lvls) # add NA first to the list
          lbls <- c("(Missing)", lbls)
        }
      }
      # some id variables has missing values or special case labels (i.e. bookid)
      if (isPISA_IDVar) {
        exception <- unique(data[[vari]][!data[[vari]] %in% lvls])
        lvls <- c(lvls, exception)
        lbls <- c(lbls, exception)
      }

      # test if there are defined numeric values not in the label
      if (esdf$validateFactorLabels && !isPISA_IDVar) {
        exception <- sort(unique(data[[vari]][!data[[vari]] %in% lvls]))
        exception <- exception[!is.na(exception) & trimws(exception, which = "both") != ""]
        if (length(exception) > 0) {
          lvls <- c(lvls, exception)
          lbls <- c(lbls, exception)
        }
      }

      replaceRows <- data[[vari]] == "" | is.na(data[[vari]])
      data[replaceRows, vari] <- NA

      # put next block into function
      isTaylorVal <- vari %in% getAllTaylorVars(esdf)
      data[ , vari] <- getFactorValue(lvls = lvls, lbls = lbls, dataVals = data[[vari]], factorOnly = (isTaylorVal || isPISA_IDVar), includeNaLabel = includeNaLabel)
    } # end else if(esdf$survey != "PIAAC" && all(lblVals %in% omittedLevels))
  } # end for(i in seq_along(labels)

  return(data)
}

getFactorValue <- function(lvls, lbls, dataVals, factorOnly = FALSE, includeNaLabel = FALSE) {
  # short circuit if we only want to make a factor and not check for making an lfactor
  if (factorOnly) {
    if (anyNA(dataVals) && includeNaLabel) {
      return(factor(dataVals, levels = lvls, labels = lbls, exclude = NULL)) # don't exclude NA from the factor
    } else {
      return(factor(dataVals, levels = lvls, labels = lbls)) # NA is the default 'exclude' param and will be skipped in the factor
    }
  }

  lvlsAllNumeric <- isAllNumeric(lvls)
  valsAllNumeric <- isAllNumeric(dataVals)
  lbls <- as.character(lbls)

  if (!lvlsAllNumeric && !valsAllNumeric) {
    if (anyNA(dataVals) && includeNaLabel) {
      return(factor(dataVals, levels = lvls, labels = lbls, exclude = NULL))
    } else {
      return(factor(dataVals, levels = lvls, labels = lbls)) # NA values excluded by default
    }
  } else {
    if (anyNA(dataVals) && includeNaLabel) {
      # to match existing functionality!!, if building an lfactor do not have NA - (Missing) retained for lfactors
      keepIdx <- which(!is.na(lvls))

      res <- tryCatch(
        {
          lfactor(dataVals, levels = lvls[keepIdx], labels = lbls[keepIdx], exclude = NULL)
        },
        error = function(e) {
          if (grepl("must either be identical or the labels must not be numbers", e)) {
            factor(dataVals, levels = lvls[keepIdx], labels = lbls[keepIdx], exclude = NULL)
          } else {
            stop(e)
          }
        }
      )
      return(res)
    } else {
      res <- tryCatch(
        {
          lfactor(dataVals, levels = lvls, labels = lbls)
        },
        error = function(e) {
          if (grepl("must either be identical or the labels must not be numbers", e)) {
            factor(dataVals, levels = lvls, labels = lbls)
          } else {
            stop(e)
          }
        }
      )
      return(res)
    }
  }

  # SHOULD NOT GET HERE, but return the dataVals for safety
  return(dataVals)
}

# this function applies a decimal conversion step, where it will multiply a large integer by a multiplier to get a decimal value
applyDecimalConversion <- function(dataDF, labelsDF, varNameColumn = "variableName", decimalColName = "Decimal") {
  for (i in seq_len(ncol(dataDF))) {
    varn <- colnames(dataDF)[i] # the variable of the column
    idx <- which(grepl(paste0("^", varn, "$"), labelsDF[[varNameColumn]], ignore.case = TRUE))
    decLen <- labelsDF[idx, decimalColName, drop = TRUE]

    if (is.null(decLen) || length(decLen) == 0) { # no match found, this is a 'cached' variable, skip it
      next
    }

    decLen[is.na(decLen)] <- 0 # use a 0 if NA is specified for decimal length (character value)
    decLen <- as.numeric(decLen)
    if (decLen > 0) {
      mult <- (10^decLen)
      xCol <- dataDF[ , varn] / mult
      # if it has a decimal conversion and is in the requested data
      dataDF[ , varn] <- xCol
    }
  }

  return(dataDF)
}

# return TRUE/FALSE value based on if all of the vals are numeric (regardless of current type)
# will return TRUE if all supplied values are numeric or NA
# will return FALSE if there are any items that cannot be coerced to a numeric
isAllNumeric <- function(vals) {
  if (is.character(vals)) {
    vals[trimws(vals) == ""] <- NA
  }

  naOrig <- sum(is.na(vals)) # original count of na values
  valsX <- suppressWarnings(as.numeric(vals)) # coerce to numeric
  naX <- sum(is.na(valsX)) # count how many NA values after coercion
  return(naOrig == naX) # compare before/after NA counts
}
