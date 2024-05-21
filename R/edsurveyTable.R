#' @title EdSurvey Tables With Conditional Means
#'
#' @description Returns a summary table (as a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}})
#' that shows the number of students, the percentage of students, and the mean
#' value of the outcome (or left-hand side) variable by the
#' predictor (or right-hand side) variable(s).
#'
#' @param formula object of class \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}},
#'                potentially with
#'                a subject scale or subscale
#'                on the left-hand side and
#'                variables to tabulate
#'                on the right-hand side.
#'                When the left-hand side of the
#'                formula is omitted and \code{returnMeans} is \code{TRUE},
#'                then the default subject scale or subscale is used.
#'                You can find the default composite scale and all subscales
#'                using the function \code{\link{showPlausibleValues}}.
#'                Note that the order of the right-hand side variables affects the output.
#' @param data object of class \code{edsurvey.data.frame}. See \code{\link{readNAEP}}
#'             for how to generate an \code{edsurvey.data.frame}.
#' @param weightVar character string indicating the weight variable to use.
#'                   Note that only the name of the
#'                   weight variable needs to be included here, and any
#'                   replicate weights will be automatically included.
#'                   When this argument is \code{NULL}, the function uses the default.
#'                   Use \code{\link{showWeights}} to find the default.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}}
#'                   term (see the Details section of
#'                 \code{\link{lm.sdf}} to see the definition of \eqn{V_{jrr}}) can be estimated with any number of plausible values, and values larger than the number of
#'                   plausible values on the survey (including \code{Inf}) will result in all of the plausible values being used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param pctAggregationLevel the percentage variable sums up to 100 for the first
#'                              \code{pctAggregationLevel} columns.
#'                              So, when set to \code{0}, the \code{PCT} column adds up to 1
#'                              across the entire sample.
#'                              When set to \code{1}, the \code{PCT} column adds up to 1
#'                              within each level of the first variable on the
#'                              right-hand side of the formula; when set to \code{2},
#'                              then the percentage
#'                              adds up to 100 within the interaction of the
#'                              first and second variable, and so on.
#'                              Default is \code{NULL}, which will result in the
#'                              lowest feasible aggregation level.
#'                              See Examples section.
#' @param returnMeans a logical value; set to \code{TRUE} (the default) to get the \code{MEAN} and
#'                     \code{SE(MEAN)} columns in the returned table described in the Value section.
#' @param returnSepct set to \code{TRUE} (the default) to get the \code{SEPCT} column in the returned table described in the Value section.
#' @param varMethod  a character set to \code{jackknife} or \code{Taylor} that indicates the variance estimation method
#'                   to be used.
#' @param drop a logical value. When set to the default value of \code{FALSE}, when a single column is returned, it is still represented as a \code{data.frame} and is
#'             not converted to a vector.
#' @param dropOmittedLevels a logical value. When set to the default value of \code{TRUE}, drops those levels of all factor variables that are specified
#'                        in an \code{edsurvey.data.frame}. Use \code{print} on an \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default conditions stored in an \code{edsurvey.data.frame}
#'                           to subset the data. Use \code{print} on an \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode} \code{=} \code{list(var1} \code{=} \code{list(from} \code{=} \code{c("a", "b", "c"),} \code{to} \code{=} \code{"c"))}.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allows for
#'                           the computation
#'                           of covariances between estimates.
#' @param omittedLevels this argument is deprecated. Use \code{dropOmittedLevels}.
#'
#' @details This method can be used to generate a simple one-way, two-way, or
#' \emph{n}-way
#' table with unweighted and weighted \emph{n} values and percentages. It also
#' can calculate the average of the subject scale or subscale for students at
#' each level of the cross-tabulation table.
#'
#' A detailed description of all statistics is given in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#'
#' @return A table with the following columns:
#'    \item{RHS levels}{one column for each right-hand side variable. Each row
#'                      regards students who are at the levels shown in that row.}
#'    \item{N}{count of the number of students in the survey in the \code{RHS levels}}
#'    \item{WTD_N}{the weighted \emph{N} count of students in the survey in \code{RHS levels}}
#'    \item{PCT}{the percentage of students at the aggregation level specified by \code{pctAggregationLevel} (see Arguments).
#'                      See the vignette titled
#'         \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#'             in the section
#' \dQuote{Estimation of Weighted Percentages} and its first subsection
#' \dQuote{Estimation of Weighted Percentages When Plausible Values Are Not Present.}}
#'    \item{SE(PCT)}{the standard  error of the percentage, accounting
#'                          for the survey sampling methodology. When \code{varMethod}
#'                          is the \code{jackknife}, the calculation of this column is
#'                          described in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#'  in the section
#' \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Jackknife Method.}
#'                       When \code{varMethod} is set to \code{Taylor}, the calculation of this column is described in
#' \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Taylor Series Method.}
#' }
#'    \item{MEAN}{the mean assessment score for units in the \code{RHS levels}, calculated according to the  vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#' in the section
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Present.}}
#'    \item{SE(MEAN)}{the standard error of the \code{MEAN} column (the mean assessment score for units in the \code{RHS levels}), calculated according to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#' in the sections
#' \dQuote{Estimation of Standard Errors of Weighted Means When Plausible Values Are Present, Using the Jackknife Method}
#' or
#' \dQuote{Estimation of Standard Errors of Weighted Means When Plausible Values Are Present, Using the Taylor Series Method,}
#' depending on the value of \code{varMethod}.}
#'
#'  When \code{returnVarEstInputs} is \code{TRUE}, two additional elements are
#'  returned. These are \code{meanVarEstInputs} and \code{pctVarEstInputs} and
#'  regard the \code{MEAN} and \code{PCT} columns, respectively. These two
#'  objects can be used for calculating covariances with
#'  \code{\link{varEstToCov}}.
#'
#' @references
#' Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys. \emph{International Statistical Review}, \emph{51}(3), 279--292.
#'
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
#'
#' @example man/examples/edsurveyTable.R
#' @author Paul Bailey and Ahmad Emad
#'
#' @importFrom stats terms ftable aggregate ave
#' @importFrom data.table setDT copy
#' @export
edsurveyTable <- function(formula,
                          data,
                          weightVar = NULL,
                          jrrIMax = 1,
                          pctAggregationLevel = NULL,
                          returnMeans = TRUE,
                          returnSepct = TRUE,
                          varMethod = c("jackknife", "Taylor"),
                          drop = FALSE,
                          dropOmittedLevels = TRUE,
                          defaultConditions = TRUE,
                          recode = NULL,
                          returnVarEstInputs = FALSE,
                          omittedLevels = deprecated()) {
  # Test class of incoming data
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  varMethod <- substr(tolower(varMethod[[1]]), 0, 1)
  if (!varMethod %in% c("j", "t")) {
    stop(paste0("The argument ", sQuote("varMethod"), " must be one of ", dQuote("Taylor"), " or ", dQuote("jackknife"), "."))
  }
  if (varMethod == "t" & returnVarEstInputs) {
    stop(paste0("The argument ", sQuote("returnVarEstInputs"), " must be ", dQuote("FALSE"), " when varMethod is ", dQuote("Taylor"), "."))
  }

  if (lifecycle::is_present(omittedLevels)) {
    lifecycle::deprecate_soft("4.0.0", "edsurveyTable(omittedLevels)", "edsurveyTable(dropOmittedLevels)")
    dropOmittedLevels <- omittedLevels
  }
  if (!is.logical(dropOmittedLevels)) stop("The ", sQuote("dropOmittedLevels"), " argument must be logical.")


  if (inherits(data, "edsurvey.data.frame.list")) {
    # res2 is a temporary variable that holds the list of results
    res2 <- list()
    ll <- length(data$datalist)

    labels <- as.character(c(1:ll))
    warns <- c()
    errs <- c()
    for (i in 1:ll) {
      sdf <- data$datalist[[i]]
      # withCallingHandlers logs warnings in the code, and the calcEdSurveyTable call continues (invokeRestart)
      # errors are dealt with by stopping execution and logging the error in the tryCatch
      # if there is an error the variable "result" will be the updated error message.
      # otherwise it is an edsurveyTable.
      withCallingHandlers(
        result <- tryCatch(
          calcEdsurveyTable(formula, sdf, weightVar, jrrIMax, pctAggregationLevel,
            returnMeans, returnSepct, varMethod, drop,
            dropOmittedLevels,
            defaultConditions = defaultConditions, recode,
            defaultConditionsMissing = missing(defaultConditions),
            returnVarEstInputs = returnVarEstInputs
          ),
          error = function(cond) {
            # in tryCatch, stop execution, return updated errs character
            # could also <<- errs
            return(c(errs, dQuote(paste(data$covs[i, ], collapse = " "))))
          }
        ),
        warning = function(cond) {
          # this happens when a warning comes up and executes in calcEdsurveyTable
          # adds this dataset to the list with warnings, then continues execution, ignoring additional warnings
          warns <<- c(warns, dQuote(paste(data$covs[i, ], collapse = " ")))
          invokeRestart("muffleWarning")
        }
      )
      if (inherits(result, "character")) {
        errs <- result
      }
      if (inherits(result, "edsurveyTable")) {
        res2[[labels[i]]] <- result
      }
    } # end of for(i in 1:ll)
    if (length(errs) > 0) {
      if (length(errs) > 1) {
        datasets <- "datasets"
      } else {
        datasets <- "dataset"
      }
      warning(paste0("Could not process ", datasets, " ", pasteItems(errs), ". Try running this call with just the affected ", datasets, " for more details."))
    }
    if (length(warns) > 0) {
      if (length(warns) > 1) {
        datasets <- "datasets"
      } else {
        datasets <- "dataset"
      }
      warning(paste0("Warnings from ", datasets, " ", pasteItems(warns), ". Try running this call with just the affected ", datasets, " for more details."))
    }
    cmbRes <- NULL
    listi <- 0
    while (is.null(cmbRes)) {
      listi <- listi + 1
      cmbRes <- res2[[labels[listi]]]
      if (listi > ll) {
        stop("No valid output.")
      }
    }
    # get the data from cmbRes
    cmbD <- cmbRes$data
    # grab the column names
    cnames <- colnames(cmbD)

    for (i in 1:ncol(data$covs)) {
      # copy over this column from covs
      cmbD[ , colnames(data$covs)[i]] <- rep(data$covs[listi, i], nrow(cmbD))
      cnames <- c(colnames(data$covs)[i], cnames)
    }
    # reorder columns so covs come first
    cmbDi <- cmbD0 <- cmbD <- cmbD[ , cnames]
    # we need these for NULL data
    rhs_vars <- all.vars(formula[[3]])
    for (listi in seq_along(labels)) {
      if (!is.null(res2[[labels[listi]]])) {
        cmbDi <- (res2[[labels[listi]]])$data
        for (i in 1:ncol(data$covs)) {
          cmbDi[ , colnames(data$covs)[i]] <- rep(data$covs[listi, i], nrow(cmbDi))
        }
        if (listi == 1) {
          cmbD <- cmbDi[ , cnames]
        } else {
          cmbD <- rbind(cmbD, cmbDi[ , cnames])
        }
      } else {
        # res is NULL
        # set everything to NA
        for (i in 1:ncol(cmbDi)) {
          # leave in RHS variables
          if (!colnames(cmbDi)[i] %in% rhs_vars) {
            cmbDi[ , i] <- NA
          }
        }
        # add covs data back
        for (i in 1:ncol(data$covs)) {
          cmbDi[ , colnames(data$covs)[i]] <- rep(data$covs[listi, i], nrow(cmbDi))
        }
        if (listi == 1) {
          cmbD <- cmbDi[ , cnames]
        } else {
          cmbD <- rbind(cmbD, cmbDi[ , cnames])
        }
      }
    }
    # add column labels back
    for (i in 1:ncol(cmbD)) {
      # note, this works even if attributes(cmbD0[ ,i])$label is NULL
      attr(cmbD[ , i], "label") <- attributes(cmbD0[ , i])$label
    }
    cmbRes$data <- cmbD
    cmbRes$n0 <- NA
    cmbRes$nUsed <- NA
    class(cmbRes) <- "edsurveyTableList"
    return(cmbRes)
  } # closes if(inherits(data, "edsurvey.data.frame.list"))
  else {
    result <- calcEdsurveyTable(formula, data, weightVar, jrrIMax, pctAggregationLevel,
      returnMeans, returnSepct, varMethod, drop,
      dropOmittedLevels,
      defaultConditions, recode,
      defaultConditionsMissing = missing(defaultConditions),
      returnVarEstInputs = returnVarEstInputs
    )
    return(result)
  }
}

calcEdsurveyTable <- function(formula,
                              data,
                              weightVar = NULL,
                              jrrIMax = 1,
                              pctAggregationLevel = NULL,
                              returnMeans = TRUE,
                              returnSepct = TRUE,
                              varMethod = c("jackknife", "Taylor", "j", "t"),
                              drop = FALSE,
                              omittedLevels = TRUE,
                              defaultConditions = TRUE,
                              recode = NULL,
                              defaultConditionsMissing = TRUE,
                              returnVarEstInputs = FALSE,
                              dropUnusedLevels = TRUE) {
  ## outline: ###################
  ## 1) check and format inputs
  ## 2) get the data
  ## 3) build the output table
  ###############################
  # in section 3, the table is built column by column


  ## 1) check and format inputs
  # test class of incoming data
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  wgt <- checkWeightVar(data, weightVar)

  # use just the first character, j or t
  varMethod <- substr(tolower(varMethod[[1]]), 0, 1)
  if (!varMethod %in% c("j", "t")) {
    stop(paste0("The argument ", sQuote("varMethod"), " must be one of ", dQuote("Taylor"), " or ", dQuote("jackknife"), "."))
  }

  psuVar <- getPSUVar(data, weightVar = wgt)
  stratumVar <- getStratumVar(data, weightVar = wgt)
  tv <- checkTaylorVars(psuVar, stratumVar, wgt, varMethod)
  psuVar <- tv$psuVar
  stratumVar <- tv$stratumVar
  varMethod <- tv$varMethod

  # fill in default subject scale / sub scale
  zeroLengthLHS <- attr(terms(formula), "response") == 0

  # get y variables
  if (zeroLengthLHS) {
    yvar <- attributes(getAttributes(data, "pvvars"))$default
    if (is.null(yvar)) { # in TALIS, there is no PV
      yvar <- colnames(data)[1]
      returnMeans <- FALSE
    }
    formula <- formula(paste0(yvar, " ", paste(as.character(formula), collapse = "")))
  } else {
    yvar <- all.vars(formula[[2]])
  }
  rhs_vars <- all.vars(formula[[3]])

  # check yvar and rhs_vars
  # rhs_vars must be  discrete variables
  if (!all(typeOfVariable(rhs_vars, data) %in% "discrete")) {
    nd <- which(!typeOfVariable(rhs_vars, data) %in% "discrete")
    warning(paste0("Variables on the right-hand side of the formula must be discrete. Nondiscrete variables will be removed from the formula. Non-discrete variables: ", pasteItems(rhs_vars[nd])))
    rhs_vars <- rhs_vars[typeOfVariable(yvar, data) %in% "discrete"]
  }

  # define default aggregation level
  if (is.null(pctAggregationLevel)) {
    # set this to aggregate as far down as is reasonable
    pctAggregationLevel <- length(rhs_vars) - 1
  }
  # if it is not possible, reset it.
  if (pctAggregationLevel > length(rhs_vars) - 1) {
    pctAggregationLevel <- length(rhs_vars) - 1
    warning(paste0("Resetting ", sQuote("pctAggregationLevel"), " to ", pctAggregationLevel, ", the largest potentially meaningful value."))
  }

  if (length(yvar) > 1 & returnMeans) {
    stop("There must be exactly one left-hand side variable in the ", sQuote("formula"), " argument.")
  }

  # get plausible values
  pvy <- hasPlausibleValue(yvar, data)

  # when we use plausible values there are multiple y variables. When we do not there is only one
  # this code sets it up correctly
  yvars <- yvar
  if (pvy) {
    yvars <- getPlausibleValue(yvar, data)
  } else {
    if (!yvar %in% colnames(data)) {
      stop(paste0("Cannot find ", sQuote(yvars), " in the edsurvey.data.frame."))
    }
  }

  if (returnMeans) {
    yvar0 <- yvars[1]
  }

  ## 2) get the data
  # We need to get all the weights
  wgtl <- getAttributes(data, "weights")[[wgt]]
  # build the replicate weight vector
  wgtall <- paste0(wgtl$jkbase, wgtl$jksuffixes)
  if (!returnMeans) {
    reqvar <- c(all.vars(formula[[3]]), wgt)
  } else {
    reqvar <- c(all.vars(formula), wgt)
  }
  if (returnMeans) {
    reqvar <- c(reqvar, wgtall)
  }
  # when including omittedLevels, include NA label
  includeNaLabel <- !omittedLevels
  # add Taylor variables, when needed
  if (varMethod == "t") {
    reqvar <- c(reqvar, psuVar, stratumVar)
  }
  # only call with defaultConditions if it was in the call to edsurveyTable
  if (defaultConditionsMissing) {
    suppressWarnings(edf <- getData(data, reqvar,
      includeNaLabel = includeNaLabel,
      returnJKreplicates = (varMethod == "j" & (returnMeans | returnSepct)),
      dropUnusedLevels = dropUnusedLevels,
      drop = drop,
      dropOmittedLevels = omittedLevels,
      recode = recode,
      addAttributes = TRUE
    ))
  } else {
    suppressWarnings(edf <- getData(data, reqvar,
      includeNaLabel = includeNaLabel,
      returnJKreplicates = (varMethod == "j" & (returnMeans | returnSepct)),
      dropUnusedLevels = dropUnusedLevels,
      drop = drop,
      dropOmittedLevels = omittedLevels,
      defaultConditions = defaultConditions,
      recode = recode,
      addAttributes = TRUE
    ))
  }
  # remove rows with NA weights
  if (any(is.na(edf[ , wgt]))) {
    warning(paste0("Removing ", sum(is.na(edf[ , wgt])), " rows with NA weights from analysis."))
    edf <- edf[!is.na(edf[ , wgt]), ]
  }
  # drop rows with 0 weights
  if ((returnMeans | returnSepct) & any(edf[ , wgt] <= 0)) {
    warning(paste0("Removing ", sum(!(edf[ , wgt] > 0)), " rows with 0 weight from analysis."))
    edf <- edf[edf[ , wgt] > 0, ]
  }
  # drop rows with missing outcomes, but only if showing means
  if (returnMeans && any(is.na(edf[ , yvar0]))) {
    warning(paste0("Removing ", sum(is.na(edf[ , yvar0])), " rows with missing scale score from analysis."))
    edf <- edf[!is.na(edf[ , yvar0]), ]
  }
  # keep only valid data
  for (var in rhs_vars) {
    edf <- edf[!is.na(edf[var]), ]
  }

  if (nrow(edf) == 0) {
    stop("The requested data has 0 rows, this analysis cannot be done.")
  }

  njk <- length(wgtl$jksuffixes)
  npv <- length(yvars)
  # Exception: if there is no RHS variable, just give summary statistics
  if (length(rhs_vars) == 0) {
    returnSepct <- FALSE
    returnMeans <- TRUE
    pctVarEstInputsJK <- NULL
    res <- data.frame(
      "N" = nrow(edf),
      "WTD_N" = sumna(edf[ , wgt]),
      "PCT" = 100.00
    )
    if (nrow(edf) > 0) {
      # fit an lm to get a mean and SE estimate and add those to the res
      # also keep track of var est inputs
      lst <- list(
        formula = formula,
        data = edf,
        weightVar = wgt,
        jrrIMax = jrrIMax,
        varMethod = varMethod,
        returnVarEstInputs = returnVarEstInputs
      )
      lmi <- tryCatch(do.call(lm.sdf, lst),
        error = function(cond) {
          message(paste0("Encountered an issue when calculating a row, ", dQuote(cond$message), " Some mean estimates will be NA in the table."))
          return(NULL)
        }
      )
      if (!is.null(lmi)) {
        cf <- summary(lmi)$coefmat
        if (!is.na(cf$coef)) {
          res[["MEAN"]] <- cf$coef
          res[["SE(MEAN)"]] <- cf$se
          if (returnVarEstInputs) {
            meanVarEstInputsJK <- lmi$varEstInputs$JK
            meanVarEstInputsJK$variable <- "label"
            meanVarEstInputsPV <- lmi$varEstInputs$PV
            meanVarEstInputsPV$variable <- "label"
          }
        } # end if (!is.na(cf$coef))
        else {
          lmi <- NULL
        }
      } # if (!is.null(lmi))
      if (is.null(lmi)) {
        res[["MEAN"]] <- NA
        res[["SE(MEAN)"]] <- NA
        if (returnVarEstInputs) {
          meanVarEstInputsJK <- data.frame(PV = NA, JKreplicate = NA, variable = "label", value = NA)
          meanVarEstInputsPV <- data.frame(PV = NA, variable = "label", value = NA)
        }
      }
    } else { # for when nrow(edf) = 0
      res[["MEAN"]] <- NA
      res[["SE(MEAN)"]] <- NA
      if (returnVarEstInputs) {
        meanVarEstInputsJK <- data.frame(PV = 1, JKreplicate = 1:njk, variable = "label", value = NA)
        meanVarEstInputsPV <- data.frame(PV = 1:npv, variable = "label", value = NA)
      }
    } # end if (nrow(edf) > 0)
  } # if (length(rhs_vars) == 0)
  else {
    ## 3) build the output
    # this makes the n sizes and RHS variables in a table
    n <- ftable(edf[ , rhs_vars, drop = FALSE])
    res <- data.frame(n)

    # if length(rhs_vars) is 1, then the names do not get assigned correctly. Fix that.
    names(res) <- c(rhs_vars, "N") # does nothing unless length(rhs_vars)==1
    # fastAgg is similar to aggregate, but fast.
    # add the weighted Ns
    wtdn <- fastAgg(formula(paste0(wgt, " ~ ", paste(rhs_vars, collapse = " + "))), data = edf, FUN = sumna)

    # rename the last column to WTD_N
    last_column <- names(wtdn)[length(names(wtdn))]
    wtdn$WTD_N <- wtdn[ , last_column]
    wtdn[ , last_column] <- NULL
    # add the column WTD_N to the result table by merging it on
    res <- merge(res, wtdn, by = rhs_vars, sort = FALSE, all.x = TRUE)
    res$WTD_N[res$N %in% 0] <- 0
    if (pctAggregationLevel == 0) {
      # percent aggregation is over all units
      res$twt <- sum(res$WTD_N, na.rm = TRUE)
      res$group <- 1 # used in Taylor series sePct
    } else {
      # percent aggregation is over subsets of units
      twt <- fastAgg(formula(paste0("WTD_N", " ~ ", paste(rhs_vars[1:pctAggregationLevel], collapse = " + "))), data = res, FUN = sumna)
      names(twt) <- c(rhs_vars[1:pctAggregationLevel], "twt")
      twt$group <- 1:nrow(twt) # used in Taylor series sePct
      res <- merge(res, twt, by = rhs_vars[1:pctAggregationLevel], all.x = TRUE)
    }
    # calculate the percent
    res["PCT"] <- res[ , "WTD_N"] / res[ , "twt"] * 100
    res[["PCT"]][is.nan(res[["PCT"]])] <- NA
    # make containers for these variables
    pctVarEstInputs <- NULL
    pctVarEstInputsJK <- NULL

    res_no0 <- res[res$N > 0, ]
    # add the column "SE(PCT)", the SE on WTD_N. Only when requested
    if (returnSepct) {
      wtdnvar <- rep(0, nrow(res_no0))
      wgtl <- getAttributes(data, "weights")[[wgt]]
      if (varMethod == "j") {
        # see statistics vignette for the formulas used here
        for (jki in seq_along(wgtl$jksuffixes)) {
          # recalculate the percent with every JK replicate weight
          wgti <- paste0(wgtl$jkbase, wgtl$jksuffixes[jki])
          wtdn <- fastAgg(formula(paste0(wgti, " ~ ", paste(rhs_vars, collapse = " + "))), data = edf, FUN = sumna)
          if (pctAggregationLevel == 0) {
            wtdt <- aggregate(formula(paste0(wgti, " ~ 1")), data = wtdn, FUN = sum, na.rm = TRUE)
          } else {
            wtdt <- fastAgg(formula(paste0(wgti, " ~ ", paste(rhs_vars[1:pctAggregationLevel], collapse = " + "))), data = wtdn, FUN = sumna)
          }
          names(wtdt)[ncol(wtdt)] <- "twti"
          # last_column is the name of the column with the wgti sum in it
          last_column <- names(wtdn)[length(names(wtdn))]
          wtdn <- merge(wtdn, res_no0, by = rhs_vars)
          if (pctAggregationLevel == 0) {
            wtdn$one__ <- 1
            wtdt$one__ <- 1
            wtdn <- merge(wtdn, wtdt, by = "one__")
          } else {
            wtdn <- merge(wtdn, wtdt, by = rhs_vars[1:pctAggregationLevel])
          }
          # store the JK replicate results
          if (returnVarEstInputs) {
            if (jki == 1) {
              wtdn_ <- wtdn
              for (ii in seq_along(rhs_vars)) {
                wtdn_[ , rhs_vars[ii]] <- as.character(wtdn[ , rhs_vars[ii]])
              }
              level_ <- c()
              for (i in 1:nrow(wtdn)) {
                level_ <- c(level_, paste(rhs_vars, wtdn_[i, rhs_vars], sep = "=", collapse = ":"))
              }
            }
            for (i in 1:nrow(wtdn)) {
              level <- paste(rhs_vars, wtdn_[i, rhs_vars], sep = "=", collapse = ":")
              pctVarEstInputsJKi <- data.frame(
                stringsAsFactors = FALSE,
                PV = 0,
                JKreplicate = jki,
                variable = level_[i],
                value = (wtdn[i, last_column] / wtdn[i, "twti"] - wtdn[i, "PCT"] / 100
                )
              )
              pctVarEstInputsJK <- rbind(pctVarEstInputsJK, pctVarEstInputsJKi)
            }
          }
          # sum up to get the vector of variances between the full sample weight results and the JK results
          wtdnvar <- wtdnvar + (wtdn[last_column] / wtdn["twti"] - wtdn["PCT"] / 100)^2
        }
        # finally, add the
        wtdndf <- data.frame(
          stringsAsFactors = FALSE,
          100 * sqrt(getAttributes(data, "jkSumMultiplier") * wtdnvar)
        )
        names(wtdndf)[1] <- "SE(PCT)"
        wtdndf[ , rhs_vars] <- wtdn[ , rhs_vars]
        res <- merge(res, wtdndf, by = rhs_vars, sort = FALSE, all.x = TRUE)
      } else { # Taylor series based method
        if (is.null(getPSUVar(data, weightVar = wgt)) & is.null(getStratumVar(data, weightVar = wgt))) {
          stop("For Taylor series the PSU and stratum variables must be defined.")
        }

        res_no0[ , "SE(PCT)"] <- NA
        # for every group (row of the table)
        sapply(unique(res_no0$group), function(z) {
          # get the Taylor series SE for this row of the table
          res_no0i <- res_no0[res_no0$group == z, ] # subset(res_no0, group == z)
          n <- nrow(res_no0i)
          res_no0i$groupsubset <- 1:n
          if (n != 1) { # if n>1 then the percent is not 100 and we need to find SE(PCT)
            pr <- res_no0[res_no0$group == z, "PCT"] / 100
            datai <- edf
            if (pctAggregationLevel > 0) {
              for (i in 1:pctAggregationLevel) {
                datai$rhsi <- datai[ , rhs_vars[i]]
                vvv <- as.character(res_no0i[1, rhs_vars[i]])
                datai <- datai[datai$rhsi == vvv, ]
              }
            }
            datai$weight__n__ <- datai[ , wgt]
            # identify unit for each obs
            for (i in 1:n) {
              datai$gss <- 1
              # set gss to 1 for just rows in this group
              for (j in (pctAggregationLevel + 1):length(rhs_vars)) {
                vvv <- as.character(res_no0i[i, rhs_vars[j]])
                datai$gss[datai[ , rhs_vars[j]] != vvv] <- 0
              }
              if (sum(datai$gss) >= 1) { # allow for units with no obs that have that
                datai$unit[datai$gss %in% 1] <- i
              }
            }
            # make W where i,jth entry is weight of unit i iff it is in j
            # make \tilde{W} where i,jth entry is weight of unit i
            wtilde <- w <- matrix(0, ncol = n, nrow = nrow(datai))
            units <- sort(unique(datai$unit))
            for (j in seq_along(units)) { # this could be 1:n but this way it is robust to levels with 0 units in them
              ss <- datai$unit %in% units[j]
              w[ss, j] <- datai[ss, wgt]
            }
            for (j in 1:ncol(wtilde)) {
              wtilde[ , j] <- datai[ , wgt] * pr[j]
            }
            # again using notation from AM documentation, including multiplicaiton by w
            # in TeX, u_{hij} * w, is called uhijw here
            uhijw <- w - wtilde # in uhijw[i,j] j= percent value, i=obs
            colnames(uhijw) <- paste0("v", 1:n)
            # use these as a convenience
            sumna <- function(x) {
              sum(x, na.rm = TRUE)
            }
            meanna <- function(x) {
              mean(x, na.rm = TRUE)
            }
            uhijw <- data.frame(uhijw,
              unit = datai$unit,
              stratV = datai[ , getStratumVar(data, weightVar = wgt)],
              psuV = datai[ , getPSUVar(data, weightVar = wgt)]
            )
            for (vi in 1:n) {
              # build uhijw, see AM documentation
              uhijw$v <- uhijw[ , paste0("v", vi)]
              uhiw <- aggregate(v ~ psuV + stratV, data = uhijw, FUN = sum)
              uhiw$vv <- ave(uhiw$v, uhiw$stratV, FUN = meanna)
              uhiw$dx <- uhiw$v - uhiw$vv
              nam <- names(uhiw)
              nam[nam == "v"] <- paste0("uhi", vi)
              nam[nam == "vv"] <- paste0("uj", vi)
              nam[nam == "dx"] <- paste0("dx", vi)
              names(uhiw) <- nam
              if (vi == 1) {
                uhiw_ <- uhiw
              } else {
                uhiw_ <- merge(uhiw_, uhiw, by = c("stratV", "psuV"))
              }
            }
            repu <- unique(uhiw_$stratV)
            S <- matrix(0, nrow = nrow(res_no0i), ncol = nrow(res_no0i))
            for (repi in seq_along(repu)) {
              # see AM documentaiton
              dataii <- uhiw_[uhiw_$stratV == repu[repi], ]
              jku <- unique(dataii$psuV)
              ni <- length(jku)
              if (ni > 1) {
                for (jkj in 1:ni) {
                  vec <- unlist(dataii[dataii$psuV == jku[jkj], paste0("dx", 1:n), drop = TRUE])
                  S <- S + (ni / (ni - 1)) * vec %*% t(vec)
                }
              }
            }
            # from AM documentaiton
            D <- diag(rep(1 / res_no0i$twt[1], nrow(res_no0i)))
            var <- D %*% S %*% t(D)
            res_no0[res_no0$group == z, "SE(PCT)"] <<- 100 * sqrt(diag(var)) # fit to percentage
          } else { # end if(n!=1) {
            # there is only one thing in this aggregation level
            # so the percent will be 100. The frequentist SE on this will be zero.
            res_no0[res_no0$group == z, "SE(PCT)"] <<- 0
          } # end else for if(n!=1) {
        }) # end sapply(unique(res_no0$group), function(z) {
        res <- merge(res, res_no0[ , c(rhs_vars, "SE(PCT)")], by = rhs_vars, sort = FALSE, all.x = TRUE)
      } # end else for if(varMethod == "j") {
      # these methods can produce NaN, return NA in those cases
      res[["SE(PCT)"]][is.nan(res[["SE(PCT)"]])] <- NA
    } # end if(returnSepct) {

    # delete intermediates from results
    res["group"] <- NULL
    res["twt"] <- NULL
    # order correctly
    for (i in length(rhs_vars):1) {
      res <- res[order(res[ , rhs_vars[i]]), ]
    }

    # res <- res[res$N>0,] #subset(res, N > 0)
    # for returnVarEstInputs
    meanVarEstInputs <- NULL
    meanVarEstInputsJK <- NULL
    meanVarEstInputsPV <- NULL


    # add mean PV score to res
    if (returnMeans) {
      # for each row of the table, get the mean and SE from lm
      # by subsetting the data to just the units relevant to that row
      # and then using lm.sdf to find the mean and SE for that row
      for (i in 1:nrow(res)) {
        dsdf <- edf
        # for this row, subset it on each dimenstion
        for (j in seq_along(rhs_vars)) {
          if (returnVarEstInputs) {
            if (j == 1) {
              label <- paste0(rhs_vars[j], "=", res[i, rhs_vars[j]])
            } else {
              label <- paste0(label, ":", rhs_vars[j], "=", res[i, rhs_vars[j]])
            }
          }
          cond <- parse(text = paste0(rhs_vars[j], " == \"", res[i, rhs_vars[j]], "\""))[[1]]
          if (inherits(dsdf, "edsurvey.data.frame")) {
            dsdf <- subset.edsurvey.data.frame(dsdf, cond, inside = TRUE) # subset to just those values at level i of the first X variable
          } else {
            dsdf <- dsdf[dsdf[ , rhs_vars[j]] %in% res[i, rhs_vars[j]], ] # subset to just those values at level i of the first X variable
          }
        } # ends for(j in seq_along(rhs_vars))
        if (nrow(dsdf) > 0) {
          # fit an lm to get a mean and SE estimate and add those to the res
          # also keep track of var est inputs
          fi <- formula(paste0(yvar, " ~ 1"))
          lst <- list(fi, dsdf,
            weightVar = wgt, jrrIMax = jrrIMax,
            varMethod = varMethod,
            dropOmittedLevels = FALSE, # taken care of above
            returnVarEstInputs = returnVarEstInputs
          )
          warningsList <- NULL
          suppressWarnings(
            lmi <- withCallingHandlers(
              tryCatch(do.call(lm.sdf, lst),
                error = function(cond) {
                  message(paste0("Encountered an issue when calculating a row, ", dQuote(cond$message), " Some mean estimates will be NA in the table."))
                  return(NULL)
                }
              ),
              warning = function(cond) {
                if (grepl("certainties", cond$message)) {
                  warningsList <<- c(warningsList, "When performing Taylor series variance estimation some rows have strata that had only one populated PSU. Units in these strata were assumed to be certainties. You can condense the strata/PSU structure to avoid this.")
                } else {
                  warningsList <<- c(warningsList, cond$message)
                }
              }
            )
          )
          if (!is.null(lmi)) {
            cf <- summary(lmi)$coefmat
            if (!is.na(cf$coef)) {
              res[i, "MEAN"] <- cf$coef
              res[i, "SE(MEAN)"] <- cf$se
              if (returnVarEstInputs) {
                meanVarEstInputsJKi <- lmi$varEstInputs$JK
                meanVarEstInputsJKi$variable <- label
                meanVarEstInputsJK <- rbind(meanVarEstInputsJK, meanVarEstInputsJKi)
                meanVarEstInputsPVi <- lmi$varEstInputs$PV
                meanVarEstInputsPVi$variable <- label
                meanVarEstInputsPV <- rbind(meanVarEstInputsPV, meanVarEstInputsPVi)
              }
            } # end if (!is.na(cf$coef))
            else {
              lmi <- NULL
            }
          }
          if (is.null(lmi)) {
            res[i, "MEAN"] <- NA
            res[i, "SE(MEAN)"] <- NA
            if (returnVarEstInputs) {
              meanVarEstInputsJKi <- data.frame(PV = NA, JKreplicate = NA, variable = "label", value = NA)
              meanVarEstInputsJK <- rbind(meanVarEstInputsJK, meanVarEstInputsJKi)
              meanVarEstInputsPVi <- data.frame(PV = NA, variable = "label", value = NA)
              meanVarEstInputsPV <- rbind(meanVarEstInputsPV, meanVarEstInputsPVi)
            }
          }
        } else { # for when nrow(dsdf) = 1
          res[i, "MEAN"] <- NA
          res[i, "SE(MEAN)"] <- NA
          if (returnVarEstInputs) {
            meanVarEstInputsJKi <- data.frame(PV = 1, JKreplicate = 1:njk, variable = "label", value = NA)
            meanVarEstInputsJKi$variable <- label
            meanVarEstInputsJK <- rbind(meanVarEstInputsJK, meanVarEstInputsJKi)
            meanVarEstInputsPVi <- data.frame(PV = 1:npv, variable = "label", value = NA)
            meanVarEstInputsPVi$variable <- label
            meanVarEstInputsPV <- rbind(meanVarEstInputsPV, meanVarEstInputsPVi)
          }
        } # end if (nrow(dsdf) > 0)
      } # end for(i in 1:nrow(res)) {
      if (length(unique(warningsList)) > 0) {
        warning(unique(warningsList))
      }
    } # if(returnMeans)
  } # end else if (length(rhs_vars) == 0)

  # clean up and produce return output
  if (varMethod == "t") {
    njk <- NA
  }
  rownames(res) <- NULL
  varmeth <- ifelse(varMethod == "t", "Taylor series", "jackknife")

  # order the output by the "by" variables
  vnames <- intersect(names(res), all.vars(formula))

  # Add variable labels as an attribute
  if (inherits(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) {
    for (i in seq_along(names(res))) {
      if (names(res)[i] %in% all.vars(formula)) {
        # not every variable on a light.edsurvey.data.frame will return here.
        # some will return a NULL
        suppressWarnings(searchRes <- searchSDF(names(res)[i], data))
        if (!is.null(searchRes)) {
          attr(res[[names(res)[i]]], "label") <- searchRes$Labels
        }
        vnames <- c(vnames, names(res)[i])
      }
    }
  }

  res2 <- list(
    formula = formula, npv = length(yvars),
    jrrIMax = min(jrrIMax, length(yvars)), weight = wgt,
    njk = njk, varMethod = varmeth, data = res
  )
  if (returnVarEstInputs) {
    meanVarEstInputs <- list(
      JK = meanVarEstInputsJK,
      PV = meanVarEstInputsPV
    )

    pctVarEstInputs <- list(
      JK = pctVarEstInputsJK,
      PV = NULL
    )
    res2 <- c(res2, list(
      meanVarEstInputs = meanVarEstInputs,
      pctVarEstInputs = pctVarEstInputs
    ))
  }
  res2 <- c(res2, list(n0 = nrow2.edsurvey.data.frame(data), nUsed = nrow(edf)))
  class(res2) <- "edsurveyTable"
  return(res2)
}

fastAgg <- function(formula, data, FUN) {
  y <- all.vars(formula[[2]])
  x <- all.vars(formula[[3]])
  # get the function name properly
  fun <- substitute(FUN)
  # DataTable is used here for faster evaluation, this is entirely based on performance testing
  # a lot of what makes it faster is that we do not sort the results at the end.
  pp <- paste0("as.data.frame(setDT(copy(data))[ ,list(", y, "=", fun, "(", y, ")),by=list(", paste(x, collapse = ","), ")])")
  eval(parse(text = pp))
}

#' @method print edsurveyTableList
# @author Paul Bailey and Howard Huo
# @aliases print.edsurveyTable
#' @export
print.edsurveyTableList <- function(x, digits = getOption("digits"), use_es_round=getOption("EdSurvey_round_output"), ...) {
  print.edsurveyTable(x, digits = digits, use_es_round = use_es_round, ...)
}

#' @method print edsurveyTable
# @author Paul Bailey and Howard Huo
#' @export
print.edsurveyTable <- function(x, digits = getOption("digits"), use_es_round=getOption("EdSurvey_round_output"), ...) {
  if(use_es_round) {
    x <- es_round(x)
  }
  cat(paste0("\nFormula: ", paste(deparse(x$formula), "\n", collapse = ""), "\n"))

  if (x$npv > 1) {
    # only print if it could be larger than one.
    cat(paste0("Plausible values: ", x$npv, "\n"))
    if (tolower(x$varMethod) %in% "jackknife") {
      cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
    }
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ", x$varMethod, "\n"))
  if (!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  if (!is.na(x$n0)) {
    cat(paste0("full data n: ", x$n0, "\n"))
    cat(paste0("n used: ", x$nUsed, "\n\n"))
  }
  cat("\n")
  cat(paste0("Summary Table:\n"))
  # find out if it wraps,
  co <- capture.output(print(x$data, digits = digits, row.names = TRUE, ...))
  # it it wraps, that will make the length of co more than the number
  # of rows in data plus a header row, so include row names to help the user
  print(x$data, digits = digits, row.names = length(co) > (nrow(x$data) + 1), ...)
}

sumna <- function(x) {
  sum(x, na.rm = TRUE)
}

typeOfVariable <- function(var, data) {
  if (inherits(data, "light.edsurvey.data.frame")) {
    return(sapply(var, function(v) {
      if (hasPlausibleValue(v, data)) {
        return("continuous")
      }
      if (!tolower(v) %in% tolower(colnames(data))) {
        return(NA)
      }
      if (is.numeric(data[[v]])) {
        return("continuous")
      } else {
        return("discrete")
      }
    }))
  }

  fileFormat <- do.call("rbind", lapply(
    data$dataList,
    function(dl) {
      dl$fileFormat
    }
  ))
  sapply(var, function(v) {
    if (hasPlausibleValue(v, data)) {
      return("continuous")
    }
    if (!tolower(v) %in% tolower(colnames(data))) {
      return(NA)
    }
    # get the cache, if it exists
    cache <- getAttributes(data = data, "cache", errorCheck = FALSE)
    if (tolower(v) %in% tolower(colnames(cache))) {
      if (is.numeric(cache[[v]])) {
        return("continuous")
      } else {
        return("discrete")
      }
    }
    if (fileFormat$dataType[tolower(fileFormat$variableName) == tolower(v)] == "numeric") {
      return("continuous")
    } else {
      return("discrete")
    }
  })
}
