#' @title Achievement Levels
#'
#' @description Returns achievement levels using weights and variance estimates appropriate for the \code{edsurvey.data.frame}.
#'
#' @param achievementVars character vector indicating variables to be included in the achievement 
#'                        levels table, potentially with a subject scale or subscale. When the subject 
#'                        scale or subscale is omitted, the default subject scale or subscale is 
#'                        used. You can find the default composite scale and all subscales using the 
#'                        function \code{\link{showPlausibleValues}}.
#' @param aggregateBy character vector specifying variables by which to aggregate achievement levels. The percentage
#'                    column sums up to 100 for all levels of all variables specified here. When set to the 
#'                    default of \code{NULL}, the percentage column sums up to 100 for all 
#'                    levels of all variables specified in \code{achievementVars}.
#' @param data      an \code{edsurvey.data.frame}
#' @param weightVar  character string indicating the weight variable to use.
#'                   Only the name of the
#'                   weight variable needs to be included here, and any
#'                   replicate weights will be automatically included.
#'                   When this argument is \code{NULL}, the function uses the default.
#'                   Use \code{\link{showWeights}} to find the default.
#' @param cutpoints numeric vector indicating cutpoints. Set to standard NAEP cutpoints for 
#'                  Basic, Proficient, and Advanced by default.
#' @param jrrIMax    a numeric value. When using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}} 
#'                   term (see \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}} for the definition of \eqn{V_{jrr}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param omittedLevels  a logical value. When set to the default value (\code{TRUE}), 
#'                       it drops those levels in all factor variables that are specified in \code{achievementVars} 
#'                       and \code{aggregateBy}. Use \code{print} on an \code{edsurvey.data.frame} 
#'                       to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default 
#'                          conditions stored in an \code{edsurvey.data.frame} to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame} to see the default
#'                          conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'               \code{recode} \code{=} \code{list(var1=} \code{list(from=c("a",} \code{"b",} \code{"c"),} \code{to ="d"))}. See Examples.
#' @param returnDiscrete logical indicating if discrete achievement levels should be returned. Defaults 
#'                       to \code{TRUE}.
#' @param returnCumulative logical indicating if cumulative achievement levels should be returned. Defaults
#'                         to \code{FALSE}. The first and last categories are the same as defined for discrete levels.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of 
#'                          primary sampling units (PSUs)
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allows for the computation
#'                           of covariances between estimates.
#' @author Huade Huo, Ahmad Emad, and Trang Nguyen
#' @details The \code{achievementLevels} function applies appropriate weights
#'          and the variance estimation method for each
#'          \code{edsurvey.data.frame}, with several arguments for customizing
#'          the aggregation and output of the analysis 
#'          results. Namely, by using these optional arguments, users can choose
#'          to generate the percentage of students 
#'          performing at each achievement level (discrete), generate the
#'          percentage of students performing at or above each achievement level
#'          (cumulative), 
#'          calculate the percentage distribution of students by achievement
#'          level (discrete or cumulative) and 
#'          selected characteristics (specified in \code{aggregateBy}), and
#'          compute the percentage distribution of students 
#'          by selected characteristics within a specific achievement level.
#'
#' \subsection{Calculation of percentages}{
#'          The details of the methods are shown in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey} in 
#'          \dQuote{Estimation of Weighted Percentages When Plausible Values Are Present} and are used to calculate 
#'          all cumulative and discrete probabilities.
#'
#'          When the requested achievement levels are discrete (\code{returnDiscrete = TRUE}),
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \code{aggregateBy}) 
#'          whose scores lie in the range  \eqn{[cutPoints_i, cutPoints_{i+1}), i = 0,1,...,n}.
#'          \code{cutPoints} is the score thresholds provided by the user with \eqn{cutPoints_0} taken
#'          to be 0. \code{cutPoints} are set to NAEP standard cutpoints for achievement levels by default.
#'          To aggregate by a specific variable, for example, \code{dsex}, specify \code{dsex} in \code{aggregateBy}
#'          and all other variables in \code{achievementVars}. To aggregate by subscale, specify 
#'          the name of the subscale (e.g., \code{num_oper}) in \code{aggregateBy} and all other variables in 
#'          \code{achievementVars}.
#'          
#'          When the requested achievement levels are cumulative (\code{returnCumulative = TRUE}),
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \code{aggregateBy}) 
#'          whose scores lie in the range  [\eqn{cutPoints_i}, \eqn{\infty}), \eqn{i = 1, 2, ..., n-1}. The 
#'          first and last categories are the same as defined for discrete levels.
#' } 
#'         
#' \subsection{Calculation of standard error of percentages}{
#'          The method used to calculate the standard error of the percentages is described in the vignette titled
#'          \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey} 
#'          in the sections \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Present, Using the Jackknife Method} 
#'          and \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Taylor Series Method.}
#'          For \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Present, Using the Jackknife Method,}
#'         the value of \code{jrrIMax} sets the value of \eqn{m^*}.
#' }
#'          
#' @return
#' A \code{list} containing up to two data frames, one discrete achievement levels (when \code{returnDiscrete} is \code{TRUE})
#' and one for cumulative achievement levels (when \code{returnCumulative} is \code{TRUE}). The \code{data.frame} contains the following columns:
#' \item{Level}{one row for each level of the specified achievement cutpoints}
#' \item{Variables in achievementVars}{one column for each variable in \code{achievementVars} 
#' and one row for each level of each variable in \code{achievementVars}}
#' \item{Percent}{the percentage of students at or above each achievement level aggregated as specified by \code{aggregateBy}}
#' \item{StandardError}{the standard error of the percentage, accounting for the survey sampling methodology. 
#'                             See the vignette titled \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey}.}
#' \item{N}{the number of observations in the incoming data (the
#'                  number of rows when \code{omittedLevels} and
#'                  \code{defaultConditions} are set to \code{FALSE})}
#' \item{wtdN}{the weighted number of observations in the data}
#' \item{nPSU}{the number of PSUs at or above each achievement level aggregated as specified by \code{aggregateBy}. Only returned with \code{returnNumberOfPSU=TRUE}.}
#' @references 
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
#'
#' @importFrom data.table setDT melt data.table ':=' rbindlist .N .SD as.data.table setkeyv
#' @example man/examples/achievementLevels.R
#' @export
achievementLevels <- function(achievementVars = NULL, 
                              aggregateBy = NULL,
                              data,
                              cutpoints = NULL,
                              returnDiscrete = TRUE,
                              returnCumulative = FALSE,
                              weightVar=NULL,
                              jrrIMax=1,
                              omittedLevels=TRUE,
                              defaultConditions=TRUE,
                              recode=NULL,
                              returnNumberOfPSU=FALSE,
                              returnVarEstInputs=FALSE) {
  
  call <- match.call()
  # parse this allowing c("a", "b"), or just a or "a".
  achievementVars <- iparse(substitute(achievementVars), x=data)
  aggregateBy <- iparse(substitute(aggregateBy), x=data)

  achievementVars <- if (is.null(achievementVars)) NULL else tolower(achievementVars)
  aggregateBy <- if (is.null(aggregateBy)) NULL else tolower(aggregateBy)

  alResultDfList <- list()
  assertArgument(data)
  
  if(inherits(data, "edsurvey.data.frame.list")) {
    ll <- length(data$datalist)
    labels <- apply(data$covs, 1, function(x) { paste(as.character(x), collapse=":") })
    if(is.null(labels)) {
      labels <- as.character(c(1:ll))
    }
    
    warns <- c()
    for(i in 1:ll) {
      sdf <- data$datalist[[i]]
      temp <- tryCatch(suppressWarnings(alResult <- calAL(achievementVars, aggregateBy, sdf, cutpoints, returnDiscrete,
                                         returnCumulative, weightVar, jrrIMax, omittedLevels, defaultConditions,
                                         recode,
                                         defaultConditionsMissing=missing(defaultConditions),
                                         returnVarEstInputs=returnVarEstInputs,
                                         returnNumberOfPSU=returnNumberOfPSU)),
                       error = function(cond) {
                         warns <<- c(warns, labels[i])
                         return(NULL)
                       }
      )
      if(inherits(temp, "achievementLevels")) {
        alResultDfList[[labels[i]]] <- alResult
      }
    }
    if(length(warns)>0) {
      if(length(warns)>1) {
        datasets <- "datasets"
      } else {
        datasets <- "dataset"
      }
      warning(paste0("Could not process ", datasets, " ", pasteItems(warns), ". Try running this call with just the affected ", datasets, " for more details."))
    }
    class(alResultDfList) <- "achievementLevels"
    alResultDfList
  } 
  else {
    alResult <- calAL(achievementVars, aggregateBy, data, cutpoints, returnDiscrete,
                      returnCumulative, weightVar, jrrIMax, omittedLevels, defaultConditions,
                      recode, defaultConditionsMissing=missing(defaultConditions),
                      returnVarEstInputs=returnVarEstInputs, returnNumberOfPSU=returnNumberOfPSU)
    alResult
  }
  
}


calAL <- function(achievementVars = NULL, 
                  aggregateBy = NULL,
                  data,
                  cutpoints = NULL,
                  returnDiscrete = TRUE,
                  returnCumulative = FALSE,
                  weightVar = NULL,
                  jrrIMax = 1,
                  omittedLevels = TRUE,
                  defaultConditions = TRUE,
                  recode = NULL,
                  defaultConditionsMissing = FALSE,
                  returnVarEstInputs = FALSE,
                  returnNumberOfPSU = returnNumberOfPSU) {
  assertArgument(data)
  als <- getAttributes(data, "achievementLevels")
  assertArgument(als)

  # Determine if the user supplied variables for calculating achievementlevels, 
  # otherwise just use the default plausible value
  if(is.null(achievementVars)) {
    achievementVars <- attributes(getAttributes(data, "pvvars"))$default
  }
  
  # Check to see only one variable in all the supplied variables has plausible values
  vars <- unique(c(achievementVars, aggregateBy))
  n.pvs <- sum(sapply(vars, FUN= function(x) hasPlausibleValue(x, data)))
  
  # Determine if weight supplied, otherwise use default weight
  if (is.null(weightVar)) {
    wgt <- attributes(getAttributes(data, "weights"))$default 
  } else {
    wgt <- weightVar
  }
  assertArgument(wgt, data)
  
  # Get yvar, if no yvar is specified 
  has.pv <- sum(sapply(vars, FUN = function(x) hasPlausibleValue(x, data)))
  
  if(has.pv == 0) {
    achievementVars <- attributes(getAttributes(data, "pvvars"))$default
    vars <- c(vars, achievementVars)
  }
  yvar <- as.list(vars[sapply(vars, FUN= function(x) hasPlausibleValue(x, data))])
  assertArgument(yvar)
  achievementVarsNoPV <- vars[sapply(vars, FUN= function(x) !hasPlausibleValue(x, data))]
  aggregateByNoPV <- unlist(sapply(aggregateBy, function(x) {
    if (hasPlausibleValue(x, data)) { return("Level") }
    return(x)
  }))
  
  # with yvar in hand check for linking error
  linkingError <- "NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking$", c(yvar)))

  if (length(aggregateByNoPV) == 0) {
    aggregateByNoPV <- NULL
  }  
  
  if(linkingError) {
    pvs <- list(getPlausibleValue(yvar, data))
    if(jrrIMax != 1) {
      warning("The linking error variance estimator only supports ", dQuote("jrrIMax=1"), ". Resetting to 1.")
      jrrIMax <- 1
    }
  } else {
    # non-linking error case
    pvs <- lapply(yvar, function(x) { getPlausibleValue(x, data)})
  }
  jrrIMax <- min(jrrIMax, sapply(pvs, length))
  
  getDataVarNames <- c(vars, wgt)
  if (returnNumberOfPSU){
      # Get stratum and PSU variable
      stratumVar <- getAttributes(data, "stratumVar")
      psuVar <- getAttributes(data, "psuVar")
      if (all(c(stratumVar, psuVar) %in% names(data)) | all(c(stratumVar, psuVar) %in% colnames(data))) {
        getDataVarNames <- c(vars, wgt, stratumVar, psuVar)
    } else {
      warning(paste0("Stratum and PSU variables are required for this call and are not on the incoming data. Ignoring ", dQuote("returnNumberOfPSU=TRUE"),"."))
      returnNumberOfPSU <- FALSE
    }
  }

  # get the data. This is most of the arguments
  getDataArgs <- list(data = data,
                      varnames = getDataVarNames,
                      returnJKreplicates = TRUE,
                      drop = FALSE,
                      omittedLevels = omittedLevels,
                      recode = recode,
                      includeNaLabel = TRUE,
                      dropUnusedLevels = TRUE)
  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!defaultConditionsMissing) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  # avoid 0 rows warning
  suppressWarnings(edfDT <- do.call(getData, getDataArgs))
  edfDT <- setDT(edfDT)
  # Remove zero-weight cases
  edfDTnrow <- nrow(edfDT)
  for(i in seq_along(pvs)) {
    # only need to filter by first PV
    vi <- pvs[[i]][1]
    edfDT <- edfDT[!is.na(get(vi)), ]
  }
  # update nrow
  edfDTnrow <- nrow(edfDT)
  edfDT <- edfDT[eval(parse(text = paste0(wgt, " > 0", sep = " "))), ]
  if (nrow(edfDT) < edfDTnrow) {
    warning("Removing ", edfDTnrow - nrow(edfDT), " rows with nonpositive weight from analysis.")
    edfDTnrow <- nrow(edfDT)
  }

  stratumAndPSU <- NULL
  if (returnNumberOfPSU) {
    # Combine stratumVar and psuVar
    edfDT <- edfDT[, stratumAndPSU:=paste0(.SD, collapse = "-"), 
                   .SDcols=c(stratumVar, psuVar), 
                   by = 1:edfDTnrow][, c(stratumVar, psuVar):=NULL]
  }
  assertArgument(edfDT)
  
  if(length(vars[!vars %in% unlist(yvar)]) == 0) {
    vars <- c(yvar, "1")
  }
  
  # Determine if the user supplied cutpoints.
  # If the user did not supply cutpoints, get the default ones stored in sdf
  if(is.null(cutpoints)) {
    alsCutpoints <- lapply(yvar, function(x) {
      ALattr <- getAttributes(data, "pvvars")[[x]]$achievementLevel
      temp <- as.numeric(ALattr)
      names(temp) <- names(ALattr)
      return(temp)
    })
  } else {
    if(length(yvar) == 1) {
      if(!is.list(cutpoints)) {
        cutpoints <- list(cutpoints)
      }
    }
    cutpoints <- lapply(cutpoints, function(x) {
      xn <- names(x)
      x <- as.numeric(x)
      names(x) <- xn
      return(x)
      })
    if(any(is.na(unlist(cutpoints)))) {
      stop("Cut points must be numeric and non-missing.")
    }
    if(length(cutpoints) != length(yvar)) {
      stop("If you specify the cut points, you must specify them for every plausible value.")
    }
    alsCutpoints <- cutpoints
  }
  
  alsCutpoints <- lapply(alsCutpoints, function(x) {
    if(is.null(names(x))) {
      names(x) <- paste0("Level ", seq(1, length(x), 1))
    }
    return(x)
  })
  names(alsCutpoints) <- unlist(yvar)
  
  jkWeights <- getWeightJkReplicates(wgt, data)

  # Gather information about the call.
  npv <- min(sapply(pvs, length))
  callVars <- list(achievementVars=achievementVars,
                   aggregateBy=aggregateBy,
                   cutpoints=alsCutpoints,
                   weightVar=wgt,
                   jrrIMax=jrrIMax,
                   npv=npv,
                   varMethod="jackknife",
                   njk=length(jkWeights))
  # Show levels of achievement Vars
  levelsOfEdfDT <- sapply(edfDT, levels)
  levelsOfEdfDT[sapply(levelsOfEdfDT, is.null)] <- NULL
  levelsOfEdfDTGrid <- expand.grid(levelsOfEdfDT)
  levelsOfEdfDTGrid <- levelsOfEdfDTGrid[ , intersect(unique(c(aggregateByNoPV,colnames(levelsOfEdfDTGrid))), colnames(levelsOfEdfDTGrid)), drop = FALSE] 
  # Calculate discrete achievement levels
  pvs <- lapply(pvs, function(x) {
    return(x[1:npv])
  })
  discreteDT <- edfDT
  for(i in 1:length(pvs)) {
    discreteDT <- recodeEdf(discreteDT, pvs[[i]], alsCutpoints[[i]], returnCumulative = FALSE)
  }
  discreteOrder <- lapply(alsCutpoints, function(x) {
    sortALResults(x, returnCumulative = FALSE)
  })

  calculateALStatF <- function(achievementVars, data, aggregateBy, cum=FALSE) {
    f <- function(pv, w, short=TRUE) {
      # calculate overall sum of weights
      # eval requested by data.table, unique because some variables will be in both achievementVars and aggregateBy
      aggregateBy2 <- aggregateBy
      for(i in seq_along(aggregateBy2 == "Level")) {
        aggregateBy2[aggregateBy2 == "Level"][i] <- pv[names(aggregateBy2[aggregateBy2 == "Level"])[i] == names(pv)]
      }
      byVars <- unique(c(pv, achievementVars, aggregateBy2))
      d1 <- data[,list(N=.N, wtdN = sum(get(w))), by = byVars]
      # make sure every level is populated and ordered for output
      allLevels <- do.call(expand.grid, lapply(byVars, function(x) { levels(data[,get(x)]) } ))
      colnames(allLevels) <- byVars
      setkeyv(d1, byVars)
      d1 <- d1[allLevels,]
      # this generates NAs, set them to 0
      d1[is.na(d1)] <- 0
      if(cum) {
        # now cumsum by non-aggregateBy
        nonAgg <- byVars[!byVars %in% pv[1]]
        d1 <- d1[, wtdN2 := cumsum2(.SD$wtdN), by = nonAgg]
        d1 <- d1[, N := cumsum2(.SD$N), by = nonAgg]
        # aggregate by aggregateBy
        if(pv[1] %in% aggregateBy2) {
          d1 <- d1[, Denom := sum(.SD$wtdN2), by = aggregateBy2]
        } else {
          d1 <- d1[, Denom := sum(.SD$wtdN), by = aggregateBy2]
        }
        d1 <- d1[, DenomGroup := .GRP, by = aggregateBy2]
        d1 <- d1[, Percent := wtdN2 * 100 / Denom, by = aggregateBy2]
        # rename levels to cumulative level names
        x <- d1[,get(pv[1])]
        lvls <- levels(x)
        lvls[-length(lvls)] <- gsub("^At", "At or Above", lvls[-length(lvls)])
        x <- factor(x, levels(x), lvls)
        d1 <- d1[,(pv[1]) := x]
        d1 <- d1[,"Denom" := NULL]
        if(pv[1] %in% aggregateBy2) {
          d1 <- d1[,"wtdN" := NULL]
          colnames(d1)[colnames(d1) == "wtdN2"] <- "wtdN"
        } else {
          d1 <- d1[,"wtdN3" := wtdN]
          d1 <- d1[,"wtdN" := NULL]
          colnames(d1)[colnames(d1) == "wtdN2"] <- "wtdN"
        }
      } else {
        # aggregate according to aggregateBy
        d1 <- d1[, Percent:=wtdN * 100 / sum(.SD$wtdN), by = aggregateBy2]
        d1 <- d1[, DenomGroup := .GRP, by = aggregateBy2]
      }
      d1 <- as.data.frame(d1)
      if(short) {
        return(d1$Percent)
      }
      return(d1)
    }
    return(f)
  }

  stat_al <- calculateALStatF(achievementVars=achievementVarsNoPV,
                              data=discreteDT,
                              aggregateBy=aggregateByNoPV,
                              cum=FALSE)
  stat_alC <- calculateALStatF(achievementVars=achievementVarsNoPV,
                               data=discreteDT,
                               aggregateBy=aggregateByNoPV,
                               cum=TRUE)
  # estimator and also calculates necessary statistics for imputation variance estimator
  estAL <- lapply(pvs, function(x) { paste0(x, "_lvl") })
  names(estAL) <- unlist(yvar)
  names(pvs) <- unlist(yvar)

  if(linkingError) {
    estAL0 <- estAL[[1]]
    impAL <- grepl("_imp_", estAL0)
    sampAL <- grepl("_samp_", estAL0)
    # estimation yvars
    estAL <- list(estAL0[!(impAL | sampAL)])
    names(estAL) <- unlist(yvar)
    # imputation yvars
    impAL <- list(estAL0[impAL])
    names(impAL) <- unlist(yvar)
    # sampling yvars
    sampAL <- list(estAL0[sampAL])
    names(sampAL) <- unlist(yvar)
  }
  if(returnDiscrete) {
    if(linkingError) {
      discreteEst <- getEstAL(pvEst = estAL,
                              stat = stat_al,
                              wgt = wgt)
    } else {
      discreteEst <- getEstAL(pvEst = estAL,
                              stat = stat_al,
                              wgt = wgt)
      # setup containers for sampling variance estimates
      dvarm <- matrix(NA, nrow=jrrIMax, ncol=nrow(discreteEst$est))
    }
    # setup containers for variance estimates
    discVarEstInputs <- list()
    discVarEstInputs[["JK"]] <- data.frame()
    discVarEstInputs[["PV"]] <- data.frame()
    cn <- colnames(discreteEst$est)
    cn <- cn[!cn %in% c("N", "wtdN", "Percent")]
    d0 <- cbind(row=paste0("Row", 1:nrow(discreteEst$est)), discreteEst$est[ , cn, drop=FALSE])
    #renames lit_level to variable1...
    d0 <- renameLevelsToVariables(d0, estAL)
  }
  if(returnCumulative) {
    if(linkingError) {
      cumEst <- getEstAL(pvEst = estAL,
                         stat = stat_alC,
                         wgt = wgt)
    } else {
      cumEst <- getEstAL(pvEst = estAL,
                         stat = stat_alC,
                         wgt = wgt)
      cvarm <- matrix(NA, nrow=jrrIMax, ncol=nrow(cumEst$est))
    }
    # setup containers for variance estimates
    cumVarEstInputs <- list()
    cumVarEstInputs[["JK"]] <- data.frame()
    cumVarEstInputs[["PV"]] <- data.frame()
    cn <- colnames(cumEst$est)
    cn <- cn[!cn %in% c("N", "wtdN", "Percent")]
    c0 <- cbind(row=paste0("Row", 1:nrow(cumEst$est)), cumEst$est[ , cn, drop=FALSE])
    c0 <- renameLevelsToVariables(c0, estAL)
  }
  jkSumMult <- getAttributes(data, "jkSumMultiplier")
  
  for(pvi in 1:jrrIMax) { # for each PV (up to jrrIMax)
    if(returnDiscrete) {
      if(linkingError) {
        T0 <- discreteEst$est$Percent
        names(T0) <- paste0("Row",1:length(T0))
        dimpVar <- getLinkingImpVar(data=NULL,
                                    pvImp = impAL[[1]],
                                    ramCols = ncol(getRAM()),
                                    stat = stat_al,
                                    wgt = wgt,
                                    T0 = T0,
                                    T0Centered = FALSE)
        dsampVar <- getLinkingSampVar(data=NULL,
                                      pvSamp= sampAL[[1]],
                                      stat = stat_al,
                                      rwgt = jkWeights,
                                      T0 = T0,
                                      T0Centered = FALSE)
        # jrrIMax always 1, no need to rbind
        discVarEstInputs[["JK"]] <- fixALVarEstInputRows(dsampVar$veiJK, d0)
        discVarEstInputs[["PV"]] <- fixALVarEstInputRows(dimpVar$veiImp, d0)
      } else {
        thispvs <- unlist(lapply(estAL, function(x) { x[pvi] }))
        res <- getVarEstJK(stat = stat_al,
                           yvar = thispvs,
                           wgtM = jkWeights,
                           co0 = discreteEst$est$Percent - discreteEst$coef[pvi, ],
                           jkSumMult = jkSumMult,
                           pvName = pvi)
        discVarEstInputs[["JK"]] <- rbind(discVarEstInputs[["JK"]], fixALVarEstInputRows(res$veiJK, d0))
        dvarm[pvi, ] <- res$VsampInp
      }
    }
    if(returnCumulative) {
      if(linkingError) {
        T0 <- cumEst$est$Percent
        names(T0) <- paste0("Row",1:length(T0))
        cimpVar <- getLinkingImpVar(data=NULL,
                                    pvImp = impAL[[1]],
                                    ramCols = ncol(getRAM()),
                                    stat = stat_alC,
                                    wgt = wgt,
                                    T0 = T0,
                                    T0Centered = FALSE)
        csampVar <- getLinkingSampVar(data=NULL,
                                      pvSamp= sampAL[[1]],
                                      stat = stat_alC,
                                      rwgt = jkWeights,
                                      T0 = T0,
                                      T0Centered = FALSE)
        # jrrIMax always 1, no need to rbind
        cumVarEstInputs[["JK"]] <- fixALVarEstInputRows(csampVar$veiJK, c0)
        cumVarEstInputs[["PV"]] <- fixALVarEstInputRows(cimpVar$veiImp, c0)
      } else { 
        thispvs <- unlist(lapply(estAL, function(x) { x[pvi] }))
        res <- getVarEstJK(stat = stat_alC,
                           yvar = thispvs,
                           wgtM = jkWeights,
                           co0 = cumEst$est$Percent - cumEst$coef[pvi, ],
                           jkSumMult = jkSumMult,
                           pvName = pvi)
        jkv <- merge(c0, res$veiJK, by.y="variable", by.x="row", all.x=TRUE, all.y=FALSE)
        jkv$variable <- NULL
        colnames(jkv)[colnames(jkv) %in% "row"] <- "variable"
        cumVarEstInputs[["JK"]] <- rbind(cumVarEstInputs[["JK"]], fixALVarEstInputRows(jkv, c0))
        cvarm[pvi, ] <- res$VsampInp
      }
    }
  }
  
  # Gather and order returnVarEstInputs results
  if(returnDiscrete) {
    if(linkingError) {
      varI <- dimpVar$V
      varS <- dsampVar$V
      discreteEst$est$StandardError <- sqrt(varI + varS)
    } else {
      varI <- apply(dvarm, 2, mean)
      M <- nrow(discreteEst$coef)
      varS <- (M+1)/M * apply(discreteEst$coef, 2, var )
      discreteEst$est$StandardError <- sqrt(varI + varS)
      # add varEstInputs for PV
      cn <- colnames(discreteEst$est)
      cn <- cn[!cn %in% c("N", "wtdN", "Percent", "StandardError")]
      pv <- t(discreteEst$coef)
      npv <- ncol(pv)
      colnames(pv) <- paste0("v", 1:npv)
      pv <- cbind(discreteEst$est[ , cn , drop=FALSE], pv)
      # make varEstInputs
      if(returnVarEstInputs) {
        for(i in 1:npv) {
          newdf <- pv[ , c(cn, paste0("v", i))]
          colnames(newdf)[colnames(newdf) == paste0("v", i)] <- "value"
          newdf$PV <- i
          if(any(grepl("Level$", colnames(newdf)))) {
            for(i in seq_along(names(estAL))) {
              if(i == 1) {
                newdf$variable <- newdf[ , paste0(names(estAL)[i], "_Level")]
              } else {
                newdf$variable <- paste0(newdf$variable, ":", newdf[ , paste0(names(estAL)[i], "_Level")])
              }
              newdf[ , paste0(names(estAL)[i], "_Level")] <- NULL
            }
          }
          v1 <- c("PV","variable", "value")
          newdf <- newdf[ , c("PV", setdiff(colnames(newdf), v1) ,"variable", "value")]
          discVarEstInputs[["PV"]] <- rbind(discVarEstInputs[["PV"]], newdf)
        }
      }
    }
  }
  if(returnCumulative) {
    if(linkingError) {
      varI <- cimpVar$V
      varS <- csampVar$V
      cumEst$est$StandardError <- sqrt(varI + varS)
    } else {
      varI <- apply(cvarm, 2, mean)
      M <- nrow(cumEst$coef)
      varS <- (M+1)/M * apply(cumEst$coef, 2, var )
      cumEst$est$StandardError <- sqrt(varI + varS)
      # add varEstInputs for PV
      cn <- colnames(cumEst$est)
      cn <- cn[!cn %in% c("N", "wtdN", "Percent", "StandardError")]
      pv <- t(cumEst$coef)
      npv <- ncol(pv)
      colnames(pv) <- paste0("v", 1:npv)
      pv <- cbind(cumEst$est[ , cn , drop=FALSE], pv)
      if(returnVarEstInputs) {
        for(i in 1:npv) {
          newdf <- pv[ , c(cn, paste0("v", i))]
          colnames(newdf)[colnames(newdf) == paste0("v", i)] <- "value"
          newdf$PV <- i
          if(any(grepl("Level$", colnames(newdf)))) {
            for(i in seq_along(names(estAL))) {
              if(i == 1) {
                newdf$variable <- newdf[ , paste0(names(estAL)[i], "_Level")]
              } else {
                newdf$variable <- paste0(newdf$variable, ":", newdf[ , paste0(names(estAL)[i], "_Level")])
              }
              newdf[ , paste0(names(estAL)[i], "_Level")] <- NULL
            }
          }
          v1 <- c("PV", "variable", "value")
          newdf <- newdf[ , c("PV", setdiff(colnames(newdf), v1) ,"variable", "value")]
          cumVarEstInputs[["PV"]] <- rbind(cumVarEstInputs[["PV"]], newdf)
        }
      }
    }
  }
  # Create a list containing the achievement levels and the call information
  res <- list(callVars=callVars)
  if(returnDiscrete) {
    de <- discreteEst$est
    # a zero standard error should be missing
    de$StandardError[de$StandardError <= .Machine$double.eps] <- NA
    res <- c(res, list(discrete=de))
  }
  if(returnCumulative) {
    ce <- cumEst$est
    # a zero standard error should be missing
    ce$StandardError[ce$StandardError <= .Machine$double.eps] <- NA
    res <- c(res, list(cumulative=ce))
  }

  if(returnVarEstInputs) {
    if(returnDiscrete) {
      #assign to 0 if values are close to 0 for correct DOF correction
      discVarEstInputs$JK$value[which(abs(discVarEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(max(discVarEstInputs$JK$JKreplicate))))] <- 0 
      discVarEstInputs$PV <- unfactor(discVarEstInputs$PV)
      rownames(discVarEstInputs$PV) <- NULL
      discVarEstInputs$JK <- unfactor(discVarEstInputs$JK)
      rownames(discVarEstInputs$JK) <- NULL
      discVarEstInputs <- col2VarEstInputs(discVarEstInputs)
      res <- c(res, list(discVarEstInputs=discVarEstInputs))
    }
    if(returnCumulative) {
      #assign to 0 if values are close to 0 for correct DOF correction
      cumVarEstInputs$JK$value[which(abs(cumVarEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(max(cumVarEstInputs$JK$JKreplicate))))] <- 0 
      cumVarEstInputs$PV <- unfactor(cumVarEstInputs$PV)
      rownames(cumVarEstInputs$PV) <- NULL
      cumVarEstInputs$JK <- unfactor(cumVarEstInputs$JK)
      rownames(cumVarEstInputs$JK) <- NULL
      cumVarEstInputs <- col2VarEstInputs(cumVarEstInputs)
      res <- c(res, list(cumVarEstInputs=cumVarEstInputs))
    }
  }
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data), nUsed=edfDTnrow))
  class(res) <- "achievementLevels"
  return(res)
}

renameLevelsToVariables <- function(data, estAL) {
  cn <- colnames(data)
  levels <- c()
  data$variable <- ""
  for(i in seq_along(names(estAL))) {
    leveli <- paste0(names(estAL)[i], "_Level")
    if(i == 1){
      data$variable <- data[ , leveli]
    } else {
      levels <- cartFactor(data$variable, data[ , leveli])
      data$variable <- factor(paste0(as.character(data$variable), ":", as.character(data[ , leveli])), levels=levels, labels=levels)
    }
    data[ , leveli] <- NULL
  }
  return(data)
}

# fix pre-varEstInputs from achievementLevels to have all column in variable 
col2VarEstInputs <- function(varEstInputs) {
  res <- lapply(varEstInputs, cov2VarEstPart)
}

# fix pre-varEstInputs PV and JK from achievementLevels to have all column in variable 
# puts other variables into the "variable" column with colon delimiters
cov2VarEstPart <- function(varEstInput) {
  cn <- colnames(varEstInput)
  cnr <- cn[!cn %in% c("PV", "variable", "value", "JKreplicate")]
  for(i in seq_along(cnr)) {
    varEstInput$variable <- paste0(varEstInput$variable, ":", cnr[i], "=", varEstInput[ , cnr[i]])
    varEstInput[ , cnr[i] ] <- NULL
  }
  return(varEstInput)
}


assertArgument <- function(arguments, data){
  argumentName <- deparse(substitute(arguments))
  switch(argumentName,
         "data"= {
           # Check data supplied are correct
           checkDataClass(arguments, c("edsurvey.data.frame", 
                                       "light.edsurvey.data.frame", 
                                       "edsurvey.data.frame.list"))
         },
         "als" = {
           cutpoints <- get("cutpoints", envir = parent.frame())
           if(arguments[1] == "Not Found" & is.null(cutpoints)) {
             stop(paste0("Default achievement levels not found. The ", sQuote("cutpoints"), " argument must be set."))
           }
         },
         "wgt" = {
           if (arguments == "") {
             stop("Argument ", sQuote("weightVar"), " is required.")
           }
           if (!(arguments %in% attributes(getAttributes(data, "weights"))$names)) {
             stop(paste0("Argument ", sQuote("weightVar"), " value of ", sQuote(arguments), 
                         " is not available on the data.")) 
           }
         },
         "yvar" = {
           # Check to see if at least one plausible value variable identified
           if(length(arguments) == 0) {
             stop(paste0("At least one variable in ", sQuote("achievementVars"), " must have plausible values to calculate achievement levels."))
           }
         },
         "edfDT" = {
           # check if there is any data
           if(nrow(arguments) <= 0) {
             stop(paste0("No data to analyze. Check if there are complete cases."))
           }
         },
         "cutpoints" = {
           # check if cutpoints are numeric
           if (!is.numeric(arguments)) {
             stop(paste0(dQuote("cutpoints"), " must be numeric values."))
           }
         }
  )
}

sortALResults <- function(als, returnCumulative) {
  sortList <- paste0("Below ", names(als)[1])
  if (!returnCumulative){
    for (i in 1:length(als)) {
      sortList <- c(sortList, paste0("At ", names(als)[i]))
    }
  } else {
    for (i in 1:(length(als) - 1)) {
      sortList <- c(sortList, paste0("At or Above ", names(als)[i]))
    }
    sortList <- c(sortList, paste0("At ", tail(names(als), n = 1)))
  }
  return(sortList)
}

# Recode edf with achievement levels
recodeEdf <- function(edfDT, pvs, als, returnCumulative, cumulativeLevel = 0){
  levelCols <- paste0(pvs,"_lvl")
  # clean existing level columns
  junk <- intersect(levelCols, names(edfDT))
  if (length(junk) > 0) {
    edfDT <- edfDT[,(junk) := NULL]
  }
  
  # build level columns for each pv
  if (!returnCumulative) {
    discreteLabels <- c(paste0("Below ", names(als)[1]), paste0("At ", names(als)[1]))
    if (length(als) > 1) {
      discreteLabels <- c(discreteLabels, paste0("At ", names(als)[2:length(als)]))
    }
    edfDT <- edfDT[,(levelCols) := lapply(.SD, function(c) {
      cut(c, breaks = c(-Inf, als, Inf), labels = discreteLabels, right = FALSE)
    }),
    .SDcols = pvs]
  } else {
    edfDT <- edfDT[,(levelCols) := lapply(.SD, function(c) {
      ifelse(c < als[cumulativeLevel], "Other", paste0("At or Above ", names(als)[cumulativeLevel]))
    }),
    .SDcols = pvs]
  }
}

# Estimation of weighted percentages when plausible values are present
calculateAL <- function(recodeEdfResults, pvs, jrrIMax, returnVarEstInputs, 
                        achievementVars, aggregateBy, wgt, jkSumMultiplier, jkWeights, returnNumberOfPSU){
  # result list
  # 1. res: discrete or cumulative result by Level and achievementVars/ aggregateBy variables
  # 2. VarEstImput: JK (jrr) by PV (jrrIMAx), Level, achievementVars, jk
  # 3. VarEstInput: PV (imp) by PV (M/ npv), Level, achievementVars
  alList <- list()
  
  # Preparation: Construct 2 important data.tables for later calculation
  M <- length(pvs)
  suppressWarnings(W <- sum(recodeEdfResults[,get(wgt)])) # total population weight
  # imp_dt is used for res and VarEstInput$PV
  
  suppressWarnings(imp_dt <- lapply(1:M,function(i) {
    recodeEdfResults[,list(lengthY = .N, sumY = sum(get(wgt))), by = c(paste0(pvs[i],"_lvl"),achievementVars)][,PV:=i]
  }))
  imp_dt <- rbindlist(imp_dt, use.names = FALSE)
  names(imp_dt)[1] <- "Level"
  # end imp_dt

  #jrr_dt is used for res (SE) and VarEstInput$JK
  jrr_dt <- lapply(1:jrrIMax, function(i) {
    recodeEdfResults[,lapply(.SD,sum), by = c(paste0(pvs[i],"_lvl"),achievementVars), .SDcols = jkWeights][,PV:=i]
  })

  jrr_dt <- rbindlist(jrr_dt, use.names = FALSE)
  names(jrr_dt)[1] <- "Level"
  # end jrr_dt
  
  # Count unique PSU and stratum combinations
  if (returnNumberOfPSU){
    nPSU_dt <- lapply(1:jrrIMax, function(i) {
      recodeEdfResults[,lapply(.SD, function(x) length(unique(x))), 
                       by = c(paste0(pvs[i],"_lvl"), achievementVars), 
                       .SDcols = "stratumAndPSU"][,PV:=i]
    })
    nPSU_dt <- rbindlist(nPSU_dt, use.names = FALSE)
    names(nPSU_dt)[1] <- "Level"
    names(nPSU_dt)[names(nPSU_dt) == "stratumAndPSU"] <- "nPSU"
  }
  # end nPSU_dt
  
  # 1. Calculate weighted percentages (N, wtdN, Percent)
  res <- imp_dt[,list(wtdN=sum(sumY), N = sum(lengthY)), by = c("Level",achievementVars)]
  res <- res[, Percent:=wtdN * 100 / sum(.SD$wtdN), by = aggregateBy]
  res <- res[,`:=`(wtdN = wtdN/M, N = N/M)]

  # add 'value' to imp_dt
  imp_dt <- merge(imp_dt, res[,c("Level", achievementVars, "Percent"), with = FALSE], 
    by = c("Level", achievementVars), all.x = TRUE, all.y = TRUE)
  
  imp_dt <- imp_dt[,pcti:=100*sumY/sum(.SD$sumY), by = c("PV",aggregateBy)][,value:=(Percent - pcti)]

  # add 'value' to jrr_dt
  jrr_dt <- merge(jrr_dt, imp_dt[,c("PV","Level",achievementVars,"pcti"), with = FALSE], by = c("PV","Level",achievementVars),
    all.x = TRUE, all.y = FALSE)
  jrr_dt <- jrr_dt[,(jkWeights):=lapply(.SD, function(jj) 100*jj/sum(jj) - pcti), .SDcols = jkWeights, by = c("PV",aggregateBy)][,pcti:=NULL]
  jrr_dt <- melt(jrr_dt, id.vars = c("PV","Level", achievementVars),
                 variable.name = "JKreplicate", value.name = "value")

  # 2. Estimate standard error of weighted percentages when plausible values
  # are present, using the jackknife method (SE)
  # a. res$Vjrr
  res <- merge(res, jrr_dt[,list(Vjrr = jkSumMultiplier * sum(value^2)/jrrIMax), 
                           by = c("Level",achievementVars)], sort = FALSE, by = c("Level", achievementVars), all.x = TRUE)
  # b. res$Vimp
  res <- merge(res, imp_dt[,list(Vimp = sum(value^2)*(M+1)/(M*(M-1))), by = c("Level", achievementVars)], 
               sort = FALSE, by = c("Level", achievementVars), all.x = TRUE)
  
  # c. StandardError
  res <- res[,StandardError:= sqrt(Vjrr + Vimp)]
  if (returnNumberOfPSU) {
    res <- merge(res, nPSU_dt, sort = FALSE, by = c("Level", achievementVars), all.x = TRUE)
    alList[[1]] <- as.data.frame(res)[,c("Level",achievementVars, "N", "wtdN","Percent","StandardError", "nPSU")]
  } else {
    alList[[1]] <- as.data.frame(res)[,c("Level",achievementVars, "N", "wtdN","Percent","StandardError")]
  }

  
  # 3. Return VarEstInputs (JK and PV)
  if (returnVarEstInputs) {
    # JK
    jrr_dt <- jrr_dt[Level!="Other"][,JKreplicate := as.integer(gsub("[^0-9]","",JKreplicate))]
    jrrResultOrder <- c("PV","JKreplicate", achievementVars,"Level","value")
    jrr_dt <- as.data.frame(jrr_dt)[,jrrResultOrder]
    names(jrr_dt) <- c("PV","JKreplicate", achievementVars,"variable","value")
    # rearrange output
    for (i in c("variable","JKreplicate","PV")) {
      jrr_dt <- jrr_dt[order(jrr_dt[,i]),]
    }
    jrr_dt$variable <- as.character(jrr_dt$variable)
    alList[[2]] <- jrr_dt
    # PV
    imp_dt <- imp_dt[Level!="Other"][,PV := as.integer(gsub("[^0-9]","",PV))]
    impResultOrder <- c("PV", achievementVars, "Level", "value")
    imp_dt <- as.data.frame(imp_dt)[,impResultOrder]
    names(imp_dt) <- c("PV", achievementVars, "variable", "value")
    # rearrange output
    for (i in c("variable","PV")){
      imp_dt <- imp_dt[order(imp_dt[,i]),]
    }
    imp_dt$variable <- as.character(imp_dt$variable)
    alList[[3]] <- imp_dt
  }
  return(alList)
}

#' @title Print AchievementLevels Results
#'
#' @description Prints details of discrete and cumulative achievement levels
#'  calculated using weights and variance
#' estimates appropriate for the \code{edsurvey.data.frame}.
#' 
#' @param x             an \code{achievementLevels} object
#' @param printCall     a logical value; by default (\code{TRUE}), prints details about plausible 
#'                      values and weights used for calculating achievement levels
#' @param printDiscrete a logical value; by default (\code{TRUE}), prints discrete achievement 
#'                      levels if they are present in \code{x}
#' @param printCumulative a logical value; by default (\code{TRUE}), prints cumulative achievement 
#'                        levels if they are present in \code{x}
#' @param ... these arguments are not passed anywhere and are included only for compatibility
#' @method print achievementLevels
#' @author Huade Huo and Ahmad Emad 
#' @export
print.achievementLevels <- function(x, printCall=TRUE, printDiscrete=TRUE, printCumulative=TRUE, ...) {
  if("callVars" %in% names(x)) {
    ll <- "1"
  }
  else {
    ll <- names(x)
  }
  
  for(i in ll) {
    if("callVars" %in% names(x)) {
      x1 <- x
    }
    else {
      x1 <- x[[i]]
    }
    
    if(length(ll) != 1) {
      cat("\n\nOutput for dataset ", i, "\n")
    }
    
    if(printCall) {
      cv <- x1$callVars
      # comment form PB, I'm not sure what makes the most sense here. Look at print.summary.lm.sdf function
      avs <- paste(cv$achievementVars, collapse=", ")
      p0 <- FALSE
      if(nchar(avs) > 0) {
        cat(paste0("\nAchievementVars: ", avs, "\n"))
        p0 <- TRUE
      }
      ab <- paste(cv$aggregateBy, collapse=", ")
      if(nchar(ab) > 0) {
        if(!p0) {
          cat("\n")
        }
        cat(paste0("aggregateBy: ", ab, "\n"))
        p0 <- TRUE
      }
      if(p0) {
        cat("\n")
      }
      cat(paste0("Achievement Level Cutpoints:\n"))
      if(length(cv$cutpoints) > 1) {
        lapply(names(cv$cutpoints), function(name) {
          cat(paste0(name, ":\n"))
          cat(cv$cutpoints[[name]], "\n")
        })
        cat("\n")
      } else {
        cat(cv$cutpoints[[1]], "\n\n")
      }
      cat(paste0("Plausible values: ", cv$npv, "\n"))
      cat(paste0("jrrIMax: ", cv$jrrIMax, "\n"))
      cat(paste0("Weight variable: ", sQuote(cv$weight), "\n"))
      cat(paste0("Variance method: ",cv$varMethod,"\n"))
      if(!is.na(cv$njk)) {
        cat(paste0("JK replicates: ", cv$njk, "\n"))
      }
      cat(paste0("full data n: ", x1$n0, "\n"))
      cat(paste0("n used: ", x1$nUsed, "\n"))
      cat("\n")
    }
    if(printDiscrete & !is.null(x1$discrete)) {
      cat("\nDiscrete\n")
      print(x1$discrete, row.names=FALSE) 
    }
    if(printCumulative & !is.null(x1$cumulative)) {
      cat("\nCumulative\n")
      print(x1$cumulative, row.names=FALSE)
    }
  }
}

getEstAL <- function(pvEst, stat, wgt, longReturn=TRUE) {
  # just pass names
  pv0 <- unlist(lapply(pvEst, function(x) { x[1] }))
  T_0 <- stat(pv=pv0, w=wgt, short=FALSE)
  for(i in 1:length(pvEst)) {
    colnames(T_0)[i] <- paste0(names(pvEst)[i], "_Level")
  }
  T_n <- matrix(0, ncol = nrow(T_0), nrow = length(pvEst[[1]]))
  T_n[1, ] <- T_0$Percent
  if(length(pvEst[[1]]) > 1) {
    for(n in 2:length(pvEst[[1]])) {
      pvi <- unlist(lapply(pvEst, function(x) { x[n] }))
      st <- stat(pv=pvi, w=wgt, short=FALSE)
      T_0$wtdN <- T_0$wtdN + st$wtdN
      T_0$N <- T_0$N + st$N
      T_0$Percent <- T_0$Percent + st$Percent
      T_n[n, ] <- st$Percent
    }
  }
  T_0$wtdN <- T_0$wtdN / length(pvEst[[1]])
  T_0$N <- T_0$N / length(pvEst[[1]])
  # force wtdN/denom to agree with percent
  if("wtdN3" %in% colnames(T_0)) {
    # use aggregation weighted N 
    T_0$Denom <- ave(T_0$wtdN3, T_0$DenomGroup, FUN=sum)
    T_0$wtdN3 <- NULL
  } else {
    T_0$Denom <- ave(T_0$wtdN, T_0$DenomGroup, FUN=sum)
  }
  T_0$Percent <- 100*T_0$wtdN/T_0$Denom
  T_0$Denom <- NULL # we do not need this now
  T_0$DenomGroup <- NULL # we do not need this now
  Ta <- T_0$Percent
  colnames(T_n) <- paste0("Row",1:ncol(T_n))
  names(Ta) <- paste0("Row",1:ncol(T_n))
  for(i in 1:length(pvEst)) {
    colnames(T_0)[colnames(T_0) == names(pvEst)[i]] <- paste0(names(pvEst)[i], "_Level")
  }
  T_n <- -1 * t(t(T_n) - T_0$Percent)
  res <- list(est=T_0, coef=T_n)
  return(res)
}


# helper function to calculate cumulative achievement levels
cumsum2 <- function(x) {
  return(c(x[1], rev(cumsum(rev(x[-1])))))
}

fixALVarEstInputRows <- function(vei, namedData) {
  icn <- intersect(colnames(namedData), colnames(vei))
  icn <- setdiff(icn, "variable")
  for(vi in icn) {
    namedData[ , vi] <- NULL
  }
  res <- merge(namedData, vei, by.x="row", by.y="variable", all.x=TRUE, all.y=FALSE)
  vn <- colnames(namedData)
  vn <- vn[!vn %in% "row"]
  res$row <- NULL
  cn <- colnames(res)
  cn <- cn[!cn %in% c("PV", vn, "value")]
  res <- res[ , c("PV", cn, vn, "value")]
  ov <- list(res$PV)
  if("JKreplicate" %in% colnames(res)) {
    ov <- c(ov, list(res$JKreplicate))
  }
  for(vni in seq_along(vn)) {
    ov <- c(ov, list(res[,vn[vni]]))
  }
  res <- res[do.call(order, ov), ] 
  return(res)
}

unfactor <- function(x) {
  for(i in 1:ncol(x)) {
    if(inherits(x[ , i], "factor")) {
      x[ , i] <- as.character(x[ , i])
    }
  }
  return(x)
}