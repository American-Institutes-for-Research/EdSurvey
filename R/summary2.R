#' @title Summarize edsurvey.data.frame Variables
#'
#' @description Summarizes \code{edsurvey.data.frame} variables.
#'
#' @param data an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list}, or \code{light.edsurvey.data.frame}
#' @param variable character vector of variable names
#' @param weightVar character weight variable name. Default is the default weight of \code{data} if it exists.
#'                  If the given survey data do not have a default weight,
#'                  the function will produce unweighted statistics instead. 
#'                  Can be set to \code{NULL} to return unweighted statistics.
#' @param omittedLevels a logical value. When set to \code{TRUE}, drops those levels of the specified \code{variable}.
#'                     Use print on an \code{edsurvey.data.frame} to see the omitted levels. Defaults to \code{FALSE}.
#'
#' @return 
#' summary of weighted or unweighted statistics of a given variable in an \code{edsurvey.data.frame}  
#' 
#' For categorical variables, the summary results are a crosstab of all variables and include the following:
#'   \item{[variable name]}{level of the variable in the column name that the row regards. There is one column per element of \code{variable}.}
#'   \item{N}{number of cases for each category. Weighted N also is produced if users choose to produce weighted statistics.}
#'   \item{Percent}{percentage of each category. Weighted percent also is produced if users choose to produce weighted statistics.}
#'   \item{SE}{standard error of the percentage statistics}
#'  
#' For continuous variables, the summary results are by variable and include the following:
#'   \item{Variable}{name of the variable the row regards}
#'   \item{N}{total number of cases (both valid and invalid cases)}
#'   \item{Min.}{smallest value of the variable}
#'   \item{1st Qu.}{first quantile of the variable}
#'   \item{Median}{median value of the variable}
#'   \item{Mean}{mean of the variable}
#'   \item{3rd Qu.}{third quantile of the variable}
#'   \item{Max.}{largest value of the variable}
#'   \item{SD}{standard deviation or weighted standard deviation}
#'   \item{NA's}{number of \code{NA} in variable and in weight variables}
#'   \item{Zero weights}{number of zero weight cases if users choose to produce weighted statistics}
#' 
#' If the weight option is chosen, the function produces weighted percentile and standard deviation. Refer to the vignette titled 
#' \emph{\href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey}} and
#' the vignette titled
#' \emph{\href{https://www.air.org/sites/default/files/EdSurvey-Percentiles.pdf}{Methods Used for Estimating Percentiles in EdSurvey}}
#' for how the function calculates these statistics (with and without plausible values). 
#' 
#' @importFrom stats quantile
#' @export
#' @example man\examples\summary2.R
#' @seealso \code{\link{percentile}} 
#' @author Paul Bailey and Trang Nguyen
summary2 <- function(data, variable,
                     weightVar = attr(getAttributes(data, "weights"), "default"),
                     omittedLevels = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("light.edsurvey.data.frame", "edsurvey.data.frame"))
  callc <- match.call()
  # check if weightVar is valid
  if(inherits(weightVar, "character") | is.null(weightVar)) {
    # use only first weightVar
    while(length(weightVar) > 1) {
      weightVar <- weightVar[[1]]
    }
  } else {
    stop(paste0("The argument ", dQuote("weightVar"), " must be a quoted variable name."))
  }
  weightVar <- weightVar[[1]]
  if (is.null(weightVar) || !weightVar %in% colnames(data)) {
    callc$weightVar <- NULL
    edf <- getData(data, variable, omittedLevels = omittedLevels, includeNaLabel = !omittedLevels, dropUnusedLevels=TRUE)
    N <- nrow(edf)
    if(length(unique(typeOfVariable(variable, data))) > 1) {
      stop("Summarize only discrete or only continious variables together.")
    }
    if (unique(typeOfVariable(variable, data)) == "discrete") {
      ret <- as.data.frame(ftable(edf[ , variable], exclude = NULL))
      colnames(ret) <- c(variable, "N")
      ret$Percent <- ret$N/N * 100
    } else {
      ret <- lapply(1:ncol(edf), function(i) {
        descriptiveContinuous(edf[[i]])
      })
      ret <- cbind("Variable" = names(edf), as.data.frame(do.call('rbind', ret)))
    }
    ret <- list(summary=ret)
    ret$call <- callc
    class(ret) <- "summary2"
    return(ret)
  } # end if (is.null(weightVar) || !weightVar %in% colnames(data))

  linkingError <- "NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking", variable, fixed=TRUE))
  if(linkingError) {
    stop("summary2 does not support linking error.")
  }
  
  callc$weightVar <- weightVar
  data <- getData(data, c(variable, weightVar), omittedLevels = omittedLevels,
                  addAttributes = TRUE, includeNaLabel = !omittedLevels, drop = FALSE)
  if(length(unique(typeOfVariable(variable, data))) > 1) {
    stop("The summary2 function requires that all variables are discrete or all variables are continuous.")
  }
  if(unique(typeOfVariable(variable, data)) == "discrete") {
    ret <- edsurveyTable(as.formula(paste0("~ ",paste(variable, collapse=" + "))),
                         data=data,returnMeans=FALSE,
                         omittedLevels = omittedLevels,
                         weightVar = weightVar)
    ret <- ret$data
    # change col names
    colnames(ret)[colnames(ret) == "WTD_N"] <- "Weighted N"
    colnames(ret)[colnames(ret) == "PCT"] <- "Weighted Percent"
    colnames(ret)[colnames(ret) == "SE(PCT)"] <- "Weighted Percent SE"
  } else { # end # end if(typeOfVariable(variable,data) == "discrete")
    variableR <- variable
    # build a data.frame, robust to vector "variable"
    ret <- do.call(rbind, lapply(variable, function(v) {
      as.data.frame(suppressWarnings(percentile(v, data=data,
                                                percentiles=c(0, 25, 50, 75, 100),
                                                weightVar = weightVar,
                                                confInt = FALSE)))$estimate
    }))
    # turn plausible value variables into their member parts
    ret0 <- lapply(1:length(variable), function(vi) {
      v <- variable[vi]
      if (hasPlausibleValue(v,data)) {
        v <- getPlausibleValue(v, data)
      }
      lm0 <- fast.sd(data[ , v], data[ , weightVar])
      meanVar <- lm0$mean
      sdVar <- lm0$std
      n <- nrow(data)
      wN <- sum(data[ , weightVar], na.rm = TRUE)
      nNA <- sum(rowSums(is.na(data[ , v, drop=FALSE])) > 0 | is.na(data[ , weightVar]))
      return(c(n, wN, ret[vi, 1:3], meanVar, ret[vi, 4:5], sdVar, nNA,
               sum(data[ , weightVar] == 0, na.rm = TRUE)))
    })
    ret <- do.call(rbind, ret0)
    colnames(ret) <- c("N","Weighted N","Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
                       "Max.", "SD", "NA's", "Zero weights")
    ret <- cbind("Variable" = variableR, as.data.frame(ret))
  } # end else for if(typeOfVariable(variable,data) == "discrete")
  ret <- list(summary=ret)
  ret$call <- callc
  class(ret) <- "summary2"
  return(ret)
}



#' @method print summary2
#' @export
print.summary2 <- function(x, ...) {
  call <- x$call
  if (!"weightVar" %in% names(call)) {
    cat("Estimates are not weighted.\n")
  } else {
    cat(paste0("Estimates are weighted using the weight variable ", sQuote(call$weightVar),"\n"))
  } # end if (!"weightVar" %in% names(call))
  print(x$summary, ...)
}



# calculates a mean and standard deviation (std) estimate based on variables
# that are already read in.
# variables: the variables in this variable (this is not vectorized, simply allows for PV vars)
# weight: the full sample weight
fast.sd <- function(variables, weight) { 
  y <- as.matrix(variables) # need to abstract PVs
  variance <- mu <- rep(NA, ncol(y))
  for(i in 1:ncol(y)) {
    y0 <- y[!is.na(y[ , i]) & !is.na(weight) & weight != 0,i]
    w0 <- weight[!is.na(y[ , i]) & !is.na(weight) & weight !=0]
    mu[i] <- sum(w0 * y0)/sum(w0)
    N <- length(w0[w0 > 0])
    variance[i] <- sum(w0 * (y0 - mu[i])^2)/( (N-1)/N * sum(w0))
  }
  return(list(mean=mean(mu), std=sqrt(mean(variance))))
}

fast.sd.var <- function(variables, weightVar, replicateWeights, 
                        jrrIMax=1, jkSumMultiplier=1, returnVarEstInputs=TRUE) {
  y <- as.matrix(variables) # need to abstract PVs
  variance <- mu <- rep(NA, ncol(y))
  stdVSamp <- 0
  jrrIMax <- min(jrrIMax, ncol(y))
  njr <- jrrIMax * ncol(replicateWeights)
  JK <- data.frame(PV=rep(1:jrrIMax,ncol(replicateWeights)),
                   JKreplicate=rep(1:ncol(replicateWeights),jrrIMax),
                   variable=rep("SD", njr),
                   value=rep(NA,njr))
  for(i in 1:ncol(y)) {
    y0 <- y[!is.na(y[,i]) & !is.na(weightVar) & weightVar != 0,i]
    w0 <- weightVar[!is.na(y[,i]) & !is.na(weightVar) & weightVar !=0]
    mu[i] <- sum(w0 * y0)/sum(w0)
    N <- length(w0[w0>0])
    variance[i] <- sum(w0 * (y0 - mu[i])^2)/((N-1)/N * sum(w0))
    if(i <= jrrIMax) {
      for(j in 1:ncol(replicateWeights)) {
        wj <- replicateWeights[,j]
        w0 <- wj[!is.na(y[,i]) & !is.na(wj) & wj !=0]
        y0 <- y[ !is.na(y[,i]) & !is.na(wj) & wj !=0,i]
        muj <- sum(w0 * y0)/sum(w0)
        variancej <- sum(w0 * (y0 - muj)^2)/( sum(w0))
        JK$value[(i-1)*ncol(replicateWeights)+j] <- sqrt(variancej) - sqrt(variance[i])
        stdVSamp <- stdVSamp + (sqrt(variancej) - sqrt(variance[i]))^2
      }
    }
  }
  stdVSamp <- stdVSamp / jrrIMax
  # imputation variance
  m <- ncol(y) 
  stdVImp <- 0
  if(m > 1) {
    stdVImp <- (m+1)/(m*(m-1)) * sum((sqrt(variance) - mean(sqrt(variance)))^2)
  }
  varEstInputs <- list(JK=JK)
  if(ncol(y) > 1) {
    PV <- data.frame(PV=1:ncol(y),
                     variable=rep("SD", ncol(y)),
                     value=sqrt(variance) - mean(sqrt(variance)))
    varEstInputs <- c(varEstInputs,list(PV=PV))
  }
  df <- DoFCorrection(varEstA=varEstInputs, varA="SD", method="JR")
  stdse <- sqrt(stdVImp + stdVSamp)
  if (returnVarEstInputs){
    return(list(mean=mean(mu), 
                std=mean(sqrt(variance)), 
                stdSE=stdse, 
                stdVar=list(varImp=stdVImp, varSamp=stdVSamp), 
                df=df,
                varEstInputs=varEstInputs))
  } else {
    return(list(mean=mean(mu), 
                std=mean(sqrt(variance)), 
                stdSE=stdse, 
                stdVar=list(varImp=stdVImp, varSamp=stdVSamp), 
                df=df))
  }
}

#' @title EdSurvey Standard Deviation
#'
#' @description Calculate the standard deviation of a numeric variable in an \code{edsurvey.data.frame}.
#'
#' @param data an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list}, or a \code{light.edsurvey.data.frame}
#' @param variable character vector of variable names
#' @param weightVar character weight variable name. Default is the default weight of \code{data} if it exists.
#'                  If the given survey data do not have a default weight,
#'                  the function will produce unweighted statistics instead. 
#'                  Can be set to \code{NULL} to return unweighted statistics.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \code{Vjrr} 
#'                   term (see 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param varMethod  deprecated parameter; \code{gap} always uses the jackknife variance estimation
#' @param omittedLevels a logical value. When set to \code{TRUE}, drops those levels of the specified \code{variable}.
#'                     Use print on an \code{edsurvey.data.frame} to see the omitted levels. Defaults to \code{FALSE}.
#' @param defaultConditions a logical value. When set to the default value of
#'                          \code{TRUE}, uses the default conditions stored in
#'                           an \code{edsurvey.data.frame} to subset the data. Use
#'                          \code{print} on an \code{edsurvey.data.frame} to
#'                          see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as \code{recode} \code{=} \code{list(var1}
#'               \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to}
#'               \code{=} \code{"d"))}. 
#' @param targetLevel a character string. When specified, calculates the gap in
#'                    the percentage of students at
#'                    \code{targetLevel} in the \code{variable} argument, which is useful for
#'                    comparing the gap in the percentage of students at a
#'                    survey response level.
#' @param jkSumMultiplier when the jackknife variance estimation method---or
#'                        balanced repeated replication (BRR) 
#'                        method---multiplies the final jackknife variance estimate by a value, 
#'                        set \code{jkSumMultiplier} to that value.
#'                        For an \code{edsurvey.data.frame}, or
#'                        a \code{light.edsurvey.data.frame},
#'                        the recommended value can be recovered with
#'                        \code{EdSurvey::getAttributes(}\code{myData,} \code{"jkSumMultiplier")}.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allows for
#'                           the computation
#'                           of covariances between estimates.
#'                           
#' @return 
#'  a list object with elements:
#'  \item{mean}{the mean assessment score for \code{variable}, calculated according to the vignette titled 
#'             \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistical Methods Used in EdSurvey}}
#'  \item{std}{the standard deviation of the \code{mean}}
#'  \item{stdSE}{the standard error of the \code{std}}
#'  \item{df}{the degrees of freedom of the \code{std}}
#'  \item{varEstInputs}{the variance estimate inputs used for calculating covariances with \code{\link{varEstToCov}}. Only returned with \code{returnVarEstInputs} is \code{TRUE}}
#'
#' @author Paul Bailey and Huade Huo
#' @example man/examples/SD.R
#' @export
SD <- function(data, 
               variable, 
               weightVar=NULL, 
               jrrIMax=1, 
               varMethod="jackknife",
               omittedLevels=TRUE,
               defaultConditions=TRUE,
               recode=NULL,
               targetLevel=NULL,
               jkSumMultiplier=getAttributes(data, "jkSumMultiplier"), 
               returnVarEstInputs=FALSE) {
  # check incoming variables
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  # !missing allows the user to intentionally set the weight to NULL to get unweighted
  if(missing(weightVar)) {
    weightVar <- attributes(getAttributes(data, "weights"))$default
  }
  gg <- getData(varnames=c(variable, weightVar), data=data, omittedLevels=omittedLevels,
                recode=recode, defaultConditions=defaultConditions)
  repw <- unique(getWeightJkReplicates(var=weightVar, data=data))
  if(hasPlausibleValue(data=data, variable)) {
    var <- gg[,getPlausibleValue(data=data, variable)]
  } else {
    var <- gg[,variable]
  }
  
  if(!is.null(targetLevel)) {
    var <- var == targetLevel
  }
  fast.sd.var(var,
              weightVar=gg[,weightVar],
              replicateWeights=gg[,repw],
              jrrIMax=jrrIMax,
              jkSumMultiplier=jkSumMultiplier,
              returnVarEstInputs=returnVarEstInputs)
}

# returns the N, min, quartiles, max, mean, and sd of x, and number of NA's
# does no checking of x
descriptiveContinuous <- function(x) {
  # type=8 is the unbiased quantile, also what is used in EdSurvey::percentile
  q <- quantile(x, probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE, type = 8)
  ret <- c(length(x),q[1:3],mean(x,na.rm=TRUE),q[4:5],sd(x,na.rm = TRUE), sum(is.na(x)))
  names(ret) <- c("N","Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
                  "Max.", "SD", "NA's")
  return(ret)
}
