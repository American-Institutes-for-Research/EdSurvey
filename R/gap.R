#' @title Gap Analysis
#'
#' @description Compares the average levels of a variable between two groups
#'              that potentially share members.
#'
#' @param variable a character indicating the variable to be compared,
#'                 potentially with a subject scale or subscale
#' @param data     an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}
#' @param groupA   an expression or character expression that defines a condition for the subset.
#'                 This subset will be compared to \code{groupB}. If not specified, it will define
#'                 a whole sample as in \code{data}. 
#' @param groupB   an expression or character expression that defines a condition for the subset.
#'                 This subset will be compared to \code{groupA}. If not specified, it will define
#'                 a whole sample as in \code{data}. If set to \code{NULL}, estimates for the second group 
#'                 will be dropped.
#' @param percentiles a numeric vector. The \code{gap} function calculates the
#'                    mean when this
#'                    argument is omitted or set to \code{NULL}. Otherwise,
#'                    the gap at the percentile given is calculated.
#' @param achievementLevel the achievement level(s) at which percentages
#'                         should be calculated
#' @param achievementDiscrete a logical indicating if the achievement level
#'                            specified in the \code{achievementLevel}
#'                            argument should be interpreted as discrete
#'                            so that
#'                            just the percentage in that particular achievement
#'                            level
#'                            will be included. Defaults to \code{FALSE}
#'                            so that
#'                            the percentage at or above that achievement level
#'                            will be
#'                            included in the percentage.
#' @param stDev a logical, set to \code{TRUE} to calculate the gap in standard deviations.
#' @param targetLevel a character string. When specified, calculates the gap in
#'                    the percentage of students at
#'                    \code{targetLevel} in the \code{variable} argument. This is useful for
#'                    comparing the gap in the percentage of students at a
#'                    survey response level.
#' @param weightVar a character indicating the weight variable to use.
#'                  See Details.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \code{Vjrr} 
#'                   term, or sampeling variance term, can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param varMethod  deprecated parameter, \code{gap} always uses the jackknife variance estimation
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of 
#'                      all factor variables.
#'                      Use \code{print} on an \code{edsurvey.data.frame}
#'                      to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value
#'                          of \code{TRUE}, uses the default 
#'                          conditions stored in \code{edsurvey.data.frame}
#'                          to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame}
#'                          to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as
#'               \code{recode} \code{=} \code{list(var1} \code{=}
#'               \code{list(from} \code{=} \code{c("a",} \code{"b",}
#'               \code{"c"),} \code{to} \code{=} \code{"d"))}.
#' @param referenceDataIndex a numeric used only when the \code{data} argument is an
#'                           \code{edsurvey.data.frame.list},
#'                           indicating which dataset is the reference
#'                           dataset that other datasets are compared with. 
#'                           Defaults to 1.
#' @param returnVarEstInputs a logical value; set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates which allows for the
#'                           computation
#'                           of covariances between estimates.
#' @param returnSimpleDoF    a logical value set to \code{TRUE} to return the degrees
#'                           of freedom for some statistics (see Value
#'                           section) that do not have a
#'                           \emph{t}-test; useful primarily for further computation
#' @param returnSimpleN      a logical value set to \code{TRUE} to add the count
#'                           (\emph{n}-size) of observations included in groups A and B
#'                           in the percentage object
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of 
#'                          PSUs used in the calculation
#' @param noCov set the covariances to zero in result
#' @param pctMethod a character that is one of \code{unbiased} or \code{simple}.
#'                  See the help for \code{\link{percentile}} for more information.
#' @param includeLinkingError a logical value set to \code{TRUE} to include the
#'                            linking error in variance estimation.
#'                            Standard errors (e.g., \code{diffAAse}, \code{diffBBse}, 
#'                            and \code{diffABABse}) and \emph{p}-values (e.g., \code{diffAApValue}, 
#'                            \code{diffBBpValue}, and \code{diffABABpValue}) would be adjusted for 
#'                            comparisons between digitally based assessments (DBA) and 
#'                            paper-based assessments (PBA) data.
#'                            This option is supported only for NAEP data.
#' @details This function calculates the gap between \code{groupA} and \code{groupB} (which 
#' may be omitted to indicate the full sample). The gap is
#' calculated for one of four statistics:
#' \describe{
#'   \item{the gap in means}{The mean score gap (in the score
#'      variable) identified in the \code{variable} argument.
#'      This is the default. The means and their standard errors are
#'      calculated using the methods
#'      described in the \code{\link{lm.sdf}} function documentation.}
#'   \item{the gap in percentiles}{The gap between respondents at
#'      the percentiles specified in the \code{percentiles} argument.
#'      This is returned when the \code{percentiles} argument is
#'      defined. The mean and standard error are computed as described in the 
#'      \code{\link{percentile}} function documentation.}
#'   \item{the gap in achievement levels}{The gap in the percentage of 
#'      students at (when \code{achievementDiscrete} is \code{TRUE}) or at
#'      or above (when \code{achievementDiscrete} is \code{FALSE}) a
#'      particular achievement level. This is used when the 
#'      \code{achievementLevel} argument is defined. The mean and standard error
#'      are calculated as described in the \code{\link{achievementLevels}}
#'      function documentation.}
#'   \item{the gap in a survey response}{The gap in the percentage of
#'      respondents responding at \code{targetLevel} to 
#'      \code{variable}. This is used when \code{targetLevel} is
#'      defined. The mean and standard deviation are calculated as described in
#'      the \code{\link{edsurveyTable}} function documentation.}
#' }
#' 
#' @return
#' The return type depends on if the class of the \code{data} argument is an
#' \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}. Both
#' include the call (called \code{call}), a list called \code{labels}, 
#' an object named \code{percentage}
#' that shows the percentage in \code{groupA} and \code{groupB}, and an object
#' that shows the gap called \code{results}. 
#'
#' The labels include the following elements:
#'   \item{definition}{the definitions of the groups}
#'   \item{nFullData}{the \emph{n}-size for the full dataset (before applying the definition)}
#'   \item{nUsed}{the \emph{n}-size for the data after the group is subsetted and other
#'                restrictions (such as omitted values) are applied}
#'   \item{nPSU}{the number of PSUs used in calculation--only returned when 
#'               \code{returnNumberOfPSU} \code{=} \code{TRUE}}
#'
#' The percentages are computed according to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#'  in the section
#' \dQuote{Estimation of Weighted Percentages When Plausible Values Are Not Present.}
#' The standard errors are calculated according to
#' \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Jackknife Method.}
#' Standard errors of differences are calculated as the square root of the typical
#' variance formula
#' \deqn{Var(A-B) = Var(A) + Var(B) - 2 Cov(A,B)}
#' where the covariance term is calculated as described in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#'  in the section
#' \dQuote{Estimation of Covariances.} These degrees of freedom are available only
#' with the jackknife variance estimation. The degrees of freedom used for hypothesis testing
#' are always set to the number of jackknife replicates in the data.
#'
#' \strong{the data argument is an edsurvey.data.frame}
#'   When the \code{data} argument is an \code{edsurvey.data.frame},
#'   \code{gap} returns an S3 object of class \code{gap}. 
#' 
#'   The \code{percentage} object is a numeric vector with the following elements:
#'     \item{pctA}{the percentage of respondents in \code{groupA} compared with the whole sample in \code{data}}
#'     \item{pctAse}{the standard error on the percentage of respondents in
#'                       \code{groupA}}
#'     \item{dofA}{degrees of freedom appropriate for a \emph{t}-test involving \code{pctA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{pctB}{the percentage of respondents in \code{groupB}.}
#'     \item{pctBse}{the standard error on the percentage of respondents in
#'                       \code{groupB}}
#'     \item{dofB}{degrees of freedom appropriate for a \emph{t}-test involving \code{pctA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{diffAB}{the value of \code{pctA} minus \code{pctB}}
#'     \item{covAB}{the covariance of \code{pctA} and \code{pctB}; used in
#'                  calculating \code{diffABse}.}
#'     \item{diffABse}{the standard error of \code{pctA}
#'                            minus \code{pctB}}
#'     \item{diffABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAB}
#'                         is zero}
#'     \item{dofAB}{degrees of freedom used in calculating
#'                       \code{diffABpValue}}
#' 
#'   The \code{results} object is a numeric data frame with the following elements:
#'     \item{estimateA}{the mean estimate of \code{groupA} (or the percentage estimate
#'                      if \code{achievementLevel} or \code{targetLevel} is specified)}
#'     \item{estimateAse}{the standard error of \code{estimateA}}
#'     \item{dofA}{degrees of freedom appropriate for a \emph{t}-test involving \code{meanA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{estimateB}{the mean estimate of \code{groupB} (or the percentage estimate
#'                      if \code{achievementLevel} or \code{targetLevel} is specified)}
#'     \item{estimateBse}{the standard error of \code{estimateB}}
#'     \item{dofB}{degrees of freedom appropriate for a \emph{t}-test involving \code{meanB}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{diffAB}{the value of \code{estimateA} minus \code{estimateB}}
#'     \item{covAB}{the covariance of \code{estimateA} and \code{estimateB}. Used in
#'                  calculating \code{diffABse}.}
#'     \item{diffABse}{the standard error of \code{diffAB}}
#'     \item{diffABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAB}
#'                         is zero.}
#'     \item{dofAB}{degrees of freedom used for the \emph{t}-test on \code{diffAB}}
#'   
#'   If the gap was in  achievement levels or percentiles and more
#'   than one percentile or achievement level is requested,
#'   then an additional column
#'   labeled \code{percentiles} or \code{achievementLevel} is included
#'   in the \code{results} object.
#'
#'   When \code{results} has a single row and when \code{returnVarEstInputs}
#'   is \code{TRUE}, the additional elements \code{varEstInputs} and
#'   \code{pctVarEstInputs} also are returned. These can be used for calculating
#'   covariances with \code{\link{varEstToCov}}.
#' 
#' \strong{the data argument is an edsurvey.data.frame.list}
#'   When the \code{data} argument is an \code{edsurvey.data.frame.list},
#'   \code{gap} returns an S3 object of class \code{gapList}.
#'   
#'   The \code{results} object in the \code{edsurveyResultList} is
#'   a \code{data.frame}. Each row regards a particular dataset from the
#'   \code{edsurvey.data.frame}, and a reference dataset is dictated by
#'   the \code{referenceDataIndex} argument.
#'   
#'   The \code{percentage} object is a \code{data.frame} with the following elements:
#'     \item{covs}{a data frame with a column for each column in the \code{covs}. See previous
#'                 section for more details.}
#'     \item{...}{all elements in the \code{percentage} object in the
#'                previous section}
#'     \item{diffAA}{the difference in \code{pctA} between the reference data
#'                   and this dataset. Set to \code{NA} for the
#'                   reference dataset.}
#'     \item{covAA}{the covariance of \code{pctA} in the reference data and
#'                  \code{pctA} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffAAse}{the standard error for \code{diffAA}}
#'     \item{diffAApValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAA}
#'                         is zero}
#'     \item{diffBB}{the difference in \code{pctB} between the reference data
#'                   and this dataset. Set to \code{NA} for the
#'                   reference dataset.}
#'     \item{covBB}{the covariance of \code{pctB} in the reference data and
#'                  \code{pctB} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffBBse}{the standard error for \code{diffBB}}
#'     \item{diffBBpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffBB}
#'                         is zero}
#'     \item{diffABAB}{the value of \code{diffAB} in the reference dataset
#'                            minus the value of \code{diffAB} in this dataset. Set
#'                            to \code{NA} for the reference dataset.}
#'     \item{covABAB}{the covariance of \code{diffAB} in the reference data and
#'                    \code{diffAB} on this row. Used in
#'                    calculating \code{diffABABse}.}
#'     \item{diffABABse}{the standard error for \code{diffABAB}}
#'     \item{diffABABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffABAB}
#'                         is zero}
#'
#'   The \code{results} object is a \code{data.frame} with the following elements:
#'     \item{...}{all elements in the \code{results} object in the
#'                previous section}
#'     \item{diffAA}{the value of \code{groupA} in the reference dataset minus
#'                          the value in this dataset. Set to \code{NA} for the
#'                          reference dataset.}
#'     \item{covAA}{the covariance of \code{meanA} in the reference data and
#'                  \code{meanA} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffAAse}{the standard error for \code{diffAA}}
#'     \item{diffAApValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAA}
#'                         is zero}
#'     \item{diffBB}{the value of \code{groupB} in the reference dataset minus
#'                          the value in this dataset. Set to \code{NA} for the
#'                          reference dataset.}
#'     \item{covBB}{the covariance of \code{meanB} in the reference data and
#'                  \code{meanB} on this row. Used in
#'                  calculating \code{diffBBse}.}
#'     \item{diffBBse}{the standard error for \code{diffBB}}
#'     \item{diffBBpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffBB}
#'                         is zero}
#'     \item{diffABAB}{the value of \code{diffAB} in the reference dataset
#'                            minus the value of \code{diffAB}
#'                            in this dataset. Set
#'                            to \code{NA} for the reference dataset.}
#'     \item{covABAB}{the covariance of \code{diffAB} in the reference data and
#'                    \code{diffAB} on this row. Used in
#'                    calculating \code{diffABABse}.}
#'     \item{diffABABse}{the standard error for \code{diffABAB}}
#'     \item{diffABABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffABAB}
#'                         is zero}
#'     \item{sameSurvey}{a logical value indicating if this line uses the same
#'                       survey as the reference line. Set to \code{NA} for the
#'                       reference line.}
#'
#' @author Paul Bailey, Trang Nguyen, and Huade Huo
#' @importFrom stats formula
#' @example man/examples/gap.R
#' @export
gap <- function(variable, data, groupA = "default", groupB = "default",
                percentiles=NULL,
                achievementLevel=NULL,
                achievementDiscrete=FALSE,
                stDev=FALSE,
                targetLevel=NULL,
                weightVar=NULL, jrrIMax=1,
                varMethod=c("jackknife"),
                omittedLevels=TRUE,
                defaultConditions=TRUE,
                recode=NULL,
                referenceDataIndex=1,
                returnVarEstInputs=FALSE,
                returnSimpleDoF=FALSE,
                returnSimpleN=FALSE,
                returnNumberOfPSU=FALSE,
                noCov=FALSE,
                pctMethod=c("unbiased", "symmetric", "simple"),
                includeLinkingError=FALSE) {
  if(is.character(substitute(groupA))) {
    groupA <- parse(text=groupA)[[1]]
  }
  if (all(as.character(substitute(groupA)) == 'default')) {
    groupB <- NULL
  }
  if(!missing(groupB)) {
    if(is.character(substitute(groupB))) {
      groupB <- parse(text=groupB)[[1]]
    }
  }
  if(!missing(varMethod)) {
    warning(paste0("Argument ", dQuote("varMethod"), " deprecated. The ", dQuote("gap") ," function always uses jackknife variance estimation."))
  }
  # check incoming variables
  call <- match.call()
  # get valid pctMethod
  pctMethod <- match.arg(pctMethod)
 
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  # if recode list is provided, it will be added to userConditions at first (before subset in groupA and groupB)
  if (!is.null(recode)) {
    data <- recode.sdf(data, recode)
  }

  if(inherits(data, "edsurvey.data.frame") | inherits(data, "light.edsurvey.data.frame")) {
    survey <- getAttributes(data, "survey")
  } else {
    # edsurvey.data.frame.list
    robustSurvey <- function(dat) {
      res <- tryCatch(getAttributes(dat, "survey"),
                      error=function(e) {
                        NULL
                      })
      return(res)
    }
    survey <- unique(unlist(lapply(data$data, robustSurvey)))
  }
  
  # check if linking error is supported for this survey
  if(!missing(includeLinkingError) && includeLinkingError) {
    checkDataClass(data, c("edsurvey.data.frame.list"))
    if(!any(c("NAEP", "PISA") %in% survey)) {
      stop("the argument ", dQuote("includeLinkingError"), " can only be set to ", dQuote("TRUE"), " when EdSurvey suports linking error for the survey. Currently EdSurvey supports linking error for NAEP and PISA.")
    }
  }
  # Set dbaLabels as NULL for linking error with non ESDFL object or 
  # includeLinkingError set as False
  dbaLabels <- NULL
  
  # gapStat required for PISA regardless of linking error
  if("PISA" %in% survey) {     
    if (!is.null(percentiles)) {
      gapStat <- "percentile"
    } else if (!is.null(achievementLevel)) {
      gapStat <- "AL"
    } else if (stDev) {
      gapStat <- "SD"
    } else {
      gapStat <- "Mean"
    }
  }
  # Get parameters for linking error
  if (!missing(includeLinkingError) && includeLinkingError)  {
    # the following is only for NAEP which has linking error within survey
    if("NAEP" %in% survey) {     
      if(inherits(data, "edsurvey.data.frame.list")) {
        uniqueSubject <- unique(unlist(lapply(data$data, getAttributes, "subject")))
        if (length(uniqueSubject) != 1) {
          stop(paste0("Linking error only support ", dQuote("edsurvey.data.frame.list"), " with a single subject. Found subjects: ", pasteItems(uniqueSubject), "."))
        }
      
        uniqueGrade <- unique(unlist(lapply(data$data, getAttributes, "gradeLevel")))
        if (length(uniqueGrade) != 1) {
          stop(paste0("Linking error only support ", dQuote("edsurvey.data.frame.list"), " with a single grade level. Found grades: ", pasteItems(uniqueGrade), "."))
        }
      }
        
      # For achievementDiscrete=TRUE, return "At basic" (not "below basic") if "achievementLevel = "basic"
      alLinkingErrorhelper <- function(alstrings, discrete) {
        matchedAL <- NULL
        for (alstring in alstrings) {
          if (discrete) {
            if (grepl("^basic", alstring, ignore.case = TRUE)) {
              matchedAL <- c(matchedAL, "At basic")
            } else {
              matchedAL <- c(matchedAL, 
                             c("Below basic", "At basic", "At proficient", 
                               "At advanced")[grepl(alstring, c("Below basic", "At basic", 
                                                                "At proficient", "At advanced"), 
                                                    ignore.case = TRUE)])
            }
          } else {
            matchedAL <- c(matchedAL, 
                           c("At or above basic", 
                             "At or above proficient",
                             "At advanced")[
                               grepl(alstring, c("At or above basic", "At or above proficient", "At advanced"), 
                                     ignore.case = TRUE)])
          }
        }
        if (length(matchedAL) != length(alstrings)) {
          stop("Linking error for provided achievement level ", pasteItems(alstrings[!alstrings %in% matchedAL]) , "is not available.")
        }
        return(matchedAL)
      }
        
      # Get DBA labels from DBA years
      # 2017 is the first NAEP DBA year
      if(inherits(data, "edsurvey.data.frame.list")) {
        allYears <- unlist(itterateESDFL(call("getAttributes", attribute="year"), data))
        dbaLabels <- data[[2]]$labels[allYears >= 2017]
        if (!all(dbaLabels %in% data[[2]]$labels)) {
          stop(paste0(pasteItems(dbaLabels[!dbaLabels %in% data[[2]]$labels]), 
                      " is not in the ", dQuote("edsurvey.data.frame.list"), " labels."))
        }
      } else {
        allYears <- getAttributes(data, attribute="year")
        dbaLabels <- NULL
      }
      if (!is.null(percentiles) && !all(percentiles %in% c(10, 25, 50, 75, 90))) {
        stop("NAEP linking error only supported for 10th, 25th, 50th, 75th, 90th percentiles.", 
             "Set ", dQuote("includeLinkingError"), " to ", dQuote("FALSE"), 
             "to calculate any percentile without linking error.")
      }
      if (!is.null(percentiles)) {
        gapStat <- percentiles
      } else if (!is.null(achievementLevel)) {
        gapStat <- alLinkingErrorhelper(achievementLevel, achievementDiscrete)
      } else if (stDev) {
        gapStat <- "SD"
      } else {
        gapStat <- "Mean"
      }
    }
  }

  # deal with the possibility that there is more than one line of results for various possible reasons
  if(inherits(data, "edsurvey.data.frame.list") | length(percentiles) > 1 | length(achievementLevel) > 1) {
    # a flag that is TRUE when there is no information in data$covs. Default to false
    nocovs <- FALSE
    if(!inherits(data, "edsurvey.data.frame.list")) {
      # this is a situation where data is not an esdfl. The following code
      # required it to be one, so make it one. But note that covs  
      # does not have real information in it.
      nocovs <- TRUE
      data <- edsurvey.data.frame.list(list(data), labels="data")
    }
    ll <- length(data$datalist)
    lpct <- ifelse(is.null(percentiles), 1, length(percentiles))
    lal <- ifelse(is.null(achievementLevel), 1, length(achievementLevel))
    
    # check variable specific to edsurvey.data.frame.list
    suppressWarnings(refi <- as.integer(referenceDataIndex))
    if(!refi %in% 1:ll) {
      stop(paste0("The argument ", sQuote("referenceDataIndex"), " must be an integer between 1 and ", ll, "."))
    }
    res <- list(results=list(),
                labels=list(A=substitute(groupA), B=substitute(groupB)))
    
    pctdf0 <- data$covs
    if(!nocovs) {
      pctdf0$covAA <- 0
      pctdf0$dofAA <- Inf
      pctdf0$covBB <- 0
      pctdf0$dofBB <- Inf
      pctdf0$covABAB <- 0
      pctdf0$dofABAB <- Inf
    }
    
    # make sure refi gets calculated first
    llvec <- unique(c(refi,1:ll))
    resilist <- list()
    # call gap for each edsurvey.data.frame
    for(i in llvec) {
      # for each element of the edsurvey.data.frame.list
      calli <- call
      # adjust the call per the needs of this call
      calli$data <- data$datalist[[i]]
      # we need the these inputs to calculate across essurvey.data.frame
      # covariances (but, only if they are the same dataset) an issue
      # that will be resolved later.
      calli$returnVarEstInputs <- TRUE
      # this returns the degrees of freedom for meanA and meanB
      # used for diffAA and diffBB, respectively
      calli$returnSimpleDoF <- TRUE
      # if these are NULL then setting them changes nothing
      calli$percentiles <- percentiles
      calli$achievementLevel <- achievementLevel
      calli <- as.list(calli)
      # use the same list of arguments but change the function to gapHelper
      calli[[1]] <- as.name("gapHelper")
      resilist[[i]] <- tryCatch(resi <- eval(as.call(calli)),
                                error=function(cond) {
                                  message(paste0("An error occurred while working on a dataset ",
                                                 pasteItems(dQuote(data$covs[i,])),
                                                 ". The results from that dataset will be excluded. Error message: ",
                                                 cond))
                                  return(0)
                                })
      # if there was not an error
      if(!inherits(resilist[[i]], "gap")) {
        # there was an error
        if(i %in% referenceDataIndex) {
          stop("An error prevented processing of the reference dataset. This must be fixed before comparisons to the reference dataset can be made.")
        }
      } else {
        resi$call <- NULL

        # the following code is to clean up the results (adding some columns to make the code to extract results from gapHelper more consistent)
        # after this code, we should expect that each gap result will have:
        # 1. resi$results will have 'statisticsLevel' column: indicate percentile or achievementLevel, or 'data' if none is specified
        # 2. resi$varEstInputs: PV and JK data frame will have 'Level' column similar to 'statisticsLevel' column in resi$results
        if (is.null(percentiles) && is.null(achievementLevel)) {
          resi$results$statisticsLevel <- "data"
        }
        # if no or only 1 percentile/achievementLevel is specified
        if (length(percentiles) <=1 && length(achievementLevel) <=1) {
          resi$varEstInputs$JK$Level <- "data"
          if (!is.null(resi$varEstInputs$PV)) { #PV is null for type = "eq"
            resi$varEstInputs$PV$Level <- "data"
          }
        }
        # in gapHelper, type = 'pct' will return 'percentiles' column and type = 'al' will return 'achievementLevel' column
        # this replacement is to use 1 consistent name for the result extraction code below
        # it will be renamed back to original names after the extraction
        colnames(resi$results) <- gsub("achievementLevel|percentiles","statisticsLevel",colnames(resi$results))
        resilist[[i]] <- resi
      }
    } # end for(i in llvec)
    
    # Extract results and compute AA or BB statistics
    lstats <- max(lpct, lal)
    for(j in 1:lstats) {
      resdf <- data$covs
      if(!nocovs) {
        # only generate these columns if there are multiple datasets
        resdf$covAA <- 0
        resdf$dofAA <- Inf
        resdf$covBB <- 0
        resdf$dofBB <- Inf
        resdf$covABAB <- 0
        resdf$dofABAB <- Inf
      }
      # to check whether the two samples are from the same survey or not
      # we need to use DOFCorrection and compute covariance if the two samples are from the same survey
      resdf$sameSurvey <- ifelse(1:nrow(resdf) == refi,
                                 rep(NA, nrow(resdf)),
                                 rep(FALSE, nrow(resdf)))
      if(lpct > 1) {
        resdf$percentiles <- percentiles[[j]]
      }
      for (i in llvec) {
        if(!inherits(resilist[[i]], "gap")) {
          # there was an error
          if(i %in% referenceDataIndex) {
            stop("An error prevented processing of the reference dataset. This must be fixed before comparisons to the reference dataset can be made.")
          }
          # this is to make sure that all achievementLevels or percentiles are created as to the reference edsurvey.data.frame
          # Missing values will be NA for statistics
          if (!is.null(achievementLevel)) {
            resdf$achievementLevel[i] <- resilist[[refi]]$results$statisticsLevel[j]
          } else if (!is.null(percentiles)) {
            resdf$percentiles[i] <- resilist[[refi]]$results$statisticsLevel[j]
          }
        } else { # there was not an error
          # dataframes do not like vectorwise assignment by column name,
          # so assign one at a time
          resi <- resilist[[i]]
  
          resi$results <- resi$results[j,]
          resi$varEstInputs$JK=resi$varEstInputs$JK[tolower(resi$varEstInputs$JK$Level) %in% tolower(c(resi$results$statisticsLevel,"data")),]
          if (!is.null(resi$varEstInputs$PV)) {
            resi$varEstInputs$PV <- resi$varEstInputs$PV[tolower(resi$varEstInputs$PV$Level) %in% tolower(c(resi$results$statisticsLevel,"data")),]
          }
                                              
          if (!is.null(achievementLevel)) {
            colnames(resi$results) <- gsub("statisticsLevel","achievementLevel",colnames(resi$results))
          } else if (!is.null(percentiles)) {
            colnames(resi$results) <- gsub("statisticsLevel","percentiles",colnames(resi$results))
          } else {
            resi$results$statisticsLevel <- NULL
          }
          for(ii in 1:length(resi$results)) {
            resdf[i,names(resi$results)[ii]] <- resi$results[[ii]]
          }
          skipB <- is.null(resi$labels$B) # a boolean to indicate whether groupB estimates are available in the results of gapHelper
          if(i == refi) {
            refVarEstInputs <- resi$varEstInputs
            refPctVarEstInputs <- resi$pctVarEstInputs
          }
          # refi is the index of reference dataset
          # AA and BB statistics of reference dataset will be set to NA
          if(i != refi) {
            resdf$dofAA[i] <- dofCompute((resi$results)$estimateAse, resdf[refi,"estimateAse"] ,(resi$results)[["dofA"]], resdf[refi,"dofA"])
            if (!skipB) {
              resdf$dofBB[i] <- dofCompute((resi$results)$estimateBse, resdf[refi,"estimateBse"] ,(resi$results)[["dofB"]], resdf[refi,"dofB"])
              resdf$dofABAB[i] <- dofCompute((resi$results)$diffABse, resdf[refi,"diffABse"] ,(resi$results)[["dofAB"]], resdf[refi,"dofAB"])
            }
            # if the datasets and from the same sample, add covariances and do DoFCorrection
            if(sameSurvey(data$datalist[[i]], data$datalist[[refi]])) {
              if (nrow(resi$varEstInputs$JK) != 0) { # some Level (percentile or achievementLevel) do not have varEstInput data so the subset
                                   # in line 476 will return an empty data.frame
                resdf$dofAA[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "A", method="JR")
              }
              resdf$covAA[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "A", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
              if (!skipB & nrow(resi$varEstInputs$JK) != 0) {
                resdf$dofBB[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "B", method="JR")
                resdf$dofABAB[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "A-B", method="JR")
                resdf$covBB[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                resdf$covABAB[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "A-B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
              }
              resdf$sameSurvey[i] <- TRUE
            } else {
              if("PISA" %in% survey) {
                linke <- 0
                if(includeLinkingError & gapStat %in% c("Mean", "percentile", "SD")) {
                  if(variable %in% c("read", "math", "scie")) {
                    yearref <- getAttributes(data$datalist[[refi]], "year")
                    yeari <- getAttributes(data$datalist[[i]], "year")
                    if(yeari != yearref) {
                      linke <- calLinkingErrorPISA(subject=variable, years=c(yearref, yeari))
                    }
                  } else {
                    warning("No published linking error for variable ", dQuote(variable),"\n")
                  }
                }
                covvars <- c("covAA", "covBB", "covABAB")
                for(cvi in 1:length(covvars)) {
                  if(covvars[cvi] %in% colnames(resdf)) {
                    resdf[i,covvars[cvi]] <- -1/2 * linke^2
                  }
                }
              }
            }
          } else { # end if(i != refi)
            # if this is refi, set these to NULL
            resdf$covAA[i] <- resdf$covBB[i] <- resdf$covABAB[i] <- NA
            resdf$dofAA[i] <- resdf$dofBB[i] <- resdf$dofABAB[i] <- NA
          }
          # for the first j value only, grab the percent
          if(j == 1) {
            for(ii in 1:ncol(resi$percentage)) {
              pctdf0[i,colnames(resi$percentage)[ii]] <- resi$percentage[,ii] 
              if(i != refi) {
                pctdf0$dofAA[i] <- dofCompute((resi$percentage)$pctAse, pctdf0[refi,"pctAse"] ,(resi$percentage)[["dofA"]], pctdf0[refi,"dofA"])
                if (!skipB) {
                  pctdf0$dofBB[i] <- dofCompute((resi$percentage)$pctBse, pctdf0[refi,"pctBse"] ,(resi$percentage)[["dofB"]], pctdf0[refi,"dofB"])
                  pctdf0$dofABAB[i] <- dofCompute((resi$percentage)$diffABse, pctdf0[refi,"diffABse"] ,(resi$percentage)[["dofAB"]], pctdf0[refi,"dofAB"])
                }
                
                if(sameSurvey(data$datalist[[i]], data$datalist[[refi]])) {
                  pctdf0$dofAA[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "A", method="JR")
                  pctdf0$covAA[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "A", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                  if (!skipB) {
                    pctdf0$dofBB[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "B", method="JR")
                    pctdf0$dofABAB[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "A-B", method="JR")
                    pctdf0$covBB[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                    pctdf0$covABAB[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "A-B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                  }
                }
              } else { # end if(i != refi)
                # if this is refi, set these to NULL
                pctdf0$covAA[i] <- pctdf0$covBB[i] <- pctdf0$covABAB[i] <- NA
                pctdf0$dofAA[i] <- pctdf0$dofBB[i] <- pctdf0$dofABAB[i] <- NA
              } # end else for (i != refi)
            } # end for(ii in 1:ncol(resi$percentage))
            if(returnSimpleN) {
              # return the n counts
              pctdf0$nA[i] <- resi$labels$nUsedA
              pctdf0$nB[i] <- resi$labels$nUsedB
            }
            if(returnNumberOfPSU) {
              pctdf0$nPSUA[i] <- ifelse(is.null(resi$labels$nPSUA),NA,resi$labels$nPSUA)
              pctdf0$nPSUB[i] <- ifelse(is.null(resi$labels$nPSUB),NA,resi$labels$nPSUB)
            }
          } # end if(j == 1) 
        } # end if(class(temp) == "gap")
      } # End of for(i in 1:i) loop
      # Compute diff statistics (across different edsurvey.data.frame)
      resdf$diffAA <- ifelse(1:nrow(resdf) == refi, NA, resdf$estimateA[refi] - resdf$estimateA)
      if (!is.null(dbaLabels)) {
        # get linking error
        leAA <- calLinkingError(X=resdf$estimateA[refi],
                                subjectI=uniqueSubject, 
                                gradeI=uniqueGrade, 
                                dependentVarI=variable, 
                                statElementI=gapStat,
                                gapI=FALSE)[j]
        resdf$diffAAse <- ifelse(1:nrow(resdf) == refi, NA, 
                                 ifelse(resdf$labels %in% dbaLabels, 
                                        sqrt(resdf$estimateAse[refi]^2 + resdf$estimateAse^2 - 2 * resdf$covAA),
                                        sqrt(resdf$estimateAse[refi]^2 + resdf$estimateAse^2 - 2 * resdf$covAA + leAA)))
      } else {
        resdf$diffAAse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$estimateAse[refi]^2 + resdf$estimateAse^2 - 2 * resdf$covAA))
      }
      
      resdf$diffAApValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffAA/resdf$diffAAse), df=resdf$dofAA)))
      if (!skipB) {
        resdf$diffBB <- ifelse(1:nrow(resdf) == refi, NA, resdf$estimateB[refi] - resdf$estimateB)
        if (!is.null(dbaLabels)) {
          leBB <- calLinkingError(X=resdf$estimateB[refi],
                                  subjectI=uniqueSubject, 
                                  gradeI=uniqueGrade, 
                                  dependentVarI=variable, 
                                  statElementI=gapStat,
                                  gapI=FALSE)[j]
          resdf$diffBBse <- ifelse(1:nrow(resdf) == refi, NA, 
                                   ifelse(resdf$labels %in% dbaLabels, 
                                          sqrt(resdf$estimateBse[refi]^2 + resdf$estimateBse^2 - 2 * resdf$covBB),
                                          sqrt(resdf$estimateBse[refi]^2 + resdf$estimateBse^2 - 2 * resdf$covBB + leBB)))
        } else {
          resdf$diffBBse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$estimateBse[refi]^2 + resdf$estimateBse^2 - 2 * resdf$covBB))
        }
        resdf$diffBBpValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffBB/resdf$diffBBse), df=resdf$dofBB)))
        resdf$diffABAB <- ifelse(1:nrow(resdf) == refi, NA, resdf$diffAB[refi] - resdf$diffAB)
        if (!is.null(dbaLabels)) {
          leABAB <- calLinkingError(X=resdf$diffAB[refi],
                                    subjectI=uniqueSubject, 
                                    gradeI=uniqueGrade, 
                                    dependentVarI=variable, 
                                    statElementI=gapStat,
                                    gapI=TRUE)[j]
          resdf$diffABABse <- ifelse(1:nrow(resdf) == refi, NA, 
                                   ifelse(resdf$labels %in% dbaLabels, 
                                          sqrt(resdf$diffABse[refi]^2 + resdf$diffABse^2 - 2 * resdf$covABAB),
                                          sqrt(resdf$diffABse[refi]^2 + resdf$diffABse^2 - 2 * resdf$covABAB + leABAB)))
        } else {
          resdf$diffABABse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$diffABse[refi]^2 + resdf$diffABse^2 - 2 * resdf$covABAB))
        }
        resdf$diffABABpValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffABAB/resdf$diffABABse), df=resdf$dofABAB)))
      }
      
      # only generate these 
      if(!nocovs) {
        pctdf0$diffAA <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$pctA[refi] - pctdf0$pctA)
        pctdf0$diffAAse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$pctAse[refi]^2 + pctdf0$pctAse^2 - 2 * pctdf0$covAA))
        pctdf0$diffAApValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffAA/pctdf0$diffAAse), df=pctdf0$dofAA)))
        if (!skipB) {
          pctdf0$diffBB <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$pctB[refi] - pctdf0$pctB)
          pctdf0$diffBBse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$pctBse[refi]^2 + pctdf0$pctBse^2 - 2 * pctdf0$covBB))
          pctdf0$diffBBpValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffBB/pctdf0$diffBBse), df=pctdf0$dofBB)))
          pctdf0$diffABAB <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$diffAB[refi] - pctdf0$diffAB)
          pctdf0$diffABABse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$diffABse[refi]^2 + pctdf0$diffABse^2 - 2 * pctdf0$covABAB))
          pctdf0$diffABABpValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffABAB/pctdf0$diffABABse), df=pctdf0$dofABAB)))
        }
      }
      if(j==1) {
        resdf0 <- resdf
      } else {
        resdf0 <- rbind(resdf0, resdf)
      }
    } # end for(j in 1:lstats) loop
    if(nocovs) {
      # remove the labels columns which are just populated
      # with the unhelpful text "data"
      pctdf0$labels <- NULL
      resdf0$labels <- NULL
      # reorder columns when there are no covariates
      if(returnSimpleDoF) {
        if (!skipB) {
          resvar0 <- c("estimateA", "estimateAse", "dofA",
                       "estimateB", "estimateBse", "dofB")
        } else {
          resvar0 <- c("estimateA", "estimateAse", "dofA")
        }
        
      } else {
        if (!skipB) {
          resvar0 <- c("estimateA", "estimateAse",
                       "estimateB", "estimateBse")
        } else {
          resvar0 <- c("estimateA", "estimateAse")
        }
        
      }
      if (!skipB) {
        resvar0 <- c(resvar0,
                     "diffAB", "covAB",
                     "diffABse", "diffABpValue", "dofAB")
      }
      addons <- c("achievementLevel", "percentiles")
      resvar <- c(addons[addons %in% names(resdf0)], resvar0)
      if("sameSurvey" %in% names(resdf0) && sum(!is.na(resdf0$sameSurvey)) > 0) {
        resvar <- c(resvar, "sameSurvey")
      }
      resdf0 <- resdf0[,resvar]
      # remove "estimate" with "pct" where "estimate" must start the variable name
      resvar <- gsub("^estimate", "pct", resvar0)
      if(returnSimpleN) {
        if (!skipB) {
          resvar <- c(resvar, "nA", "nB")
        } else {
          resvar <- c(resvar, "nA")
        }
        
      }
      if(returnNumberOfPSU) {
        if (!skipB) {
          resvar <- c(resvar, "nPSUA", "nPSUB")
        } else {
          resvar <- c(resvar, "nPSUA")
        }
        
      }
      pctdf0 <- pctdf0[,resvar]
    } else { # end  if(nocovs)
      # reorder columns when there are covs
      if(returnSimpleDoF) {
        if (!skipB) {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse", "dofA",
                       "estimateB", "estimateBse", "dofB")
        } else {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse", "dofA")
        }
        
      } else {
        if (!skipB) {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse",
                       "estimateB", "estimateBse")
        } else {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse")
        }
        
      }
      if (!skipB) {
        resvar0 <- c(resvar0,
                     "diffAB", "covAB", "diffABse",
                     "diffABpValue", "dofAB",
                     "diffAA", "covAA", "diffAAse",
                     "diffAApValue", "dofAA",
                     "diffBB", "covBB", "diffBBse",
                     "diffBBpValue", "dofBB",
                     "diffABAB", "covABAB", "diffABABse",
                     "diffABABpValue", "dofABAB")
      } else {
        resvar0 <- c(resvar0,
                     "diffAA", "covAA", "diffAAse",
                     "diffAApValue", "dofAA")
      }   
      addons <- c("achievementLevel", "percentiles")
      resvar <- c(addons[addons %in% names(resdf0)], resvar0)
      if("sameSurvey" %in% names(resdf0) && sum(!is.na(resdf0$sameSurvey)) > 0) {
        resvar <- c(resvar, "sameSurvey")
      }
      resdf0 <- resdf0[,resvar]
      resvar <- gsub("^estimate", "pct", resvar0)
      missing <- resvar[!resvar %in% names(pctdf0)] 
      if(returnSimpleN) {
        if (!skipB) { resvar <- c(resvar, "nA", "nB") } else {resvar <- c(resvar,"nA")}
      }
      pctdf0 <- pctdf0[,resvar]
    } # end else  if(nocovs)
    res[["results"]] <- resdf0
    res[["percentage"]] <- pctdf0
    res[["call"]] <- call
    class(res) <- "gapList"
    return(res)        
    
    
  } else { # end if(inherits(data, "edsurvey.data.frame.list"))
    calli <- as.list(call)
    calli[[1]] <- as.name("gapHelper")
    calli$data <- data
    resi <- eval(as.call(calli))
    resi$call <- call
    return(resi)
  } # end else for if(inherits(data, "edsurvey.data.frame.list")) {
}


# this is called when data is edsurvey.data.frame or light.edsurvey.data.frame
gapHelper <- function(variable, data, groupA = "default", groupB = "default",
                      percentiles=NULL,
                      achievementLevel=NULL,
                      achievementDiscrete=FALSE,
                      stDev=FALSE,
                      targetLevel=NULL,
                      weightVar=NULL, jrrIMax=1,
                      omittedLevels=TRUE,
                      defaultConditions=TRUE,
                      recode=NULL,
                      referenceDataIndex=1,
                      returnVarEstInputs=FALSE,
                      returnSimpleDoF=FALSE,
                      returnSimpleN=FALSE,
                      returnNumberOfPSU=FALSE,
                      noCov=FALSE,
                      pctMethod=c("unbiased", "symmetric", "simple"),
                      varMethod=NULL,
                      includeLinkingError=NULL) {
  if(is.character(substitute(groupA))) {
    groupA <- parse(text=groupA)[[1]]
  }
  if (all(as.character(substitute(groupA)) == 'default')) {
    groupB <- NULL
  }
  if(!missing(groupB)) {
    if(is.character(substitute(groupB))) {
      groupB <- parse(text=groupB)[[1]]
    }
  }
  pctMethod <- match.arg(pctMethod)
  # get the weight var
  # if the weight var is not set, use the default
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(data, "weights"))$default
  } else {
    wgt <- weightVar
  } # End of if/else: is.null(weightVar)
  if(min(nchar(wgt)) == 0) {
    # no weight
    stop(paste0("There is no default weight variable for ",
                getAttributes(data,"survey")," data, so the argument ",sQuote("weightVar"), 
                " must be specified."))
  }
  
  varEstInputs <- NULL
  type <- "mu" # mean is the default
  if(!is.null(percentiles)) {
    type <- "pct" 
  }
  if(!is.null(achievementLevel)) {
    if(type != "mu") {
      stop(paste0("Only one of ", sQuote("percentiles"), ", ", sQuote("achievementLevel"), 
                  ", ", sQuote("SD"), ", and ", sQuote("targetLevel"), " can be defined."))
    }
    type <- "AL"
    achievementLevel <- gmatchAttr(achievementLevel, getALNames(data, variable))
  }
  if(!is.null(targetLevel)) {
    if(type != "mu") {
      stop(paste0("Only one of ", sQuote("percentiles"), ", ", sQuote("achievementLevel"), 
                  ", ", sQuote("SD"), ", and ", sQuote("targetLevel"), " can be defined."))
    }
    type <- "eq"
  }
  if(stDev) {
    if(type != "mu") {
      stop(paste0("Only one of ", sQuote("percentiles"), ", ", sQuote("achievementLevel"), 
                  ", ", sQuote("SD"), ", and ", sQuote("targetLevel"), " can be defined."))
    }
    type <- "stdev"
  }
  # make the data with just groupA
  if (all(as.character(substitute(groupA)) == "default")) {
    dataA <- data
  } else {
    dataA <- subset(data, substitute(groupA), inside=TRUE)
  }
  # skipB is a boolean to indicate whether we need to compute statistics for group B
  # skipB = TRUE happens when (1) groupB is set to NULL or (2) groupA = 'default' (full sample)
  skipB <- FALSE
  # if groupB is null, just use data as the reference.
  # NOTE: the call to is.null requires "substitute" or groupB
  # will be evaluated right here
  if (is.null(substitute(groupB))) {
    skipB <- TRUE
  } else if(all(as.character(substitute(groupB)) == "default")) {
    dataB <- data
    dataAB <- dataA
  } else {
    # otherwise filter on groupB
    dataB <- subset(data, substitute(groupB), inside=TRUE)
    dataAB <- subset(dataA, substitute(groupB), inside=TRUE)
  }
  callv <- list(weightVar=weightVar,
                jrrIMax=jrrIMax,
                omittedLevels=omittedLevels,
                recode=NULL) # recode.sdf is already done in line 335
  if(!missing(defaultConditions)) {
    callv <- c(callv, list(defaultConditions=defaultConditions))
  }
  if(type == "mu") {
    # gap in mean
    calllm <- c(list(formula(paste0(variable, " ~ 1"))),
                callv,
                list(returnVarEstInputs=TRUE)) # necessary for covariance est
    calllmA <- c(calllm, list(data=dataA))
    meanA <- do.call(lm.sdf, calllmA)
    coefA <- meanA$coefmat
    resA <- coefA[1,1]
    if (!skipB) {
      calllmB <- c(calllm, list(data=dataB))
      meanB <- do.call(lm.sdf, calllmB)
      diff <- unname(meanA$coef[1] - meanB$coef[1]) # unname removes the name (Intercept) that otherwise ends up in the retruned value
      coefB <- meanB$coefmat
      resB <- coefB[1,1]
      SEs <- list(estimateAse=coefA[1,2], estimateBse=coefB[1,2])
    } else {
      SEs <- list(estimateAse=coefA[1,2])
    }
    
    # prepare varEstInputs
    # make varEstInputs$JK
    maJK <- subset(meanA$varEstInputs$JK, variable=="(Intercept)")
    maJK$variable <- "A"
    if (!skipB) {
      mbJK <- subset(meanB$varEstInputs$JK, variable=="(Intercept)")
      mbJK$variable <- "B"
      # calculate the difference
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    # make varEstInputs$PV
    if(!is.null(meanA$varEstInput$PV)) {
      maPV <- subset(meanA$varEstInputs$PV, variable=="(Intercept)")
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$varEstInputs$PV, variable=="(Intercept)")
        mbPV$variable <- "B"
        # calculate the difference
        mdPV <- merge(maPV, mbPV, by=c("PV"), suffixes=c(".A", ".B"))
        mdPV$variable <- "A-B"
        mdPV$value <- mdPV$value.A - mdPV$value.B
        mdPV$value.A <- mdPV$value.B <- NULL
        mdPV$variable.A <- mdPV$variable.B <- NULL
        varEstPV <- rbind(maPV, mbPV, mdPV)
      } else {
        # only group A
        varEstPV <- maPV
      }
    } else { # end if(!is.null(meanA$varEstInput$PV))
      # no PVs, so no error from them
      varEstPV <- NULL
    } # end else for if(!is.null(meanA$varEstInput$PV))
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  } # end if(type == "mu")
  if(type == "pct") {
    # gap at a percentile
    # here the length of percentiles is always one
    callpct <- c(list(variable=variable,
                      percentiles=percentiles,
                      confInt=FALSE,# do not share CI, so do not calculate it
                      pctMethod=pctMethod), 
                 callv,
                 returnVarEstInputs=TRUE)
    callpctA <- c(callpct, list(data=dataA))
    meanA <- do.call(percentile, callpctA)
    resA <- meanA$estimate
    statisticsLevelOut <- paste0("P",percentiles)
    if (!skipB) {
      callpctB <- c(callpct, list(data=dataB))
      meanB <- do.call(percentile, callpctB)
      resB <- meanB$estimate
      SEs <- list(estimateAse=meanA$se, estimateBse=meanB$se)
    } else {
      SEs <- list(estimateAse = meanA$se)
    }
    maJK <- attributes(meanA)$varEstInputs$JK
    maJK$Level <- maJK$variable
    maJK$variable <- "A"
    if (!skipB) {
      mbJK <- attributes(meanB)$varEstInputs$JK
      mbJK$Level <- mbJK$variable
      mbJK$variable <- "B"
      # calculate the difference
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate","Level"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
  
    # make varEstInputs$PV
    maPV <- attributes(meanA)$varEstInputs$PV
    maPV$Level <- maPV$variable
    maPV$variable <- "A"
  
    if (!skipB) {
      mbPV <- attributes(meanB)$varEstInputs$PV
      mbPV$Level <- mbPV$variable
      mbPV$variable <- "B"
      # calculate the difference
      mdPV <- merge(maPV, mbPV, by=c("PV","Level"), suffixes=c(".A", ".B"))
      mdPV$variable <- "A-B"
      mdPV$value <- mdPV$value.A - mdPV$value.B
      mdPV$value.A <- mdPV$value.B <- NULL
      mdPV$variable.A <- mdPV$variable.B <- NULL
      varEstPV <- rbind(maPV, mbPV, mdPV)
    } else {
      varEstPV <- maPV
    }
  
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  } # end if (type == 'pct')
  if(type == "AL") {
    # gap in percent at or above an achievement level
    callal <- c(list(achievementVars=variable,
                     returnVarEstInputs=TRUE,
                     returnCumulative = !achievementDiscrete),
                callv)
    als <- getALNames(data, variable)
    callalA <- c(callal, list(data=dataA))
    meanA <- do.call(achievementLevels, callalA)
    if(!is.null(meanA$discrete)) {
      colnames(meanA$discrete)[grepl("Level$", colnames(meanA$discrete))] <- "Level"
    }
    if(!is.null(meanA$cumulative)) {
      colnames(meanA$cumulative)[grepl("Level$", colnames(meanA$cumulative))] <- "Level"
    }
    if (!skipB) {
      callalB <- c(callal, list(data=dataB))
      meanB <- do.call(achievementLevels, callalB)
      if(!is.null(meanB$discrete)) {
        colnames(meanB$discrete)[grepl("Level$", colnames(meanB$discrete))] <- "Level"
      }
      if(!is.null(meanB$cumulative)) {
        colnames(meanB$cumulative)[grepl("Level$", colnames(meanB$cumulative))] <- "Level"
      }
    }
    # achievementDiscrete indicates whether we should extract cumulative or discrete achievement level results
    if (achievementDiscrete){
      # get index of desired achievement levels from the results
      if (typeof(als) == "character") {
        lA <- sapply(achievementLevel, function(al) {
          lal <- addALPrefix(al=al, als=als, discrete=TRUE)
          grep(lal, meanA$discrete$Level, ignore.case = TRUE)
        })
      } else {
        lA <- rep(TRUE, length(als[[achievementLevel]]))
      }

      if (length(lA) == 0) {
        stop("Achievement level cannot be found in the results of the call of the function ", sQuote("achievementLevel"), ".")
      }
      if (!skipB) {
        if (typeof(als) == "character") {
          lB <- sapply(achievementLevel, function(al) {
            lal <- addALPrefix(al=al, als=als, discrete=TRUE)
            grep(lal, meanB$discrete$Level, ignore.case = TRUE)
          })} else {
            lB <- rep(TRUE, length(als[[achievementLevel]]))
          }
        if (length(lB) == 0) {
          stop("Achievement level cannot be found in the results of the call of the function ", sQuote("achievementLevel"), ".")
        }
      }
      resA <- meanA$discrete$Percent[unlist(lA)]
      if (!skipB) {
        resB <- meanB$discrete$Percent[unlist(lB)]
        SEs <- list(estimateAse=meanA$discrete$StandardError[unlist(lA)],
                    estimateBse=meanB$discrete$StandardError[unlist(lB)])
      } else {
        SEs <- list(estimateAse=meanA$discrete$StandardError[unlist(lA)])
      }
      
    } else {
      # get index of desired achievement levels from the results
      if (typeof(als) == "character") {
        lA <- sapply(achievementLevel, function(al) {
            lal <- addALPrefix(al=al, als=als, discrete=FALSE)
            grep(lal,meanA$cumulative$Level, ignore.case = TRUE)})} 
      else {
          lA <- rep(TRUE, length(als[[achievementLevel]]))
        }
      if (length(lA) == 0) {
        stop("Achievement level cannot be found in the results of the call of the function ", sQuote("achievementLevel"), ".")
      }
      if (!skipB) {
        if (typeof(als) == "character") {
        lB <- sapply(achievementLevel, function(al) {
          lal <- addALPrefix(al=al, als=als, discrete=FALSE)
          grep(lal, meanB$cumulative$Level, ignore.case = TRUE)
        })} else {
          lB <- rep(TRUE, length(als[[achievementLevel]]))
        }
      }
      resA <- meanA$cumulative$Percent[unlist(lA)]
      if (!skipB) {
        resB <- meanB$cumulative$Percent[unlist(lB)]
        SEs <- list(estimateAse=meanA$cumulative$StandardError[unlist(lA)],
                    estimateBse=meanB$cumulative$StandardError[unlist(lB)])
      } else {
        SEs <- list(estimateAse=meanA$cumulative$StandardError[unlist(lA)])
      }
    }
    # used inside next if, but also outside
    statisticsLevelOut <- meanA$discrete$Level[unlist(lA)]
    # get Cov
    if (achievementDiscrete){
      maJK <- subset(meanA$discVarEstInputs$JK, variable %in% meanA$discrete$Level[unlist(lA)])
      maJK$Level <- maJK$variable
      maJK$variable <- "A"
      if (!skipB) {
        mbJK <- subset(meanB$discVarEstInputs$JK, variable %in% meanB$discrete$Level[unlist(lB)])
        mbJK$Level <- mbJK$variable
        mbJK$variable <- "B"
      }
    } # end if(achievementDiscrete)
    else {
      maJK <- subset(meanA$cumVarEstInputs$JK, variable %in% meanA$cumulative$Level[unlist(lA)])
      maJK$Level <- maJK$variable
      maJK$variable <- "A"
      if (!skipB) {
        mbJK <- subset(meanB$cumVarEstInputs$JK, variable %in% meanB$cumulative$Level[unlist(lB)])
        mbJK$Level <- mbJK$variable
        mbJK$variable <- "B"
      } 
    } # end else if(achievementDiscrete)
    if (!skipB) {
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate","Level"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    # make varEstInputs$PV
    if (achievementDiscrete){
      maPV <- subset(meanA$discVarEstInputs$PV, variable %in% meanA$discrete$Level[unlist(lA)])
      maPV$Level <- maPV$variable
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$discVarEstInputs$PV, variable %in% meanB$discrete$Level[unlist(lB)])
        mbPV$Level <- mbPV$variable
        mbPV$variable <- "B"
      }  
    } # end if(achievementDiscrete)
    else {
      statisticsLevelOut <- meanA$cumulative$Level[unlist(lA)]
      maPV <- subset(meanA$cumVarEstInputs$PV, variable %in% meanA$cumulative$Level[unlist(lA)])
      maPV$Level <- maPV$variable
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$cumVarEstInputs$PV, variable %in% meanB$cumulative$Level[unlist(lB)])
        mbPV$Level <- mbPV$variable
        mbPV$variable <- "B"
      }
    } # end else if(achievementDiscrete)
    # calculate the difference
    if (!skipB) {
      mdPV <- merge(maPV, mbPV, by=c("PV","Level"), suffixes=c(".A", ".B"))
      mdPV$variable <- "A-B"
      mdPV$value <- mdPV$value.A - mdPV$value.B
      mdPV$value.A <- mdPV$value.B <- NULL
      mdPV$variable.A <- mdPV$variable.B <- NULL
      varEstPV <- rbind(maPV, mbPV, mdPV)
    } else {
      varEstPV <- maPV
    }
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  } # end if (type == "AL")
  if(type == "stdev") {
    # gap at a standard deviation
    # here the length of standard deviation is always one
    callStDev <- c(list(variable=variable), 
                   callv,
                   returnVarEstInputs=TRUE)
    if("weightVar" %in% names(callStDev) && is.null(callStDev[["weightVar"]])) {
      # default is used only if weightVar is missing.
      callStDev[["weightVar"]] <- NULL
    }
    callStDevA <- c(callStDev, list(data=dataA))
    meanA <- do.call(SD, callStDevA)
    resA <- meanA$std
    statisticsLevelOut <- "SD"
    if (!skipB) {
      callStDevB <- c(callStDev, list(data=dataB))
      meanB <- do.call(SD, callStDevB)
      resB <- meanB$std
      SEs <- list(estimateAse=meanA$stdSE, estimateBse=meanB$stdSE)
    } else {
      SEs <- list(estimateAse = meanA$stdSE)
    }
    maJK <- meanA$varEstInputs$JK
    maJK$Level <- maJK$variable
    maJK$variable <- "A"
    if (!skipB) {
      mbJK <- meanB$varEstInputs$JK
      mbJK$Level <- mbJK$variable
      mbJK$variable <- "B"
      # calculate the difference
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate","Level"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    
    # make varEstInputs$PV
    maPV <- meanA$varEstInputs$PV
    maPV$Level <- maPV$variable
    maPV$variable <- "A"
    
    if (!skipB) {
      mbPV <- meanB$varEstInputs$PV
      mbPV$Level <- mbPV$variable
      mbPV$variable <- "B"
      # calculate the difference
      mdPV <- merge(maPV, mbPV, by=c("PV","Level"), suffixes=c(".A", ".B"))
      mdPV$variable <- "A-B"
      mdPV$value <- mdPV$value.A - mdPV$value.B
      mdPV$value.A <- mdPV$value.B <- NULL
      mdPV$variable.A <- mdPV$variable.B <- NULL
      varEstPV <- rbind(maPV, mbPV, mdPV)
    } else {
      varEstPV <- maPV
    }
    
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  } # end if (type == 'stDev')
  groupA_0 <- FALSE
  groupB_0 <- FALSE
  if(type == "eq") {
    # gap in percent in some covariate
    calleq <- c(callv, list(formula=formula(paste0(" ~ ", variable)),
                            returnMeans=FALSE,
                            pctAggregationLevel = NULL,
                            returnSepct = TRUE,
                            drop = FALSE,
                            returnVarEstInputs=TRUE))
    calleqA <- c(calleq, list(data=dataA))
    meanA <- do.call(edsurveyTable, calleqA)
    
    if (!targetLevel %in% meanA$data[[variable]]) {
      groupA_0 <- TRUE
    }
    resA <- ifelse(groupA_0,0,meanA$data[meanA$data[variable] == targetLevel,4])
    if (!skipB) {
      calleqB <- c(calleq, list(data=dataB))
      meanB <- do.call(edsurveyTable, calleqB)
      if (!targetLevel %in% meanB$data[[variable]]) {
        groupB_0 <- TRUE
      }
      resB <- ifelse(groupB_0,0,meanB$data[meanB$data[variable] == targetLevel,4])
      SEs <- list(estimateAse=ifelse(groupB_0,NA,meanA$data[meanA$data[variable] == targetLevel,5]),
                  estimateBse=ifelse(groupB_0,NA,meanB$data[meanB$data[variable] == targetLevel,5]))
    } else {
      SEs <- list(estimateAse=ifelse(groupB_0,NA,meanA$data[meanA$data[variable] == targetLevel,5]))
    }
    
    # make varEstInputs$JK
    if (!groupA_0) {
      maJK <- meanA$pctVarEstInputs$JK[meanA$pctVarEstInputs$JK$variable == paste0(variable,"=", targetLevel),]
      maJK$variable <- "A"
    } else  {
      maJK <- NULL
    }
    if (!skipB) {
      if (!groupB_0) {
        mbJK <- meanB$pctVarEstInputs$JK[meanB$pctVarEstInputs$JK$variable == paste0(variable,"=", targetLevel),]
        mbJK$variable <- "B"
      } else {
        mbJK <- NULL
      }
      # calculate the difference
      if (!groupA_0 & !groupB_0) {
        mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate"), suffixes=c(".A", ".B"))
        mdJK$variable <- "A-B"
        mdJK$value <- mdJK$value.A - mdJK$value.B
        mdJK$value.A <- mdJK$value.B <- NULL
        mdJK$variable.A <- mdJK$variable.B <- NULL
      } else {
        mdJK <- NULL
      }
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    
    # make varEstInputs
    if (!is.null(varEstJK)) {
      row.names(varEstJK) <- NULL
      varEstInputs <- list(JK=varEstJK, PV=NULL)
    } else {
      varEstInputs <- NULL
    }
    
  } # end if (type == "eq")
  # Compute difference statistics (diffAB, covAB, pooledse, pooleddf)
  if (!skipB) {
    diff <- unname(resA - resB) # unname removes the name (Intercept) that otherwise ends up in the retruned value
    nSize <- c(nrow(dataA),nrow(dataB))
    total <- which.max(nSize) # total is 1 if groupA is the total and 2 if groupB is the total
    p <- nrow(dataAB)/nSize[total]
    cov <- p * (SEs[[total]])^2
    wgtl <- getAttributes(data, "weights")[[wgt]]
    JRdfA <- JRdfB <- JRdf <- length(wgtl$jksuffixes)
    if(!is.null(varEstInputs)) {
      if (type %in% c("pct","AL")) {
        cov <- c()
        JRdfA <- c()
        JRdfB <- c()
        JRdf <- c()
        # we need to calculate cov by each Level
        for (si in statisticsLevelOut) {
          varEstInputsTemp <- list(JK=subset(varEstInputs$JK, Level %in% si),
                                   PV=subset(varEstInputs$PV, Level %in% si))
          if (nrow(varEstInputsTemp$JK) == 0) {
            varEstInputsTemp$JK <- NULL
          }
          if (nrow(varEstInputsTemp$PV) == 0) {
            varEstInputsTemp$PV <- NULL
          }
          if (!is.null(varEstInputsTemp$JK)) {
            cov <- c(cov, varEstToCov(varEstInputsTemp, 
                                      varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier")))
            
            JRdfA <- c(JRdfA,DoFCorrection(varEstInputsTemp, varA="A", method=c("JR")))
            JRdfB <- c(JRdfB,DoFCorrection(varEstInputsTemp, varA="B", method=c("JR")))
            JRdf <- c(JRdf,DoFCorrection(varEstInputsTemp, varA="A", varB="B", method=c("JR")))   
          } # end if (!is.null(varEstInputsTemp$JK))
          else {
          	cov <- c(cov, NA)
          	JRdfA <- c(JRdfA, NA)
          	JRdfB <- c(JRdfB, NA)
          	JRdf <- c(JRdf, NA)
          }
        }
      } #end if (type %in% c("pct","AL")) 
      else {
        cov <- ifelse(groupA_0 | groupB_0, cov, varEstToCov(varEstInputs, varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier")))
        JRdfA <- ifelse(groupA_0, NA, DoFCorrection(varEstInputs, varA="A", method=c("JR")))
        JRdfB <- ifelse(groupB_0, NA, DoFCorrection(varEstInputs, varA="B", method=c("JR")))
        JRdf <- ifelse(groupA_0 | groupB_0, NA, DoFCorrection(varEstInputs, varA="A", varB="B", method=c("JR"))) 
      }
      
    } # end if (!is.null(varEstInputs))
    if(noCov) {
      cov <- 0*cov
      JRdf <- ifelse(groupA_0 | groupB_0, NA, (SEs[[1]]^2 + SEs[[2]]^2)^2 / (SEs[[1]]^4/JRdfA + SEs[[2]]^4/JRdfB)) 
    }
    diffSE <- mapply(function(a,b,c) {
      unname(sqrt(a^2 + b^2 - 2 * c))
    }, SEs[[1]],SEs[[2]],cov)
    diffSE <- unlist(diffSE)
    # get percent in each group
    
    # get variables used in conditions for groupA and groupB
    vn <- c(parseVars(substitute(groupA), data), parseVars(substitute(groupB), data))
    # add weights and variable itself
    vn <- c(vn, wgt, variable)
    
    if (returnNumberOfPSU){
      # Get stratum and PSU variable
      stratumVar <- getAttributes(data,"stratumVar")
      psuVar <- getAttributes(data,"psuVar")
      if (all(c(stratumVar, psuVar) %in% names(data)) | all(c(stratumVar, psuVar) %in% colnames(data))) { #names(data$data) changed to colnames(data)::Tom
        vn <- unique(c(vn, stratumVar, psuVar))
      } else {
        warning("Stratum and PSU variable are required for this call and are not on the incoming data. Ignoring returnNumberOfPSU=TRUE.")
        returnNumberOfPSU <- FALSE
      }
    }
    
    # setup non-data call variables    
    callv <- list(varnames=vn,
                  drop=FALSE,
                  omittedLevels=omittedLevels,
                  recode=NULL) # recode is already done in line 335
    if(!missing(defaultConditions)) {
      callv <- c(callv, list(defaultConditions=defaultConditions))
    }
    
    # add weight call variables and do calls
    wgtcall0 <- c(callv, list(data=data))
    d0 <- do.call(getData, wgtcall0)
    wgtcallA <- c(callv, list(data=dataA))
    dA <- do.call(getData, wgtcallA)
    wgtcallB <- c(callv, list(data=dataB))
    dB <- do.call(getData, wgtcallB)
    pctA <- sum(dA[,wgt]) / sum(d0[,wgt])
    pctB <- sum(dB[,wgt]) / sum(d0[,wgt])
    # calculate covAB using JK method
    wgtl <- getAttributes(data, "weights")[[wgt]]
    pctJK <- t(sapply(wgtl$jksuffixes, function(suffix) {
      w0 <- sum(d0[,paste0(wgtl$jkbase, suffix)])
      wA <- sum(dA[,paste0(wgtl$jkbase, suffix)])
      wB <- sum(dB[,paste0(wgtl$jkbase, suffix)])
      c(pctA=wA/w0, pctB=wB/w0)
    }))
    seA <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,1] - pctA)^2 ))
    seB <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,2] - pctB)^2 ))
    varJK <- data.frame(stringsAsFactors=FALSE,
                        PV=rep(0, 3*nrow(pctJK)),
                        JKreplicate=rep(1:nrow(pctJK),3),
                        variable=rep(c("A", "B", "A-B"), each=nrow(pctJK)),
                        value=c(pctJK[,1] - pctA,
                                pctJK[,2] - pctB,
                                (pctJK[,1] - pctA) - (pctJK[,2] - pctB)
                        )
    )
    # there is no PV here because weights are not imputed
    pctVarEstInputs <- list(JK=varJK, PV=NULL)
    if(noCov) {
      covAB <- rep(0, length(seA))
      pctDoFA <- DoFCorrection(pctVarEstInputs, varA="A", method="JR")
      pctDoFB <- DoFCorrection(pctVarEstInputs, varA="B", method="JR")
      # Satterthwaite approximation
      pctJRdf <- (seA^2 + seB^2)^2 / (seA^4/pctDoFA + seB^4/pctDoFB)
    } else {
      covAB <- varEstToCov(pctVarEstInputs, varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier"))
      pctDoFA <- DoFCorrection(pctVarEstInputs, varA="A", method="JR")
      pctDoFB <- DoFCorrection(pctVarEstInputs, varA="B", method="JR")
      pctJRdf <- DoFCorrection(pctVarEstInputs, varA="A-B", method="JR")
    }
    # end covAB calculation
    pctA <- 100 * pctA
    pctB <- 100 * pctB
    seAB <- unname(sqrt(seA^2 + seB^2 - 2 * 100 * 100 * covAB))
    
    # start return vector, first name achievement level
    vec <- switch(type,
                  pct=data.frame(stringsAsFactors=FALSE, percentiles=as.numeric(percentiles)),
                  AL=data.frame(stringsAsFactors=FALSE, achievementLevel=gsub("Below","below",as.character(statisticsLevelOut))),
                  data.frame())
    if(returnSimpleDoF) {
      # add dofA and dofB if requested
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(stringsAsFactors=FALSE,
                                estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA,
                                estimateB=resB, estimateBse=SEs[[2]], dofB=JRdfB))
      } else { # this is equivalent to  if (type %in%  c("mu","eq"))
        vec <- data.frame(stringsAsFactors=FALSE,
                          estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA,
                          estimateB=resB, estimateBse=SEs[[2]], dofB=JRdfB)
      }
    } # end if(returnSimpleDoF) 
    else {
      # otherwise, no dofA or dofB
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(stringsAsFactors=FALSE,
                                estimateA=resA, estimateAse=SEs[[1]],
                                estimateB=resB, estimateBse=SEs[[2]]))
      } else { # this is equivalent to  if (type %in%  c("mu","eq"))
        vec <- data.frame(stringsAsFactors=FALSE,
                          estimateA=resA, estimateAse=SEs[[1]],
                          estimateB=resB, estimateBse=SEs[[2]])
      }
    }
    diffp <- 2*(1-pt2(abs(diff/diffSE), df=JRdf))
    vec <- cbind(vec,
                 data.frame(stringsAsFactors=FALSE,
                            diffAB=diff, covAB=unname(cov),
                            diffABse= diffSE,
                            diffABpValue=diffp,
                            dofAB=JRdf))
    row.names(vec) <- NULL
    if(returnSimpleDoF) {
      percentage <- data.frame(stringsAsFactors=FALSE,
                               pctA=pctA,
                               pctAse=seA,
                               dofA=pctDoFA,
                               pctB=pctB,
                               pctBse=seB,
                               dofB=pctDoFB)
    } else {
      percentage <- data.frame(stringsAsFactors=FALSE,
                               pctA=pctA,
                               pctAse=seA,
                               pctB=pctB,
                               pctBse=seB)
    }
    pdiffp <- 2*(1-pt2(abs(pctA - pctB)/seAB, df=pctJRdf))
    percentage <- cbind(percentage,
                        data.frame(stringsAsFactors=FALSE,
                                   diffAB=pctA - pctB,
                                   covAB=100*100*covAB,
                                   diffABse=seAB,
                                   diffABpValue=pdiffp,
                                   dofAB=pctJRdf))
    
    lst <- list(results=vec,
                labels=list(A=substitute(groupA),B=substitute(groupB),
                            n0A=nrow2.edsurvey.data.frame(dataA),
                            n0B=nrow2.edsurvey.data.frame(dataB),
                            nUsedA=nrow(dA),
                            nUsedB=nrow(dB)
                           ),
                percentage=percentage)
    if (returnNumberOfPSU) {
      lst$labels$nPSUA <- nrow(unique(dA[,c(stratumVar,psuVar)]))
      lst$labels$nPSUB <- nrow(unique(dB[,c(stratumVar,psuVar)]))
    }
  } else { # end if(!skipB)
    wgtl <- getAttributes(data,"weights")[[wgt]]
    JRdfA <- ifelse(groupA_0,NA,length(wgtl$jksuffixes))
    if (!is.null(varEstInputs)) {
      if (type %in% c("pct","AL")) {
        JRdfA <- c()
        for (si in statisticsLevelOut) {
          varEstInputsTemp <- list(JK=subset(varEstInputs$JK, Level %in% si),
                                   PV=subset(varEstInputs$PV, Level %in% si))
          if (nrow(varEstInputsTemp$JK) == 0) {
            varEstInputsTemp$JK <- NULL
          }
          if (nrow(varEstInputsTemp$PV) == 0) {
            varEstInputsTemp$PV <- NULL
          }
          if (!is.null(varEstInputsTemp$JK)) {
            JRdfA <- c(JRdfA,DoFCorrection(varEstInputsTemp, varA="A", method=c("JR")))
          } else {
          	JRdfA <- c(JRdfA, NA)
          }
        }
      } #end if (type %in% c("pct","AL")) 
      else {
        JRdfA <- ifelse(groupA_0,NA,DoFCorrection(varEstInputs, varA="A", method=c("JR")))
      }
    }
    # get variables used in conditions for groupA and groupB
    vn <- c(parseVars(substitute(groupA), data), parseVars(substitute(groupB), data))
    # add weights and variable itself
    vn <- c(vn, wgt, variable)
    if (returnNumberOfPSU){
      # Get stratum and PSU variable
      stratumVar <- getAttributes(data,"stratumVar")
      psuVar <- getAttributes(data,"psuVar")
      if (all(c(stratumVar, psuVar) %in% names(data)) | all(c(stratumVar, psuVar) %in% names(data$data))) {
        vn <- unique(c(vn, stratumVar, psuVar))
      } else {
        warning("Stratum and PSU variable are required for this call and are not on the incoming data. Ignoring returnNumberOfPSU=TRUE.")
        returnNumberOfPSU <- FALSE
      }
    }
    # setup non-data call variables    
    callv <- list(varnames=vn,
                  drop=FALSE,
                  omittedLevels=omittedLevels,
                  recode=NULL) # recode is already done in line 335
    if(!missing(defaultConditions)) {
      callv <- c(callv, list(defaultConditions=defaultConditions))
    }
    
    # add weight call variables and do calls
    wgtcall0 <- c(callv, list(data=data))
    d0 <- do.call(getData, wgtcall0)
    wgtcallA <- c(callv, list(data=dataA))
    dA <- do.call(getData, wgtcallA)
    pctA <- sum(dA[,wgt]) / sum(d0[,wgt])
    pctJK <- t(sapply(wgtl$jksuffixes, function(suffix) {
      w0 <- sum(d0[,paste0(wgtl$jkbase, suffix)])
      wA <- sum(dA[,paste0(wgtl$jkbase, suffix)])
      c(pctA=wA/w0, pctB = 0)
    }))
    seA <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,1] - pctA)^2 ))
    varJK <- data.frame(stringsAsFactors=FALSE,
                        PV=rep(0, nrow(pctJK)),
                        JKreplicate=1:nrow(pctJK),
                        variable="A",
                        value= pctJK[,1] - pctA
    )
    # there is no PV here because weights are not imputed
    pctVarEstInputs <- list(JK=varJK, PV=NULL)
    pctDoFA <- DoFCorrection(pctVarEstInputs, varA="A", method="JR")
    pctA <- 100 * pctA
    # start return vector, first name achievement level
    vec <- switch(type,
                  pct=data.frame(stringsAsFactors=FALSE, percentiles=as.numeric(percentiles)),
                  AL=data.frame(stringsAsFactors=FALSE, achievementLevel=gsub("Below","below",as.character(statisticsLevelOut))),
                  data.frame())
    if(returnSimpleDoF) {
      # add dofA and dofB if requested
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(stringsAsFactors=FALSE, estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA))
      } else {
        vec <- data.frame(stringsAsFactors=FALSE, estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA)
      }
    } else {
      # otherwise, no dofA or dofB
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(stringsAsFactors=FALSE, estimateA=resA, estimateAse=SEs[[1]]))
        
      } else {
        vec <- data.frame(stringsAsFactors=FALSE, estimateA=resA, estimateAse=SEs[[1]])
      }
    }
    row.names(vec) <- NULL
    if(returnSimpleDoF) {
      percentage <- data.frame(stringsAsFactors=FALSE,
                               pctA=pctA,
                               pctAse=seA,
                               dofA=pctDoFA)
    } else {
      percentage <- data.frame(stringsAsFactors=FALSE,
                               pctA=pctA,
                               pctAse=seA)
      
    }
    
    lst <- list(results=vec,
                labels=list(A=substitute(groupA),
                            n0A=nrow2.edsurvey.data.frame(dataA),
                            n0B=NA,
                            nUsedA=nrow(dA),
                            nUsedB=NA
                ),
                percentage=percentage
                )
    if(returnNumberOfPSU) {
      lst$labels$nPSUA <- nrow(dA[,c(stratumVar,psuVar)])
      lst$labels$nPSUB <- NA
    }
  }
  
  if(returnVarEstInputs) {
    if ("Level" %in% colnames(varEstInputs$JK)) { # equivalent to type = 'pct' or 'AL'
      if (type == 'pct') {
        varEstInputs$JK$Level <- as.numeric(gsub("^P","",varEstInputs$JK$Level))
        varEstInputs$PV$Level <- as.numeric(gsub("^P","",varEstInputs$PV$Level))
      }
      if (length(percentiles) <= 1 & length(achievementLevel) <= 1) {
        varEstInputs$JK$Level <- NULL
        varEstInputs$PV$Level <- NULL
      }
    }
    #assign values close to 0 to 0 
    varEstInputs$JK$value[which(abs(varEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(nrow(varEstInputs$JK))))] <- 0 
    pctVarEstInputs$JK$value[which(abs(pctVarEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(nrow(pctVarEstInputs$JK))))] <- 0 
    
    lst <- c(lst, list(varEstInputs=varEstInputs,
                       pctVarEstInputs=pctVarEstInputs))
  }
  class(lst) <- "gap"
  return(lst)
}

# @title Linking error for PISA 2015 and subsequent DBA v prior PBA
# @param years a vector of two years involved, including 2000, 2003, 2006, 2009, 2012, 2015 or 2018
# @param subject, one of "read", "math", or "scei"
# The linking error for PISA is entirely cross year and one value
# @author Paul Bailey
calLinkingErrorPISA <- function(subject=c("read", "math", "scie"),
                                years=c(2000, 2003, 2006, 2009, 2012, 2015, 2018)) {
  years <- as.numeric(years)
  linkingErrorsFinance <- data.frame(subject=rep("fin", 3),
                                     yearSmall=c(2012, 2015, 2012),
                                     yearBig=c(2018, 2018, 2015),
                                     error=c(5.55, 9.37, 5.3309),
                                     stringsAsFactors=FALSE)
  linkingErrorsRead <- data.frame(subject=rep("read", 11),
                                  yearSmall=c(  2000,   2003,   2006,   2009,   2012, 2000, 2003, 2006, 2009, 2012,2015),
                                  yearBig=  c(  2015,   2015,   2015,   2015,   2015, 2018, 2018, 2018, 2018, 2018,2018),
                                  error=    c(6.8044, 5.3907, 6.6064, 3.4301, 5.2535, 4.04, 7.77, 5.24, 3.52, 3.74, 3.93),
                                  stringsAsFactors=FALSE)
  linkingErrorsMath <- data.frame(subject=rep("math", 9),
                                  yearSmall=c(2003,    2006,   2009,   2012, 2003, 2006, 2009, 2012, 2015),
                                  yearBig=  c(2015,    2015,   2015,   2015, 2018, 2018, 2018, 2018, 2018),
                                  error=    c(5.6080, 3.511, 3.7853, 3.5462, 2.80, 3.18, 3.54, 3.34, 2.33),
                                  stringsAsFactors=FALSE)
  linkingErrorsScei <- data.frame(subject=rep("scei", 7),
                                  yearSmall=c(  2006,   2009,   2012, 2006, 2009, 2012, 2015),
                                  yearBig=  c(  2015,   2015,   2015, 2018, 2018, 2018, 2018),
                                  error=    c(4.4821, 4.5016, 3.9228, 3.47, 3.59, 4.01, 1.51),
                                  stringsAsFactors=FALSE)
  linkingErrorsPISA <- rbind(linkingErrorsFinance, linkingErrorsRead, linkingErrorsMath, linkingErrorsScei)
  sbj <- match.arg(subject)
  years <- years[years %in% c(2000, 2003, 2006, 2009, 2012, 2015, 2018)]
  if(length(years) != 2) {
    stop("Argument ", dQuote("years"), " must have 2 valid years so there are two years to contrast.")
  }
  lep <- linkingErrorsPISA[linkingErrorsPISA$subject == sbj, ]
   
  res <- expand.grid(yearSmall = years, yearBig = years)
  res <- res[res$yearSmall < res$yearBig, ]
  res <- merge(lep, res, by=c("yearSmall", "yearBig"), all=FALSE)
  if(nrow(res) == 0) {
    return(0)
  }
  if(nrow(res) > 1) {
    stop("Multiple returns.")
  }
  return(res$error)
}

# @title Linking error for NAEP DBA/PBA gap analysis.
# @param X a numeric value estimated from digitally based assessments
# @param subjectI a character value set to "Mathematics" or "Reading" for subject information
# @param gradeI a character value set to "Grade 4" or "Grade 8" as grade level
# @param gapI  a logical indicating if the linking error is a gap
# @param dependentVarI a character value set to one of NAEP math or reading subscales
# @param statElementI a character vector selected from "Mean", "Row Percent", "10", "25", "50", "75", "90", 
# "At or above basic", "At or above proficient", "Below basic", "At basic", "At proficient", or "At advanced" indicating the relevant statistic
# @author Huade Huo
calLinkingError <- function(X,
                            subjectI=NULL, 
                            gradeI=NULL, 
                            dependentVarI=NULL, 
                            statElementI=NULL,
                            gapI=FALSE) {
  linkingErrorsRDS <- readRDS(system.file("linkingErrors", "linkingErrors.rds", package = "EdSurvey"))
  linkingErrorRDSi <- linkingErrorsRDS[linkingErrorsRDS$subject %in% subjectI &
                                       linkingErrorsRDS$dependentVar %in% dependentVarI & 
                                       linkingErrorsRDS$grade %in% gradeI &
                                       linkingErrorsRDS$statElement %in% statElementI &
                                       linkingErrorsRDS$gap %in% gapI, ]
  linkingErrorRDSi$statElement <- factor(linkingErrorRDSi$statElement, levels = statElementI)
  linkingErrorRDSi <- linkingErrorRDSi[order(linkingErrorRDSi$statElement),]
  if (nrow(linkingErrorRDSi) == 0) {
    stop("Linking error is not avaialble.")
  }
  
  linkingErrori <- linkingErrorRDSi$Aterm * X^2 + linkingErrorRDSi$Bterm * X + linkingErrorRDSi$Cterm
  return(linkingErrori)
}

#' @rdname printGap
#' @title Gap Analysis Printing
#'
#' @description Prints labels and a results vector of a gap analysis.
#' @param x an \code{R} object representing a \code{gap} of class \code{gap} or \code{gapList}
#' @param printPercentage a logical value set to \code{TRUE} to request printing 
#'                        of the percentage in the groups. Defaults to \code{TRUE}.
#' @param ... these arguments are not passed anywhere and are included only for compatibility
#' @method print gap
#' @author Paul Bailey
#' @aliases print.gapList
#' @export
print.gap <- function(x, ..., printPercentage=TRUE) {
  cat("Call: ")
  print(x$call)
  cat("\nLabels:\n")
  lab <- data.frame(stringsAsFactors=FALSE,
                    group=c("A", "B"),
                    definition=c(deparse(x$labels$A), deparse(x$labels$B)),
                    nFullData=c(x$labels$n0A, x$labels$n0B),
                    nUsed=c(x$labels$nUsedA, x$labels$nUsedB))
  if (!is.null(x$labels$nPSUA)) {
    lab$nPSU <- c(x$labels$nPSUA, x$labels$nPSUB)
  }
  print(lab, row.names=FALSE)
  if(printPercentage) {
    cat("\nPercentage:\n")
    print(x$percentage, row.names=FALSE)
  }
  cat("\nResults:\n")
  print(x$results, row.names=FALSE, ...)
}

#' @rdname printGap
#' @method print gapList
#' @export
print.gapList <- function(x, ..., printPercentage=TRUE) {
  cat("gapList\n")
  cat(paste0("Call: "))
  print(x$call)
  cat("\nLabels:\n")
  lab <- data.frame(stringsAsFactors=FALSE,
                    group=c("A", "B"),
                    definition=c(deparse(x$labels$A), deparse(x$labels$B)))
  print(lab, row.names=FALSE)
  if(printPercentage) {
    cat("\nPercentage:\n")
    pct <- data.frame(x$percentage, stringsAsFactors=FALSE)
    print(pct, row.names=FALSE)
  }
  cat("\nResults:\n")
  print(x$results, row.names=FALSE)
}

# helper that identifies variables in a call
# ccall is the call to parse
# x is an edsurvey.data.frame or light.edsurvey.data.frame
# @author Paul Bailey
parseVars <- function(ccall,x) {
  vars <- c()
  # for each element
  if (typeof(ccall) == "symbol") {
    return(vars)
  }
  for(i in 1:length(ccall)) {
    # if it is a name
    if(class(ccall[[i]]) %in% c("name")) {
      ccall_c <- as.character(ccall[[i]])
      # if it is not in the data and is in the parent.frame, it might be a variable.
      if(ccall_c %in% colnames(x) ) {  #orignally:: if(ccall_c %in% names(x$data) ) { TOM FINK::dataList update
        if (ccall[[i]] == "%in%" || is.function(ccall[[i]])) {
          # do nothing
        } else {
          vars <- c(vars, ccall_c)
        } 
      } # End of if statment: if (ccall_c %in% names(x$data))
    } # end if(class(ccall[[i]]) %in% c("name")) {
    if(class(ccall[[i]]) %in% "call") {
      # if this is a call, recursively parse that
      vars <- c(vars, parseVars(ccall[[i]], x))
    } #end of if statment: if class(ccall[[i]]) %in% "call"
  } # end of for loop: i in 1:length(ccall)
  vars
} # End of fucntion: parseVars

# helpter function that calculates "pooled" dof, used for two distinct samples
# @author Trang Nguyen and Paul Bailey
dofCompute <- function(seA,seB,dofA,dofB) {
  return(mapply(function(seA,seB,dofA,dofB) {
    ifelse(seA + seB > 0, (seA^2+seB^2)^2/(seA^4/dofA + seB^4/dofB), 0)
  }, seA,seB,dofA,dofB))
}

# returns NA when df is 0, but passes Info to pt to avoid a warning
# @author Paul Bailey
pt2 <- function(q, df) {
  df2 <- ifelse(df == 0, Inf, df)
  return(ifelse(df == 0, NA, pt(q, df2)))
}

# match strings to attributes in a case insensitive way
# throwing an error if the string matches more than one achievement level
# or matches none
gmatchAttr <- function(strings, achievementLevelNames) {
  s0 <- strings
  alN0 <- achievementLevelNames
  # lower case everyting to make the function case insensitive
  strings <- tolower(trimws(strings))
  alN <- tolower(achievementLevelNames)
  # find and store the prefix, then remove it from strings
  prefix <- ifelse(grepl("^at or above", strings), substr(s0,0,nchar("at or above")+1), "")
  prefix <- ifelse(nchar(prefix) == 0 & grepl("^at ", strings), substr(s0,0,3), prefix)
  prefix <- ifelse(grepl("^below", strings), substr(s0,0,nchar("below")+1), prefix)
  strings <- trimws(substr(strings, nchar(prefix)+1, nchar(strings)))
  out <- rep("", length(strings))
  error <- rep("", length(strings))
  for(i in 1:length(out)) {
    for(j in 1:length(alN)) {
      update <- grepl(strings[i], alN[j], fixed=TRUE)
      if(update) {
        if(nchar(out[i]) == 0) {
          # base case, store the output
          out[i] <- ifelse(update, alN0[j], out[i])
        } else {
          # append this level to previously found levels in an error
          # we can then replay all the levels to the user at the end.
          # first...
          # if this is the second match, store the previous
          # one as an error
          if(nchar(error[i]) == 0) {
            error[i] <- sQuote(out[i])
          }
          error[i] <- paste0(c(error[i], sQuote(alN0[j])), collapse=", ")
          out[i] <- "Error"
        }
      }
    }
  }
  # finalize formatting for errors
  error <- ifelse(nchar(error) > 0, paste0(" matched to ", error), "")
  error <- ifelse(nchar(out) == 0, paste0(" is not one of the achievement Level for the current survey. Choose one from ", pasteItems(achievementLevelNames)), error)
  # if there is an error, format it and throw it
  if(any(nchar(error) > nchar(out))) {
    evars <- nchar(error)>0
    ferror <- paste0(sQuote(s0[evars]), error[evars], collapse="; ")
    stop(paste0(ferror,"."))
  }
  out <- trimws(paste(trimws(prefix), trimws(out), sep=" "))
  return(out)
}

getALNames <- function(data, variable) {
  al <- getAttributes(data,"achievementLevels")
  if(is.list(al)) {
    if(variable %in% names(al)) {
      al <- al[[variable]]
    } else {
      # assumes all achievement levels are the same
      al <- al[[1]]
    }
  }
  names(al)
}

addALPrefix <- function(al, als, discrete) {
  if (discrete) {
    if (grepl("below", al, ignore.case = TRUE)) {
      lal <- al
    } else {
      if(!grepl("^at", al, ignore.case=TRUE)) {
        lal <- paste0("At ",al)
      } else {
        lal <- al
      }
    }
  } else {
    if(grepl("^below", al, ignore.case = TRUE)) { # cumulative should not have Below achievementLevel
      return(NULL)
    } else {
      # if it doesn't have the at prefix, add it
      if(!grepl("^at", al, ignore.case=TRUE)) {
        lal <- paste0(ifelse(tolower(al) == tolower(als[length(als)]),
                             "At ",
                             "At or Above "),
                      al)
      } else {
        lal <- al
      }
    }
  }
  return(lal)
}