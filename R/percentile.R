#' @title EdSurvey Percentiles
#'
#' @description Calculates the percentiles of a numeric variable in an
#'              \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'              or an \code{edsurvey.data.frame.list}.
#'
#' @param variable the character name of the variable to percentiles computed,
#'                 typically a subject scale or subscale
#' @param percentiles a numeric vector of percentiles in the range of 0 to 100
#'                    (inclusive)
#' @param data      an \code{edsurvey.data.frame} or an
#'                  \code{edsurvey.data.frame.list}
#' @param weightVar a character indicating the weight variable to use.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}} 
#'                   term (see 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param varMethod a character set to \code{jackknife} or \code{Taylor}
#'                  that indicates the variance estimation method used when 
#'                  constructing the confidence intervals. The jackknife
#'                  variance estimation method is always
#'                  used to calculate the standard error.
#' @param alpha a numeric value between 0 and 1 indicating the confidence level.
#'              An \code{alpha} value of 0.05 would indicate a 95\% 
#'              confidence interval and is the default.
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of 
#'                      all factor variables that are specified in
#'                      \code{achievementVars} and \code{aggregatBy}. 
#'                      Use \code{print} on an \code{edsurvey.data.frame}
#'                      to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value
#'                          of \code{TRUE}, uses the default 
#'                          conditions stored in an \code{edsurvey.data.frame}
#'                          to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame}
#'                          to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to
#'               \code{NULL}. Can be set as
#'               \code{recode=}\code{list(var1=} \code{list(from=} \code{c("a",}
#'               \code{"b",} \code{"c"),}
#'               \code{to=} \code{"d"))}.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates which allows for the computation
#'                           of covariances between estimates.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of 
#'                          primary sampling units (PSUs)                         
#' @param pctMethod one of \dQuote{unbiased}, \dQuote{symmetric}, \dQuote{simple};
#'                  unbiased produces a weighted median unbiased percentile estimate,
#'                  whereas simple uses a basic formula that matches previously
#'                  published results. Symmetric uses a more basic formula
#'                  but requires that the percentile is symetric to multiplying
#'                  the quantity by negative one.
#' @param confInt a Boolean indicating if the confidence interval should be returned
#' @param dofMethod passed to \code{\link{DoFCorrection}} as the \code{method} argument
#' @details
#' Percentiles, their standard errors, and confidence intervals
#' are calculated according to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#' The standard errors and confidence intervals are based
#' on separate formulas and assumptions.
#'
#' The Taylor series variance estimation procedure is not relevant to percentiles
#' because percentiles are not continuously differentiable.
#' 
#' @return
#' The return type depends on whether the class of the \code{data} argument is an
#' \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}.
#'
#' \strong{The data argument is an edsurvey.data.frame}
#'   When the \code{data} argument is an \code{edsurvey.data.frame},
#'   \code{percentile} returns an S3 object of class \code{percentile}.
#'   This is a \code{data.frame} with typical attributes (\code{names},
#'   \code{row.names}, and \code{class}) and additional attributes as follows:
#'     \item{n0}{number of rows on \code{edsurvey.data.frame} before any conditions were applied}
#'     \item{nUsed}{number of observations with valid data and weights larger than zero}
#'     \item{nPSU}{number of PSUs used in the calculation}
#'     \item{call}{the call used to generate these results}
#'
#'   The columns of the \code{data.frame} are as follows:
#'     \item{percentile}{the percentile of this row}
#'     \item{estimate}{the estimated value of the percentile}
#'     \item{se}{the jackknife standard error of the estimated percentile}
#'     \item{df}{degrees of freedom}
#'     \item{confInt.ci_lower}{the lower bound
#'                      of the confidence interval}
#'     \item{confInt.ci_upper}{the upper bound
#'                      of the confidence interval}
#'     \item{nsmall}{the number of units with more extreme results, averaged
#'                   across plausible values}
#'   When the \code{confInt} argument is set to \code{FALSE}, the confidence
#'   intervals are not returned.
#' 
#' \strong{The data argument is an edsurvey.data.frame.list}
#'   When the \code{data} argument is an \code{edsurvey.data.frame.list},
#'   \code{percentile} returns an S3 object of class \code{percentileList}.
#'   This is a data.frame with a \code{call} attribute.
#'   The columns in the \code{data.frame} are identical to those in the previous
#'   section, but there also are columns from the \code{edsurvey.data.frame.list}.
#'   
#'     \item{covs}{a column for each column in the \code{covs} value of the
#'                 \code{edsurvey.data.frame.list}.
#'                 See Examples.}
#'
#' When \code{returnVarEstInputs} is \code{TRUE}, an attribute
#' \code{varEstInputs} also is returned that includes the variance estimate
#' inputs used for calculating covariances with \code{\link{varEstToCov}}.
#'
#' @references
#' Hyndman, R. J., & Fan, Y. (1996). Sample quantiles in statistical packages. \emph{American Statistician}, \emph{50}, 361--365.
#' @author Paul Bailey
#' @importFrom stats reshape
#' @example man/examples/percentile.R
#' @export
percentile <- function(variable, percentiles, data,
                weightVar=NULL, jrrIMax=1,
                varMethod=c("jackknife", "Taylor"),
                alpha=0.05,
                omittedLevels=TRUE,
                defaultConditions=TRUE,
                recode=NULL,
                returnVarEstInputs=FALSE,
                returnNumberOfPSU=FALSE,
                pctMethod=c("symmetric", "unbiased", "simple"),
                confInt=TRUE,
                dofMethod=c("JR", "WS")
                ) {
  # check incoming variables
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  varMethod <- substr(tolower(varMethod[[1]]), 0,1)
  call <- match.call()
  pctMethod <- match.arg(pctMethod)
  dofMethod <- match.arg(dofMethod)
  # check percentiles arguments
  if (any(percentiles < 0 | percentiles > 100)) {
    message(sQuote("percentiles"), " must be between 0 and 100. Values out of range are omitted.")
    percentiles <- percentiles[percentiles >=0 & percentiles <= 100]
  }
  
  if (length(percentiles) == 0) {
    stop("The function requires at least 1 valid percentile. Please check ", sQuote("percentiles"), " argument.")
  }
  if (all(percentiles >= 0 & percentiles <= 1) & any(percentiles != 0)) {
    warning("All values in the ",sQuote("percentiles"), " argument are between 0 and 1. Note that the function uses a 0-100 scale.")
  }
  # deal with the possibility that data is an edsurvey.data.frame.list
  if(inherits(data, "edsurvey.data.frame.list")) {
    ll <- length(data$datalist)
    lp <- length(percentiles)
    # check variable specific to edsurvey.data.frame.list
    call0 <- match.call()
    res <- list(summary=list())
    # because R does partial matching the varialble name for the `data` argument could be a variety of things
    # this code finds its index using the pmatch function
    ln <- length(names(call))
    # this pmatch will return a vector like c(0,0,1,0) if `data` is the third element
    datapos <- which.max(pmatch(names(call), "data", 0L))
    warns <- c()
    results <- sapply(1:ll, function(i) {
      call[[datapos]] <- data$datalist[[i]]
      tryCatch(suppressWarnings(eval(call)),
               error = function(cond) {
                 warns <<- c(warns, dQuote(paste(data$covs[i,], collapse=" ")))
                 nullRes <- data.frame(stringsAsFactors=FALSE,
                                       percentile = percentiles,
                                       estimate = rep(NA,lp),
                                       se = rep(NA,lp),
                                       df = rep(NA,lp))
                 if(confInt){
                   nullRes$confInt.ci_lower <- rep(NA,lp)
                   nullRes$confInt.ci_upper <- rep(NA,lp)
                 }
                 nullRes$nsmall <- rep(NA, lp)
                 print(nullRes)
                 return(nullRes)
               })
    }, simplify=FALSE)
    if(length(warns) > 0) {
      if(length(warns)>1) {
        datasets <- "datasets"
      } else {
        datasets <- "dataset"
      }
      warning(paste0("Could not process ", datasets, " ", pasteItems(warns), ". Try running this call with just the affected ", datasets, " for more details."))
    }

    # a block consists of the covs and the results for a percentile level:
    resdf <- cbind(data$covs, t(sapply(1:ll, function(ii) { results[[ii]][1,] }, simplify=TRUE)))

    ind <- 2 #iteration starts at second column
    while(ind <= lp) { # lp is the number of percentiles
      # this just grabs blocks, for each percentile level
      newblock <- cbind(data$covs, t(sapply(1:ll, function(ii) {
        results[[ii]][ind,]
      }, simplify=TRUE)))
      # and then appends them to the bottom of the results
      resdf <- rbind(resdf, newblock)
      ind <- ind + 1
    }

    attr(resdf, "call") <- call0
    class(resdf) <- c("percentileList", "data.frame")
    return(resdf)
  } else {
    ####################################### 
    ############### Outline ############### 
    #######################################
    # 1) get data for this variable and weight
    # 2) setup the (x,y,w) data.frame
    # 3) identify requested points
    # 4) Calculate final results

    # clean incoming vars

    # if the weight var is not set, use the default
    wgt <- checkWeightVar(data, weightVar)

    # 1) get data for this variable and weight
    taylorVars <- NULL
    if(varMethod=="t") {
      taylorVars <- c(getPSUVar(data, wgt), getStratumVar(data, wgt))
    }
    getDataVarNames <- c(variable, wgt, taylorVars)
    tryCatch(getDataVarNames <- c(getDataVarNames, PSUStratumNeeded(returnNumberOfPSU | confInt, data)),
             error=function(e) {
               if(returnNumberOfPSU) {
                 warning(paste0("Stratum and PSU variables are required for this call and are not on the incoming data. Ignoring ", dQuote("returnNumberOfPSU=TRUE"),"."))
               }
               returnNumberOfPSU <<- FALSE
             })

    getDataArgs <- list(data=data,
                        varnames=getDataVarNames,
                        returnJKreplicates=TRUE,
                        drop=FALSE,
                        omittedLevels=omittedLevels,
                        recode=recode,
                        includeNaLabel=TRUE,
                        dropUnusedLevels=TRUE)
    # Default conditions should be included only if the user set it. This adds
    # the argument only if needed
    if(!missing(defaultConditions)) {
      getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
    }
    # edf is the actual data
    suppressWarnings(edf <- do.call(getData, getDataArgs))
    # check that there is some data to work with
    if(any(edf[,wgt] <= 0)) {
      warning("Removing rows with 0 weight from analysis.")
      edf <- edf[edf[,wgt] > 0,]
    }
    if(nrow(edf) <= 0) {
      stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                  sQuote("getData"), "."))
    }

    # get Plauisble Values of Y variable 
    pvvariable <- hasPlausibleValue(variable, data) # pvy is the plausible values of the y variable
    variables <- variable
    if(pvvariable) {
      variables <- getPlausibleValue(variable, data)
    } else {
      # if not, make sure that this variable is numeric
      edf[,variable] <- as.numeric(edf[,variable])
    }

    # is this a NAEP linking error formula case
    linkingError <- "NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking", variables, fixed=TRUE))
    # jrrIMax
    jrrIMax <- min(jrrIMax, length(variables))
    
    # get the jackknife replicate weights for this sdf
    jkw <- getWeightJkReplicates(wgt, data)
    # for each variable to find percentiles in
    percentileGen <- function(thesePercentiles, pctMethod) {
      # make sure these are not out of bounds
      fnp <- function(pv, w) {
        # data frame with x and w on it. It will eventually get the percentile for that point
        xpw <- data.frame(x=pv[w>0],
                          w=w[w>0])
        # remove missings
        xpw <- xpw[!is.na(xpw$x),]
        # order the results by x
        xpw <- xpw[order(xpw$x),]
        # WW is the total weight
        WW <- sum(xpw$w)
        # number of interior points, note extras not added yet
        nn <- nrow(xpw)
        # the percentile of each point,
        # using percentile method recomended by Hyndman and Fan (1996)
        # see percentile vignette for details.
        # xpwc is condensed (duplicate values of x are merged, summing the weight)
        # this is then used for pcpt, while xpw is used for all other pruposes
        if(pctMethod == "simple") {
          # result vector
          resv <- rep(NA, length(thesePercentiles))
          resp <- 100 * cumsum(xpw$w)/WW
          xpw$p <- resp
          for(ri in 1:length(thesePercentiles)) {
            # k + 1 (or, in short hand kp1)
            # which.max returns the index of the first value of TRUE
            kp1 <- which.max(xpw$p >= thesePercentiles[ri]) 
            resv[ri] <- xpw$x[kp1]
            names(resv)[ri] <- paste0("P", thesePercentiles[ri])
          }
          resv[thesePercentiles > xpw$p[nrow(xpw)]] <- xpw$x[nrow(xpw)]
          return(resv)
        } else if (pctMethod == "unbiased") {
          if (is.matrix(thesePercentiles)) {
            # for lower and upper latent_ci
            len <- nrow(thesePercentiles)
          } else {
            len <- length(thesePercentiles)
          }
          xpwdt <- data.table(xpw)
          xpw <- as.data.frame(xpwdt[, w:=mean(w), by="x"])
          # see Stats vignette or Hyndman & Fan
          kp <- 1 + (nn-1)/(WW-1) * ((cumsum(c(0,xpw$w[-nrow(xpw)]))+ (xpw$w-1)/2))
          # do not let kp go below 1 nor above nn-1, moves nothing when all weights >= 1
          kp <- pmax(1,pmin(nn-1,kp))
          resp <- 100 * (kp - 1/3) / (nn + 1/3)
          # this does not cover the entire 0 to 100 interval, so add points on the end.
          xpwc <- rbind(xpw[1,], xpw, xpw[nrow(xpw),])
          xpwc$w[1] <- xpwc$w[nrow(xpwc)] <- 0
          xpwc$p <- c(0,resp,100)
          # could be larger than max, handle that
          resv <- sapply(1:len, function(ri) {
            if (is.matrix(thesePercentiles)) {
              # for lower and upper latent_ci
              n <- 2
              p <- thesePercentiles[ri, ]
            } else {
              n <- 1
              p <- thesePercentiles[ri]
            }
            
            sapply(1:n, function(r_i) {
              p <- p[r_i]
              p <- min( max(p,0) , 100) # enforce 0 to 100 range
              # k + 1 (or, in short hand kp1)
              # which.max returns the index of the first value of TRUE
              xpwc <- xpwc[!duplicated(xpwc$p),]
              # kp1 stands for k + 1
              kp1 <- which.max(xpwc$p >= p)
              if(kp1==1) {
                xpwc$x[1]
              } else {
                # k = (k+1) - 1
                k <- kp1 - 1
                pk <- xpwc$p[k]
                pkp1 <- xpwc$p[kp1]
                gamma <- (p-pk)/(pkp1 - pk)
                #interpolate between k and k+1
                (1-gamma) * xpwc$x[k] + gamma * xpwc$x[kp1]
              }
            })
          })
          for(i in 1:len) {
            names(resv)[i] <- paste0("P",thesePercentiles[i])
          }
          return(resv)
        } else {
          # pctMethod == 'symmetric'
          # result vector
          resv <- rep(NA, length(thesePercentiles))
          xpwdt <- data.table(xpw)
          xpw <- as.data.frame(xpwdt[, w:=mean(w), by="x"])
          # see Stats vignette or Hyndman & Fan
          kp <- 1/2 + (nn-1)/(WW-1) * ((cumsum(c(0, xpw$w[-nrow(xpw)])) + (xpw$w-1)/2))
          # do not let kp go below 1 nor above nn-1, moves nothing when all weights >= 1
          kp <- pmax(1, pmin(nn-1, kp))	        
          resp <- 100 * kp / nn	        
          xpw$p <- resp
          # could be larger than max, handle that
          for(ri in 1:length(thesePercentiles)) {
            thisPercentile <- thesePercentiles[ri]
            names(resv)[ri] <- paste0("P", thisPercentile)
            if(thisPercentile > xpw$p[nrow(xpw)]) {
              resv[ri] <- xpw$x[nrow(xpw)]
            } else {
              # kp1 stands for k + 1
              kp1 <- which.max(xpw$p >= thisPercentile)
              # less than min, so return min
              if(kp1==1) {
                resv[ri] <- xpw$x[1]
              } else {
                # interior cases:
                # k = (k+1) - 1	
                k <- kp1 - 1	
                pk <- xpw$p[k]	
                pkp1 <- xpw$p[kp1]	
                gamma <- (thisPercentile-pk)/(pkp1 - pk)	
                #interpolate between k and k+1	
                resv[ri] <- (1-gamma) * xpw$x[k] + gamma * xpw$x[kp1]	
              }
            }
          }
          return(resv)
        }
      }
      return(fnp)
    }

    # check for unusable jrrIMax for linking error
    if(linkingError & jrrIMax != 1) {
      warning("The linking error variance estimator only supports ", dQuote("jrrIMax=1"), ". Resetting to 1.")
      jrrIMax <- 1
    }
    if(linkingError){
      # 2) setup the (x,y,w) data.frame
      resdf <- data.frame(inst=1:length(variables))
      for(i in 1:length(percentiles)) {
        resdf[paste0("P",percentiles[i])] <- 0
      }
      
      # get the jackknife replicate weights for this sdf
      # varm: JRR contributions
      # nsmall: minimum (smaller value) of n above or below percentile
      #varm <- nsmall <- r <- matrix(NA, nrow=length(variables), ncol=length(percentiles))
      # for each variable to find percentiles in
      jkSumMult <- getAttributes(data, "jkSumMultiplier")
      pctfi <- percentileGen(percentiles, pctMethod)
      estVars <- variables[!(grepl("_imp_",variables) | grepl("_samp_",variables))]
      esti <- getLinkingEst(edf, estVars, stat=pctfi, wgt=wgt)
      # build imputation variance inputs
      varEstInputsPV <- data.frame(PV=rep(1:nrow(esti$coef), length(percentiles)),
                                   variable=rep(paste0("P",percentiles), each=nrow(esti$coef)),
                                   value=rep(NA,nrow(esti$coef) * length(percentiles)))
      for(pcti in 1:length(percentiles)) {
        for(pctj in 1:length(estVars)) {
          varEstInputsPV$value[varEstInputsPV$PV == pctj & varEstInputsPV$variable==names(esti$est)[pcti]] <- esti$coef[pctj, pcti] - mean(esti$coef[ , pcti])
        }
      }
      
      # get sampling var
      Vjrr <-  getLinkingSampVar(edf,
                                 pvSamp=variables[grep("_samp", variables)],
                                 stat = pctfi,
                                 rwgt = jkw,
                                 T0 = esti$est,
                                 T0Centered = FALSE)
      # now finalize imputation vairance
      M <- length(variables)
      # imputaiton variance / variance due to uncertaintly about PVs
      Vimp <- getLinkingImpVar(data=edf,
                               pvImp = variables[grep("_imp", variables)],
                               ramCols = ncol(getRAM()),
                               stat = pctfi,
                               wgt = wgt,
                               T0 = esti$est,
                               T0Centered = FALSE)
      varEstInputsJK <- Vjrr$veiJK
      varEstInputsJK$value <- -1 * varEstInputsJK$value
      # total variance
      V <- Vimp$V + Vjrr$V
    } else {
      # NOT linkingerror

      jkSumMult <- getAttributes(data, "jkSumMultiplier")
      pctfi <- percentileGen(percentiles, pctMethod)
      esti <- getEst(edf, variables, stat=pctfi, wgt=wgt)
      names(esti$est) <- paste0("P",percentiles)
      colnames(esti$coef) <- paste0("P",percentiles)

      # build imputation variance inputs
      varEstInputsPV <- data.frame(PV=rep(1:nrow(esti$coef), length(percentiles)),
                                   variable=rep(paste0("P",percentiles), each=nrow(esti$coef)),
                                   value=rep(NA,nrow(esti$coef) * length(percentiles)))
      for(pcti in 1:length(percentiles)) {
        for(pctj in 1:length(variables)) {
          varEstInputsPV$value[varEstInputsPV$PV == pctj & varEstInputsPV$variable==names(esti$est)[pcti]] <- esti$coef[pctj, pcti] - mean(esti$coef[ , pcti])
        }
      }
      if (! pctMethod %in% 'unbiased') {
        # pctMethod is "simple" or "symmetric"
        # build jackkinve
        vsi <- matrix(0, nrow=jrrIMax, ncol=length(percentiles))
        for(j in 1:jrrIMax) {
          # based on jth PV, so use co0 from jth PV
          vestj <- getVarEstJK(stat=pctfi, yvar=edf[ , variables[j]], wgtM=edf[ , jkw], co0=esti$coef[j,], jkSumMult=jkSumMult, pvName=j)
          if(j == 1) {
            varEstInputsJK <- vestj$veiJK
          } else {
            varEstInputsJK <- rbind(varEstInputsJK, vestj$veiJK)
          }
          vsi[j,] <- vestj$VsampInp
        }
        # sampling variance is then the mean
        Vjrr <- apply(vsi, 2, mean)
        
      } else {
        # pctMethod is 'unbiased'
        percentileGen2 <- function(thesePercentiles) {
          # make sure these are not out of bounds
          thesePercentiles <- pmin( pmax(thesePercentiles, 0) , 100) # enforce 0 to 100 range
          fnp <- function(pv, w, jkws, rv, jmax) {
            # result vector
            jkwc <- colnames(jkws)
            rprs <- matrix(0, nrow=length(jkwc), ncol=length(percentiles))
            nsmall <- rep(NA, length(thesePercentiles))
            # data frame with x and w on it. It will eventually get the percentile for that point
            xpw <- data.frame(x=pv,
                              w=w)
            xpw <- cbind(xpw, jkws)
            xpw <- xpw[xpw$w>0, ]
            # remove missings
            xpw <- xpw[!is.na(xpw$x),]
            # order the results by x
            xpw <- xpw[ord <- order(xpw$x),]  # keep order as ord for use in sapply later
            
            WW <- sum(xpw$w)
            nn <- nrow(xpw)
            xpwdt <- data.table(xpw)
            xpwc <- as.data.frame(xpwdt[, w:=mean(w), by="x"])
            w <- xpw$w
            # see Stats vignette or Hyndman & Fan
            kp <- 1 + (nn-1)/(WW-1) * ((cumsum(c(0,w[-length(w)]))+ (w-1)/2))
            # do not let kp go below 1 nor above nn-1, moves nothing when all weights >= 1
            kp <- pmax(1,pmin(nn-1,kp))
            resp <- 100 * (kp - 1/3) / (nn + 1/3)
            # this does not cover the entire 0 to 100 interval, so add points on the end.
            xpwc <- rbind(xpwc[1,], xpwc, xpwc[nrow(xpwc),])
            xpwc$w[1] <- xpwc$w[nrow(xpwc)] <- 0
            xpwc$p <- c(0,resp,100)
            
            for(ri in 1:length(thesePercentiles)) {
              p <- thesePercentiles[ri]
              rp <- rv[ri]
              nsmall[ri] <- max(0, -1 + min( sum(xpw$x < rv[ri]), sum(xpw$x > rv[ri])))
              
              if (jmax) {
                # k + 1 (or, in short hand kp1)
                # which.max returns the index of the first value of TRUE
                jkrp <- sapply(1:length(jkwc), function(jki) {
                  # xpw has been reordered, so have to reorder
                  # edf in the same way to use it here
                  # set the bottom and top weight to 0 so the sum is still correct 
                  xpw$w <- edf[ord,jkw[jki]]
                  WW <- sum(xpw$w)
                  nn <- nrow(xpw) - 2
                  xpwdt <- data.table(xpw)
                  xpw <- as.data.frame(xpwdt[, w:=mean(w), by="x"])
                  w <- xpw$w
                  # see Stats vignette or Hyndman & Fan
                  kp <- 1 + (nn-1)/(WW-1) * ((cumsum(c(0,w[-length(w)]))+ (w-1)/2))
                  # do not let kp go below 1 nor above nn-1, moves nothing when all weights >= 1
                  kp <- pmax(1,pmin(nn-1,kp))
                  resp <- 100 * (kp - 1/3) / (nn + 1/3)
                  xpwc <- rbind(xpw[1,], xpw, xpw[nrow(xpw),])
                  xpwc$w[1] <- xpwc$w[nrow(xpwc)] <- 0
                  xpwc$p <- c(0,resp,100)
                  xpwc <- xpwc[!duplicated(xpwc$p),]
                  
                  # estimate the percentile with the new weights
                  kp1 <- which.max(xpwc$p >= p)
                  if(kp1==1) {
                    xpwc$x[1]
                  } else {
                    # k = (k+1) - 1
                    k <- kp1 - 1
                    pk <- xpwc$p[k]
                    pkp1 <- xpwc$p[kp1]
                    gamma <- (p-pk)/(pkp1 - pk)
                    #interpolate between k and k+1
                    (1-gamma) * xpwc$x[k] + gamma * xpwc$x[kp1]
                  }
                })
                
                rpr <- jkrp - rp
                #if difference rpr is very small round to 0 for DOF correctness 
                rpr[which(abs(rpr) < (sqrt(.Machine$double.eps)*sqrt(length(jkrp))))] <- 0 
                rprs[ , ri] <- rpr
              }
            }
            nsmall[is.na(nsmall)] <- 0
            return (list(rprs = rprs, nsmall = nsmall))
          }
          return(fnp)
        }
        
        pctfi2 <- percentileGen2(percentiles)
        # build jackkinve
        vestj <- getVarEstJKPercentile(data=edf, pvEst=variables, stat=pctfi2, wgt=wgt, jkw=jkw, rvs=esti$coef, jmaxN=jrrIMax)
        # reshape
        vestj <- as.data.frame(vestj$rprs)
        cols <- paste0('P',percentiles)
        colnames(vestj) <- cols
        vestj$JKreplicate <- rep(1:length(jkw), jrrIMax)
        vestj$PV <- rep(1:jrrIMax, each=length(jkw))
        varEstInputsJK <- reshape(vestj, direction = 'long', idvar=c('JKreplicate','PV'), varying=cols, times=cols, timevar='variable', v.names='value')
        rownames(varEstInputsJK) <- paste0(rep(1:length(jkw),length(percentiles)),'.',rep(1:length(percentiles),each=length(jkw)))
        varEstInputsJK$vs <- varEstInputsJK$value^2
        varm2 <- aggregate(vs ~ PV + variable, varEstInputsJK, sum)
        varEstInputsJK$vs <- NULL
        varEstInputsJK <- varEstInputsJK[,c('PV', 'JKreplicate', 'variable', 'value')]
        # sampling variance is then the mean
        a <- aggregate(vs ~ variable, varm2, mean)
        a$variable <- as.numeric(gsub('P','',a$variable))
        a <- a[order(a$variable),]
        Vjrr <- getAttributes(data, "jkSumMultiplier") * a$vs
      }
      
      # now finalize imputation vairance
      M <- length(variables)
      # imputaiton variance / variance due to uncertaintly about PVs
      Vimp <- rep(0, ncol(esti$coef))
      if(M > 1) {
        # imputation variance with M correction (see stats vignette)
        # [r - mean(r)]^2
        Vimp <- (M+1)/(M * (M-1)) * apply( (t(t(esti$coef) - apply(esti$coef, 2, mean)))^2, 2, sum) # will be 0 when there are no PVs
      }
      # total variance
      V <- Vimp + Vjrr
    } # end else for if(linkingError)

    varEstInputs <- list(JK=varEstInputsJK, PV=varEstInputsPV)

    # get dof, used in confidence inverval too
    dof <- 0*percentiles
    for(i in 1:length(dof)) {
      dof[i] <- DoFCorrection(varEstA=varEstInputs, varA=paste0("P", percentiles[i]), method=dofMethod)
    }
    # simple dof is #PSUs - # strata. Since 2 PSUs/strata, use that here.
    dof_simple <- nrow(varEstInputsJK)

    # find confidence interval
    # first, find the variance of the fraction that are above/below this
    # percentile

    if(confInt){
      if(varMethod=="j") {
        #Jackknife method for estimating the variance of the percent below P
        # note: the below works when the are or are not plausible vaues.
        # that is, the section, "Estimation of the standard error of weighted
        # percentages when plausible values are not present, using the jackknife
        # method" is implemented when there are not plausible vlaues present 
        # while the section, "Estimation of the standard error of weighted
        # percentages when plausible values are present, using the jackknife
        # method" is implemented when they are present.
        belowGen <- function(cutoffs) {
          f <- function(pv, w) {
            res <- rep(NA, length(cutoffs))
            for(i in 1:length(cutoffs)) {
              below <- pv < cutoffs[i]
              res[i] <- sum(w[below]) / sum(w)
            }
            names(res) <- names(esti$est)
            return(res)
          }
          return(f)
        }
        belowf <- belowGen(esti$est)
        belowf(edf[,variables[1]], edf$origwt)
        # estb[elow] that percentile
        pEst <- getEst(edf, variables, stat=belowf, wgt=wgt)
        # imputation variance 
        pVimp <- rep(0, ncol(pEst$coef))
        if(M > 1) {
          # imputation variance with M correction (see stats vignette)
          # [r - mean(r)]^2
          pVimp <- (M+1)/(M * (M-1)) * apply( (t(t(pEst$coef) - apply(pEst$coef, 2, mean)))^2, 2, sum) # will be 0 when there are no PVs
        } # else, Vimp is 0, no change needed
        # sampling variance for below
        pVsi <- matrix(0, nrow=jrrIMax, ncol=length(percentiles))
        for(j in 1:jrrIMax) {
          vestj <- getVarEstJK(stat=belowf, yvar=edf[ , variables[j]], wgtM=edf[ , jkw], co0=pEst$coef[j,], jkSumMult=jkSumMult, pvName=j)
          pVsi[j,] <- vestj$VsampInp
        }
        # sampling variance is then the mean across PVs up to jrrIMax
        pVjrr <- apply(pVsi, 2, mean)
        pV <- pVimp + pVjrr
        
      } else { # end if(varMethod=="j")
        
        # Taylor series method for confidence intervals
        r0 <- esti$est
        # We need the pi value, the proportion under the Pth percentile
        mu0 <- sapply(1:length(percentiles), function(i) {
          # get the mean of the estimates across pvs
          mean(sapply(1:length(variables), function(vari) {
            # for an individual PV, get the estimated fraction below r0[i]
            vv <- variables[vari]
            xpw <- data.frame(x=edf[[vv]],
                              w=edf[[wgt]])
            xpw <- xpw[!is.na(xpw$x),]
            xpw$below <- (xpw$x <= r0[i])
            s0 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
          }))
        })
        
        # there are two states here, above and below the percentile
        # so the D matrix and Z matrix are 1x1s
        # first calculate D
        # get the sum of weights
        # all xs will have the same missing values, so we can just use the first
        # x because we are just using the missingness
        vv <- variables[1]
        Ws <- edf[!is.na(edf[[vv]]), wgt]
        D <- 1/sum(Ws)
        
        # calculate the pVjrr (Z) and pVimp together
        pV <- sapply(1:length(percentiles), function(ri) {
          est <- getEstPcTy(data = edf,
                            pvEst = variables,
                            stat = getPcTy,
                            wgt = wgt,
                            D = D,
                            r0_ri = r0[ri],
                            s0 = mu0[ri],
                            psuV=edf[ , getPSUVar(data, wgt)], 
                            stratV=edf[ , getStratumVar(data, wgt)])
        }) 
        
        pVjrr <- do.call(cbind, pV['pVjrrs',])
        pVimp <- do.call(cbind, pV['pVimps',])
        
        pVjrr <- apply(pVjrr,2,mean, na.rm=TRUE)
        pVimp <- (M+1)/(M * (M-1)) * apply(pVimp, 2, sum) # will be 0 when there are no PVs
        pV <- pVimp + pVjrr

      } # end else for if(varMethod=="j")
      
      if (! pctMethod %in% 'unbiased') {
        latent_ci_min <- percentiles + 100 * sqrt(pV) * qt(alpha/2, df=pmax(dof, 1))
        latent_ci_max <- percentiles + 100 * sqrt(pV) * qt(1-alpha/2, df=pmax(dof, 1))
      } else {
        latent_ci_min <- percentiles + 100 * sqrt(pV) * qt(alpha/2, df=dof_simple)
        latent_ci_max <- percentiles + 100 * sqrt(pV) * qt(1-alpha/2, df=dof_simple)
      }
      
      names(latent_ci_min) <- paste0('P', percentiles)
      names(latent_ci_max) <- paste0('P', percentiles)
      latent_ci <- matrix(c(latent_ci_min, latent_ci_max), ncol=2) 
      
      if (! pctMethod %in% 'unbiased') {
        # map back to the variable space
        pctfiCIL <- percentileGen(latent_ci[,1], pctMethod)
        ciL <- getEst(edf, variables, stat=pctfiCIL, wgt=wgt)$est
        pctfiCIU <- percentileGen(latent_ci[,2], pctMethod)
        ciU <- getEst(edf, variables, stat=pctfiCIU, wgt=wgt)$est
        ci <- data.frame(ci_lower=ciL, ci_upper=ciU)
      } else {
        # map back to the variable space
        pctfiCIL <- percentileGen(latent_ci, pctMethod)
        ciL <- getEst(edf, variables, stat=pctfiCIL, wgt=wgt)
        ci <- matrix(ciL$est, nrow = length(percentiles), byrow = TRUE)
        colnames(ci) <- c("ci_lower", "ci_upper")
        rownames(ci) <- percentiles
      }

    } # end if(confInt)
    # 4) Calculate final results
    if(confInt) {
      res <- data.frame(stringsAsFactors=FALSE,
                        percentile=percentiles, estimate=unname(esti$est), se=sqrt(V),
                        df=dof,
                        confInt=ci)
    } else {
      res <- data.frame(stringsAsFactors=FALSE,
                        percentile=percentiles, estimate=unname(esti$est), se=sqrt(V),
                        df=dof)
    }
    if(returnVarEstInputs) {
      attr(res, "varEstInputs") <- varEstInputs
    }
    attr(res, "n0") <- nrow2.edsurvey.data.frame(data)
    attr(res, "nUsed") <- nrow(edf)
    if (returnNumberOfPSU) {
      stratumVar <- getAttributes(data, "stratumVar")
      psuVar <- getAttributes(data, "psuVar")
      if(sum(is.na(edf[,c(stratumVar, psuVar)])) == 0) {
        attr(res, "nPSU") <- nrow(unique(edf[,c(stratumVar,psuVar)]))
      } else {
        warning("Cannot return number of PSUs because the stratum or PSU variables contain NA values.")
      }
    }
    attr(res, "call") <- call
    class(res) <- c("percentile", "data.frame")
    return(res)
  } # end else for if(inherits(data, "edsurvey.data.frame.list")) {
}

#' @method print percentile
#' @export
print.percentile <- function(x, ...) {
  cat("Percentile\nCall: ")
  print(attributes(x)$call)
  cat(paste0("full data n: ", attributes(x)$n0, "\n"))
  cat(paste0("n used: ", attributes(x)$nUsed, "\n"))
  if(!is.null(attributes(x)$nPSU)) {
    cat(paste0("n PSU: ", attributes(x)$nPSU, "\n"))
  }
  cat("\n")
  class(x) <- "data.frame"
  if(min(x$df) <=2) {
    warning("Some degrees of freedom less than or equal to 2, indicating non-finite variance. These estimates should be treated with caution.")
  }
  x$nsmall <- NULL
  print(x, row.names=FALSE, ...)
}

#' @method print percentileList
#' @export
print.percentileList <- function(x, ...) {
  cat("percentileList\nCall: ")
  print(attributes(x)$call)
  cat("\n")
  class(x) <- "data.frame"
  print(x, row.names=FALSE, ...)
}



# @description          calculate pVjrr and pVimp for a percentile and outcome variable
# @param    xpw         dataframe with an outcome variable, weight, psuvar, stratumvar
#           D           1 over sum of weights
#           r0_ri       r0 (esti$est) for i'th percentile
#           s0          mu0 for i'th percentile
#           pVimp       boolean, if false (only one outcome in entire data) pVimp is 0
# @return               a list with the following elements
#           pVjrr       pVjrr for the percentile and outcome   
#           pVimp       pVimp for the percentile and outcome
getPcTy <- function(xpw, D, r0_ri, s0, pVimp=TRUE) {
  # pVjrr
  lengthunique <- function(x) { length(unique(x)) }
  psustrat0 <- aggregate(psuV ~ stratV, data=xpw, FUN=lengthunique)
  # subset to just those units in strata that have more than one active PSU
  names(psustrat0)[names(psustrat0) == "psuV"] <- "psuV_n"
  xpw <- merge(xpw, psustrat0, by="stratV", all.x=TRUE)
  xpw <- xpw[xpw$psuV_n > 1,]
  if (nrow(xpw) > 1) {
    # now get the mean deviates, S in the statistics vignette
    xpw$below <- (xpw$x <= r0_ri) 
    xpw$S <- xpw$w * (xpw$below - s0)
    psustrat <- aggregate(S ~ stratV + psuV, data=xpw, FUN=sum)
    meanna <- function(x) { mean(x, na.rm=TRUE) }
    psustrat$stratum_mu <- ave(psustrat$S, psustrat$stratV, FUN=meanna)
    psustrat$U_sk <- psustrat$S - psustrat$stratum_mu
    psustrat$U_sk2 <- psustrat$U_sk^2
    Z <- sum(psustrat$U_sk2, na.rm=TRUE)
    pVjrr_ri_vari <- D*Z*D
  } else {
    pVjrr_ri_vari <- NA
  }
  
  # pVimp
  if (pVimp) {
    # only calculate if more than one outcome in sdf
    s1 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
    pVimp_ri_vari <- (s1 - s0)^2
  } else {
    pVimp_ri_vari <- 0
  }
  return(list(pVjrr=pVjrr_ri_vari, pVimp=pVimp_ri_vari))
}


# @description          calculate vector of pVjrrs and pVimps (for each outcome var) for a percentile
# @param    data        dataframe 
#           pvEst       vector of outcome vars
#           stat        statistic to calculate
#           wgt         weight var
#           D           1 over sum of weights
#           r0_ri       r0 (esti$est) for i'th percentile
#           s0          mu0 for i'th percentile
#           psuV        psu var
#           stratV      stratum var
# @return               a list with the following elements
#           pVjrrs      vector of pVjrr for all the outcome vars for a percentile    
#           pVimps      vector of pVimp for all the outcome vars for a percentile
getEstPcTy <- function(data, pvEst, stat, wgt, D, r0_ri, s0, psuV, stratV) {
  pVjrrs <- c()
  pVimps <- c()
  for (n in 1:length(pvEst)) {
    xpw <- data.frame(x=data[ , pvEst[n]],
                      w=data[ , wgt],
                      psuV=psuV,
                      stratV=stratV)
    xpw <- xpw[!is.na(xpw$x),]
    if (length(pvEst)==1) {
      res <- stat(xpw=xpw, D=D, r0_ri=r0_ri, s0=s0, pVimp=FALSE)
    } else {
      res <- stat(xpw=xpw, D=D, r0_ri=r0_ri, s0=s0, pVimp=TRUE)
    }
    pVjrrs <- c(pVjrrs, res$pVjrr)
    pVimps <- c(pVimps, res$pVimp) 
  }
  return(list(pVjrrs=pVjrrs, pVimps=pVimps))
}