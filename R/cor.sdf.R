#' @title Bivariate Correlation
#'
#' @description Computes the correlation of two variables on an \code{edsurvey.data.frame},
#'              a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'              The correlation accounts for plausible values and the survey design.
#'
#' @param x          a character variable name from the \code{data} to be correlated with \code{y}
#' @param y          a character variable name from the \code{data} to be correlated with \code{x}
#' @param data       an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}
#' @param method     a character string indicating which correlation coefficient (or covariance) is to be computed.
#'                   One of \code{Pearson} (default), \code{Spearman}, \code{Polychoric}, or \code{Polyserial}. For Polyserial, the continuous argument must be \code{x}. 
#' @param weightVar  character indicating the weight variable to use. See Details section in \code{\link{lm.sdf}}.
#' @param reorder    a list of variables to reorder. Defaults to \code{NULL} (no variables are reordered). Can be set as 
#'                   \code{reorder} \code{=} \code{list(var1} \code{=} \code{c("a","b","c"),} \code{var2} \code{=} \code{c("4", "3", "2", "1"))}. See Examples.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops those levels of all factor variables that are specified
#'                        in an \code{edsurvey.data.frame}. Use \code{print} on an \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default conditions stored in an \code{edsurvey.data.frame}
#'                           to subset the data. Use \code{print} on an \code{edsurvey.data.frame} to see the default conditions.
#' @param recode       a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                    \code{recode} \code{=} \code{list(var1} \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to} \code{=} \code{"d"))}. See Examples.
#' @param condenseLevels a logical value. When set to the default value of
#'                       \code{TRUE} and either \code{x} or \code{y} is a
#'                       categorical variable, the function will drop all unused
#'                       levels and rank the levels of the variable before
#'                       calculating the correlation. When set to \code{FALSE},
#'                       the numeric levels of the variable remain the same as
#'                       in the codebook. See Examples.
#' @param fisherZ for standard error and mean calculations, set to \code{TRUE} to use
#'                the Fisher Z-transformation (see details), or \code{FALSE}
#'                to use no transformation of the data. The \code{fisherZ} argument defaults
#'                to Fisher Z-transformation for Pearson and no transformation  
#'                for other correlation types.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=Inf}, uses the 
#'                   sampling variance from all plausible values as the component for sampling variance estimation. The \code{Vjrr} 
#'                   term (see 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param verbose a logical value. Set to \code{FALSE} to avoid messages about variable conversion.
#' 
#' @details 
#' The \code{\link{getData}} arguments and \code{\link{recode.sdf}} may be useful. (See Examples.)
#' The correlation methods are calculated as described in the documentation for the \code{wCorr} package---see \code{browseVignettes(package="wCorr")}.
#'
#' When \code{method} is set to \code{polyserial}, all \code{x} arguments are assumed to be continuous and all \code{y} assumed discrete. Therefore,
#' be mindful of variable selection as this may result in calculations taking a very long time to complete.
#'
#' The Fisher Z-transformation is both a variance stabilizing  and normalizing transformation
#' for the Pearson correlation coefficient (Fisher, 1915).
#' The transformation takes the inverse hybarbolic tangent of the correlation coefficients and then calculates all variances and confidence intervals.
#' These are then transformed back to the correlation space (values between -1 and 1, inclusive) using the hyperbolic tangent function.
#' The Taylor series approximation (or delta method) is applied for the standard errors.
#'
#' @return
#' An \code{edsurvey.cor} that has print and summary methods.
#'
#' The class includes the following elements:
#' \item{correlation}{numeric estimated correlation coefficient}
#' \item{Zse}{standard error of the correlation (\code{Vimp} + \code{Vjrr}). In the case of Pearson, this is calculated in the linear atanh space and is not a standard error in the usual sense.}
#' \item{correlates}{a vector of length two showing the columns for which the correlation coefficient was calculated}
#' \item{variables}{\code{correlates} that are discrete}
#' \item{order}{a list that shows the order of each variable}
#' \item{method}{the type of correlation estimated}
#' \item{Vjrr}{the jackknife component of the variance estimate. For Pearson, in the atanh space.}
#' \item{Vimp}{the imputation component of the variance estimate. For Pearson, in the atanh space.}
#' \item{weight}{the weight variable used}
#' \item{npv}{the number of plausible values used}
#' \item{njk}{the number of the jackknife replicates used}
#' \item{n0}{the original number of observations}
#' \item{nUsed}{the number of observations used in the analysis---after any conditions and any listwise deletion of missings is applied}
#' \item{se}{the standard error of the correlation, in the correlation ([-1,1]) space}
#' \item{ZconfidenceInterval}{the confidence interval of the correlation in the transformation space}
#' \item{confidenceInterval}{the confidence interval of the correlation in the correlation ([-1,1]) space}
#' \item{transformation}{the name of the transformation used when calculating standard errors}
#' 
#' @seealso \ifelse{latex}{\code{cor}}{\code{\link[stats]{cor}}} and \ifelse{latex}{\code{weightedCorr}}{\code{\link[wCorr]{weightedCorr}}}
#' @author Paul Bailey; relies heavily on the \code{wCorr} package, written by Ahmad Emad and Paul Bailey
#'
#' @references
#' Fisher, R. A. (1915). Frequency distribution of the values of the correlation coefficient in samples from an indefinitely large population. \emph{Biometrika}, \emph{10}(4), 507--521.
#'
#' @example man/examples/cor.sdf.R
#' @importFrom wCorr weightedCorr
#' @importFrom stats var
#' @importFrom stats qt
#' @export
cor.sdf <- function(x,
                    y,
                    data,
                    method = c("Pearson", "Spearman", "Polychoric", "Polyserial"),
                    weightVar = "default",
                    reorder = NULL,
                    omittedLevels=TRUE, 
                    defaultConditions=TRUE,
                    recode = NULL,
                    condenseLevels = TRUE,
                    fisherZ = if(match.arg(method) %in% "Pearson") {TRUE} else {FALSE},
                    jrrIMax=Inf, 
                    verbose=TRUE) {
  call <- match.call()
  method <- match.arg(method)
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    # use itterateESDFL to do this call to every element of the edsurvey.data.frame.list
    return(itterateESDFL(match.call(), data))
  }
  # allow unquoted variables

  skip <- tryCatch(inherits(x, "character"),
                  error=function(e) { FALSE },
                  warning=function(w) { FALSE })
  if(!skip) {
    x <- iparse(substitute(x), x=data)
  }
  skip <- tryCatch(inherits(y, "character"),
                  error=function(e) { FALSE },
                  warning=function(w) { FALSE })
  if(!skip) {
    y <- iparse(substitute(y), x=data)
  }

  vars <- c(x, y)
  # test input
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  pvvars <- names(getAttributes(data, "pvvars", errorCheck=FALSE))
  cn <- colnames(data)
  if(!x %in% c(pvvars, cn)) {
    stop(paste0("Could not find x column ", dQuote(x), " in data."))
  }
  if(!y %in% c(pvvars, cn)) {
    stop(paste0("Could not find y column ", dQuote(y), " in data."))
  }

  if(inherits(data[[x]], "character")) {
    stop(paste0("The argument ", sQuote("x"), " must be of a class numeric, factor, or lfactor."))
  }
  if(inherits(data[[y]], "character")) {
    stop(paste0("The argument ", sQuote("y"), " must be of a class numeric, factor, or lfactor."))
  } 

  # get method
  pm <- match.arg(method)

  # fix transformation
  if(!inherits(fisherZ, "logical")) {
    stop(paste0("The argument ", dQuote("fisherZ"), " must be TRUE or FALSE."))
  }

  if(fisherZ) {
    transformation <- FisherZTrans()
  } else {
    transformation <- identityTrans()
  }

  if(!all(c("name", "trans", "itrans", "setrans") %in% names(transformation))) {
    stop(paste0("The argument ", sQuote("transformation"),
                " must be a list with three elements, each a function, named ",
                sQuote("trans"), ",",
                sQuote("itrans"), ", and ",
                sQuote("setrans"), "."))
  }
  if(!all(inherits(transformation$name, "character"),
          inherits(transformation$trans, "function"), 
          inherits(transformation$itrans, "function"),
          inherits(transformation$setrans, "function"))) {
    stop(paste0("The argument ", sQuote("transformation"),
                " must be a list with three elements, each a function, named ",
                sQuote("trans"), ",",
                sQuote("itrans"), ", and ",
                sQuote("setrans"), "."))
  }

  if(nrow(data) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows."))
  } 

  # test if x and y are plausible value variables (multiply imputed values)
  hpvx <- hasPlausibleValue(x, data)
  hpvy <- hasPlausibleValue(y, data)

  # get weight variable
  if(is.null(weightVar)) {
    wgt <- "one"
  } else {  # if the weights are not null
    if(weightVar %in% "default") { # if weightsVar is default
      wgt <- attributes(getAttributes(data, "weights"))$default
    } else { # if wgt is not defaut
      wgt <- weightVar
    } # End of nested if statment: if wgt is defalt
  } # End of if statment: if wgt is null
  
  # setup Jack Knife replicate weight variables
  if(wgt == "one") {
    wgtl <- list(jkbase="one",jksuffixes="")
  } else { # if wgt is not "one"
    wgtl <- getAttributes(data, "weights")[[wgt]]
  } # End of if statment: if wgt is "one" 
  wgts <- c(paste0(wgtl$jkbase, wgtl$jksuffixes))
  if(wgt == "one") { 
    getV <- c(vars)
  } else { # if wgt is not "one"
    getV <- c(vars, wgt)
  } # end of if statment: if wgt is "one"

  # setup the getData call
  getDataArgs <- list(data = data,
                      varnames = getV,
                      includeNaLabel = TRUE,
                      addAttributes = TRUE,
                      returnJKreplicates = TRUE,
                      drop = FALSE,
                      omittedLevels = omittedLevels,
                      recode = recode,
                      dropUnusedLevels = condenseLevels)
  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!missing(defaultConditions)) {
    getDataArgs <- c(getDataArgs, list(defaultConditions = defaultConditions))
  } 
  # edf is the actual data
  lsdf <- do.call(getData, getDataArgs)

  # check for impending conversions:
  if(hpvx) {
    pvx <- getPlausibleValue(x, lsdf)
  }
  if(hpvy) {
    pvy <- getPlausibleValue(y, lsdf)
  }
  if(verbose) {
    if( length(levels(lsdf[[x]])) > 0 & method %in% c("Pearson", "Polyserial")) {
      message(paste0("Converting ", dQuote(x), " to a continuous variable."))
    }
    if( length(levels(lsdf[[x]])) == 0 & method %in% c("Polychoric")) {
      message(paste0("Converting ", dQuote(x), " to a discrete variable."))
      xi <- ifelse(hpvx, pvx[[1]], x)
      if(length(unique(lsdf[[xi]])) >= 10) {
        message("  - Treating a variable with ", length(unique(lsdf[[xi]])), " levels as discrete will slow computation.")
      }
    }
    if( length(levels(lsdf[[y]])) > 0 & method %in% c("Pearson")) {
      message(paste0("Converting ", dQuote(y), " to a continuous variable."))
    }
    if( length(levels(lsdf[[y]])) == 0 & method %in% c("Polyserial", "Polychoric")) {
      message(paste0("Converting ", dQuote(y), " to a discrete variable."))
      yi <- ifelse(hpvy, pvy[[1]], y)
      if(length(unique(lsdf[[yi]])) >= 10) {
        message("  - Treating a variable with ", length(unique(lsdf[[yi]])), " levels as discrete will slow computation.")
      }
    }
    if(length(levels(lsdf[[x]])) > 0 & length(levels(lsdf[[y]])) == 0 & method %in% "Polyserial") {
      message(paste0("Consider swaping ", dQuote("x"), " and ", dQuote("y"), " variables."))
    }
  }
  # if the weight variable is "one" then that will need to be a valid column
  if(wgt == "one") {
    if("one" %in% colnames(lsdf) && any(lsdf$one != 1)) {
      stop(paste0("A column named one cannot be included in the input when weights of one are implicitly used. You can rename the ", dQuote("one"), " column."))
    }
    lsdf$one <- 1
  } else{
    # weighted, check for negative weights
    if(any(lsdf[ , wgt] <= 0)) {
      lsdf <- lsdf[lsdf[ , wgt] > 0, ]
      if(nrow(lsdf) == 0) {
        stop("No rows with positive weights.")
      }
      warning("Removing rows with 0 weight from the analysis.")
    }
  } # end if(wgt=="one")
  
  # do reordering of variables when the user requets a reorder
  if(!is.null(reorder[[x]])) {
    if(hpvx) {
      stop("Reordering ", dQuote("x"), " variables with plausible values not implimented.")
    }
    llx <- unique(lsdf[ , x])
    if(is.factor(lsdf[ , x])) {
      llx <- levels(llx)
    } # End if statment: if lsdf is a factor
    if( sum(!reorder[[x]] %in% llx) > 0 ) {
      bad <- reorder[[x]][!reorder[[x]] %in% llx]
      stop(paste0("Could not find reorder level(s) ", pasteItems(sQuote(bad), final="or"), " when reordering ", sQuote("x"), "."))
    } # End if Statment: if sum(!reorder[[x]] %in% llx) > 0
    lsdf[ , x] <- factor(lsdf[ , x], levels = c(reorder[[x]]))
  } # End if statment: if reorder x is not null 

  # now reorder for y variables
  if(!is.null(reorder[[y]])) {
    if(hpvy) {
      stop("Reordering ", dQuote("y"), " variables with plausible values not implimented.")
    }
    lly <- unique(lsdf[ , y])
    if(is.factor(lsdf[ , y])) {
      lly <- levels(lly)
    } # End if statment: if lsdf[,y] is a factor 
    if( sum(!reorder[[y]] %in% lly) > 0 ) {
      bad <- reorder[[y]][!reorder[[y]] %in% lly]
      stop(paste0("Could not find reorder level(s) ", pasteItems(sQuote(bad), final="or"), " when reordering ", sQuote("y"), "."))
    } # End if statment sum(!reorder[[y]] %in% lly) > 0 
    lsdf[ , y] <- factor(lsdf[ , y], levels = c(reorder[[y]]))
  } # End if statment: if reorder[[y]] is null

  # Generate levels output for variables
  # this output shows the user what levels were used in the correlation calculation
  # when it is not obvious.
  varOrder <- list()
  variables <- c()
  # this line tests if either is longer than zero
  if(length(levels(lsdf[[x]])) || length(levels(lsdf[[y]])) > 0) {
    nums <- !sapply(lsdf, is.numeric)
    variables <- subset(names(nums), nums %in% TRUE)
    for(i in unique(variables)) {
      varn <- c()
      newv <- rep(NA, nrow(lsdf))
      # convert logical to factor
      if(inherits(lsdf[[i]], "logical")) {
        lsdf[[i]] <- factor(0 + lsdf[[i]], 0:1, c("FALSE", "TRUE"))
      }
      for(z in 1:length(levels(lsdf[[i]]))) {
        # Use llevels if it's a lfactor
        if (inherits(lsdf[[i]], "lfactor") && !condenseLevels) {
          zi <- llevels(lsdf[[i]])[z]
        } else {
          zi <- z
        }
        varlev <- levels(lsdf[[i]])[z]
        if (!varlev %in% unique(lsdf[[i]])) { # do not print out unused levels
          next
        }
        varm <- paste0(zi,". ", varlev)
        varn <- c(varn, varm)
        newv[lsdf[[i]] == varlev] <- zi
      }
      lsdf[[i]] <- newv
      varlist <- list(c(varn))
      varOrder[i] <-varlist
    }
    names(varOrder) <- c(variables)
  } else { # end if(length(levels(lsdf[[x]])) || length(levels(lsdf[[y]])) > 0) {
    variables <- NULL
    varOrder <- NULL
  } 
  # end reorder variables
  # extract plausible values
  linkingError <- FALSE
  if(hpvx) {
    xvarlsdf <- lsdf[ , unlist(pvx), drop=FALSE] #lsdf
    linkingError <- "NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking", pvx, fixed=TRUE))
  } else {
    xvarlsdf <- lsdf[ , pvx <- x, drop=FALSE] #lsdf
  }
  if(hpvy) {
    yvarlsdf <- lsdf[ , unlist(pvy), drop=FALSE] #lsdf
    # check for parity in _linking variables
    if(xor(hpvx & "NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking", pvy, fixed=TRUE)),
            linkingError)) {
      stop("When correlating two assessment scores, cannot mix _linking variables with variables not including linking error")
    }
    # this could be the only PV, so check for linking
    if("NAEP" %in% getAttributes(data, "survey") & any(grepl("_linking", pvy, fixed=TRUE))) {
      linkingError <- TRUE
    }
  } else {
    yvarlsdf <- lsdf[ , pvy <- y, drop=FALSE] #lsdf
  }
  
  # drop NAs
  filter <- complete.cases(xvarlsdf) & complete.cases(yvarlsdf)
  lsdf <- lsdf[filter, ]
  xvarlsdf <- xvarlsdf[filter, , drop=FALSE]
  yvarlsdf <- yvarlsdf[filter, , drop=FALSE]
  
  if(nrow(lsdf) == 0) {
    stop("No rows with all variables to correlate.")
  }

  # number of plausible values 
  npv <- max(npvx <- length(xvarlsdf), npvy <- length(yvarlsdf))
  # if both have PVs, use the smaller set
  if(npvx > 1 & npvy > 1) {
    npv <- min(npvx, npvy)
  }
  # the results, across the PVs (vector of length PV)
  diagVar <- sapply(1:npv, function(i) weightedCorr(xvarlsdf[ , min(i, npvx)], 
                                                    yvarlsdf[ , min(i, npvy)], 
                                                    method=pm, 
                                                    weights=lsdf[ , wgt], 
                                                    fast=TRUE, 
                                                    ML=FALSE))
  
  # for Pearson we will do a forward transform and inverse transform
  # to keep the code simple we simply set the transofmr an intervse transform
  # to the identity function for everything that is not Pearson
  trans <- transformation$trans 
  itrans <- transformation$itrans
  setrans <- transformation$setrans

  # potentially (Fisher) transform estimates (ft=Fisher transform)
  ft <- trans(diagVar)
  # estimated correlation coefficient
  mcc <- itrans(premcc <- mean(ft))

  # the rest of the code estimates the variance

  # for each jackknife replicate
  jkwgtdf <- lsdf[ , paste0(wgtl$jkbase, wgtl$jksuffixes), drop=FALSE] #dataframe of jk replicate weights
  posFilter <- jkwgtdf > 0 #dataframe of T/F
  jrrIMax <- min(npv, max(1,jrrIMax))
  diagVarWgt <- matrix(NA, ncol=jrrIMax, nrow=length(wgts))
  
  # rerun with JK replicate weights
  for(i in 1:jrrIMax) {
    diagVarWgt[ , i] <- sapply(length(wgts):1, function(jki) trans(weightedCorr(xvarlsdf[posFilter[ , jki], min(i, npvx)], 
                                                                                yvarlsdf[posFilter[ , jki], min(i, npvy)], 
                                                                                method=pm, 
                                                                                weights=jkwgtdf[posFilter[ , jki], jki], 
                                                                                fast=TRUE, 
                                                                                ML=FALSE)) - ft[i])
  }
    
  # see documentation for defintion of Vjrr, Vimp, M
  # one Vjrr per plausible value (PB)
  preVjrr = getAttributes(data, "jkSumMultiplier") * apply(diagVarWgt^2, 2, sum)
  Vjrr = mean(preVjrr, na.rm=TRUE)

  # then Vimp = ((M+1)/M) * [variance of main estimate (mcc) across the PVs.]
  # M= number of plausible values
  M <- length(diagVar)
  Vimp <- ifelse( M>1, (M+1)/M * var(ft), 0)
  # then get total variance
  V <- Vimp + Vjrr
  # se is root variance
  Zse <- sqrt(V)
  
  # varEstInputs - JK
  veJK <- data.frame(stringsAsFactors=FALSE,
                     PV=rep(1:jrrIMax, each=length(wgts)),
                     JKreplicate=rep(1:length(wgts), jrrIMax),
                     variable="cor",
                     value=as.vector(diagVarWgt)) # as.vector stacks the columns of diagVarWgt
  varEstInputs <- list(JK=veJK)
  
  # varEstInputs - PV (only if multiple PVs)
  if(length(diagVar) > 1) {
    vePV <- data.frame(stringsAsFactors=FALSE,
                       PV=1:npv,
                       variable=rep("cor", npv),
                       value=diagVar - mean(diagVar))
    varEstInputs[["PV"]] <- vePV
  } 
  

  if(wgt=="one") {
    cor <- list(correlation=mcc, Zse=NA, correlates=vars, variables=variables, order=varOrder, method=method,
                Vjrr=NA, Vimp=NA, weight="unweighted", npv=M, njk=NA, se=NA, ZconfidenceInterval=NA, confidenceInterval=NA)
  } else { # else if wgt is not "one"
    dof <- DoFCorrection(varEstInputs, varA="cor", method="JR")
    tstat <- qt(p=0.975, df=dof)
    Zci <- premcc + c(-1,1) * tstat * Zse
    ci <- itrans(Zci)
    se <- setrans(premcc, Zse)
    cor <- list(correlation=mcc, Zse=Zse, correlates=vars, variables=variables, order=varOrder, method=pm,
                Vjrr=Vjrr, Vimp=Vimp, weight=wgt, npv=M, njk=length(wgtl$jksuffixes),
                se=se, ZconfidenceInterval=Zci, confidenceInterval=ci)
  } # End of if statment: if wgt is "one"
  cor <- c(cor, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(lsdf)))
  cor <- c(cor, list(transformation=transformation$name))
  class(cor) <- "edsurveyCor"
  return(cor)
}


#' @method print edsurveyCor
#' @export
print.edsurveyCor <- function(x, digits = getOption("digits"), ...) {
  class(x) <- "list"
  cat(paste0("Method: ", x$method, "\n"))
  cat(paste0("full data n: ", x$n0, "\n"))
  cat(paste0("n used: ", x$nUsed, "\n"))
  cat("\n")
  cat(paste0("Correlation: ", paste0(signif(x$correlation, digits=digits), collapse=""), "\n"))
  cat(paste0("Standard Error: ", paste0(signif(x$se, digits=digits), collapse=""), "\n"))
  cat(paste0("Confidence Interval: [",paste0(signif(x$confidenceInterval, digits=digits), collapse=", "), "]\n"))
  if(length(x$order)>0) {
    cat("\nCorrelation Levels:\n")
    for(var in 1:length(x$order)) {
      cat(paste0("  Levels for Variable '",x$variables[[var]], "' (Lowest level first):\n"))
      for(i in 1:length(x$order[[var]])) {
        cat(paste0("    ",x$order[[var]][i], "\n"))
      } # end of i in 1:length(x$order[[var]])
    } # End of for(var in 1:length(x$order))
  } # End of length(x$order)>0
}

FisherZTrans <- function() {
  list(name="Fisher Z",
       trans = atanh,
       itrans = tanh,
       setrans = function(z, Zse) {
                   sqrt(Zse^2 * (1-tanh(z)^2)^2)
                 }
      )
}

identityTrans <- function() {
  list(name="identity",
       trans = identity,
       itrans = identity,
       setrans = function(Z, Zse) {
                   identity(Zse)
                 }
      )
}
