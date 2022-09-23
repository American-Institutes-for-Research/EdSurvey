#' Draw plausible values from an mml fit
#' @param x a fit from a call to \code{\link{mml.sdf}}, or a \code{summary.mml.sdf}, which is a  \code{summary} of 
#' \code{mml.sdf} call. 
#' @param npv integer indicating the number of plausible values to draw
#' @param pvVariableNameSuffix suffix to append to the name of the new plausible values
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} to which the plausible values will be added and from which the covariates and item responses will be taken to generate them
#' @param stochasticBeta logical when \code{TRUE} the regressopm coefficients will be drawn from their posterior distribution. Can also be a data frame of values (see Details).
#' @param construct the construct to draw PVs for
#' @param ... additional parameters
#' @importFrom Dire drawPVs
#' @export
drawPVs.sdf <- function(x, npv=5L, pvVariableNameSuffix="_dire",
                        data,
                        stochasticBeta=FALSE, 
                        construct=NULL,  ...) {
  mml <- x
  checkDataClass(data, c("light.edsurvey.data.frame", "edsurvey.data.frame")) 
  ### check data class 
  checkDataClass(mml, c("summary.mml.sdf", "mml.sdf"))
  ### check for merge key 
  idVar <- getIdVar(mml)
  if(!idVar %in% colnames(data)) {
    stop(paste0("the ", dQuote("idVar"), " must be on ", dQuote("data"), " to merge on PVs. You must specify it when calling the mml.sdf function"))
  }

  # get mml data, depending whether a summary or mml object was given
  construct0 <- NULL
  if(inherits(mml, "summary.mml.sdf")) {
    theSubject <- mml$object$theSubject
    survey <- mml$object$survey
    getDataArgs <- mml$object$getDataArgs
    scoreDict <- mml$object$scoreDict
    if("formula" %in% names(mml$object$Call)) {
      construct0 <- mml$object$Call$formula[[2]]
    }
  } else{
    theSubject <- mml$theSubject
    survey <- mml$survey
    getDataArgs <- mml$getDataArgs
    scoreDict <- mml$scoreDict
    if("formula" %in% names(mml$Call)) {
      construct0 <- mml$Call$formula[[2]]
    }
  }
  # check that we have a construct
  if(missing(construct)) {
    if(!is.null(construct0)) {
      construct <- construct0
    } else {
      stop(paste0("Construct not assigned and not available from mml object. To name them use the ", dQuote("construct"), " argument."))
    }
  }
  mml <- getMmlDat(mml, stochasticBeta)
  # check if new data was provided, otherwise use original from mml call 
  newDat <- checkNewData(mml, scoreDict, data, getDataArgs, theSubject, survey)
  #draw plausible values
  pvs <- drawPVs(x=mml,
                 npv=npv,
                 stochasticBeta=stochasticBeta,
                 normalApprox=TRUE,
                 newStuDat=newDat[['stuDat']],
                 newStuItems=newDat[['stuItems']],
                 returnPosterior=FALSE,
                 construct=construct,
                 ...)
  newPVDat <- pvs$data
  # merge PVs to ed dataframe
  newPVDat$id <- as.character(newPVDat$id)
  mergedPvs <- mergePVGeneral(data, newPVDat, idVar)
  pvvars0 <- getAttributes(mergedPvs, "pvvars")
  if( any(names(pvvars0) %in% names(pvs$newpvvars)) ) {
    warning("overwriting some constructs already on data")
  }
  pvvars0[names(pvs$newpvvars)] <- pvs$newpvvars
  mergedPvs <- setAttributes(mergedPvs, "pvvars", pvvars0)
  return(mergedPvs)
}

#' @method drawPVs mml.sdf
#' @export
drawPVs.mml.sdf <- function(x, npv=5L, pvVariableNameSuffix="_dire", data, ...) {
  drawPVs.sdf(x=x, npv=npv, pvVariableNameSuffix=pvVariableNameSuffix, data=data, ...)
}
#' @method drawPVs summary.mml.sdf
#' @export
drawPVs.summary.mml.sdf <- function(x, npv=5L, pvVariableNameSuffix="_dire", data, ...) {
  drawPVs.sdf(x=x, npv=npv, pvVariableNameSuffix=pvVariableNameSuffix, data=data, ...)
}

mergePVGeneral <- function(x, pvs, idVar){
  x$MergePVmergeID___ <- as.character(x[[idVar]])
  if(inherits(x,'light.edsurvey.data.frame')){
    xInput <- x
    x <- merge(as.data.frame(x), pvs, by.x = idVar, by.y = "id", all.x=TRUE, all.y=FALSE)
    x <- rebindAttributes(x, xInput)
  } else {
    if(inherits(x,'edsurvey.data.frame')){
      mg <- merge(as.data.frame(x$cache), pvs, by.x = "MergePVmergeID___", by.y = "id", all.x = TRUE, all.y = FALSE) #ensure the cache retains it's full dimensions in the sdf
      if(nrow(mg) > nrow(x$cache)) {
        stop("some IDs were on the new data multiple times. Cannot merge plausible values on to data. Check that IDs are unique.")
      }
      x$cache <- mg[order(mg$ROWID), ]
    }
  }
  x$MergePVmergeID___ <- NULL
  return(x)
}

checkNewData <- function(mml, scoreDict, data, getDataArgs, theSubject, survey){
  ### check if new studat / stuitems are given
  # get data from "data" argument
  getDataArgs$data <- data
  # supressWarings so it does not again warn about recodes; user has already seen any warnings
  suppressWarnings(edf <- do.call(getData, getDataArgs))

  stuDatColnames <- getStuDatColnames(mml)
  idVar <- mml$idVar
  # fix idVar
  edf[[idVar]] <- as.character(edf[[idVar]])
  # necessary because TIMSS, for example, will generate up to a row per teacher/student pair
  edf <- edf[!duplicated(idVar), ]
  newStuDat <- filterOutIncompleteZeroWeight(edf, stuDatColnames, weightVar=NULL)[ , stuDatColnames]

  scoreInfo <- getScoreInfo(data, survey, theSubject)
  scoreInfo <- checkParamTabAgainstItems(data, scoreInfo)
  scoreCall <- getScoreCall(data)
  scoreCallEnv <- list2env(scoreInfo)
  assign("edf", edf, envir = scoreCallEnv) # add edf to the environment
  edf <- eval(scoreCall, envir=scoreCallEnv)
  newStuItems <- as.data.frame(melt(as.data.table(edf[ , c(scoreInfo$itemsUse, idVar)]),
                                 id.vars=idVar,
                                 measure.vars=c(scoreInfo$itemsUse)))
  colnames(newStuItems) <- c(idVar, 'key', 'score')

  return(list("stuItems" = newStuItems, "stuDat" = newStuDat))
}

getStuDatColnames <- function(x) {
  if(inherits(x, "mml.sdf")) {
    x <- x$mml
  }
  if(inherits(x, "mmlCompositeMeans") | inherits(x, "summary.mmlCompositeMeans")) {
    return(colnames(x$stuDat[[1]]))
  }
  return(colnames(x$stuDat))
}


checkId <- function(x, idVar) {
  if(inherits(x,'light.edsurvey.data.frame')){
    if(!idVar %in% colnames(x)) {
      stop("For a ",sQuote("light.edsurvey.data.frame"), " the ", paste0(sQuote("idVar"), " used in the ", sQuote("mml.sdf"), " call, ", sQuote("y"), 
                                                                         " must be in ", sQuote("x"), "."))
    } 
  } else {
    if(inherits(x,'edsurvey.data.frame')){
      if(!idVar %in% colnames(x$cache)) {
        stop("For a ",sQuote("edsurvey.data.frame"), " the ", paste0(sQuote("idVar"), " used in the ", sQuote("mml.sdf"), " call, ", sQuote("y"), 
                                                                           " must be in ", sQuote("x"), "."))
      } 
    }
  }
}


getIdVar <- function(y){
  if(inherits(y, "mml.sdf")){
   return(y$mml$idVar)
  } else {
    ### summar.mml.sdf
    if(inherits(y, "summary.mml.sdf")){
      return(y$Summary$idVar)
    } 
  }
}

getMmlDat <- function(x, stochasticBeta) {
  ### mml.sdf case 
  if(inherits(x, "mml.sdf")){
    if(stochasticBeta == TRUE) {
      ### stochasticBeta only works for summary.mml.sdf
      stop("Stochastic Beta requires class of type ", sQuote("summary.mml.sdf"), ". Try: ", sQuote("drawPVs.sdf(summary(x), stochasticBeta=TRUE)"), ".")
    } else {
      return(x$mml)
    }
  } else {
    ### summar.mml.sdf
    if(inherits(x, "summary.mml.sdf")){
      return(x$Summary)
    } 
  } 
}

