#' Draw plausible values from an mml fit
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} to which the plausible values will be added. 
#' @param mml a fit from a call to \code{\link{mml.sdf}}, or a \code{summary.mml.sdf}, which is a  \code{summary} of 
#' \code{mml.sdf} call. 
#' @param npv integer indicating the number of plausible values to draw
#' @param stochasticBeta logical when \code{TRUE} the regressopm coefficients will be drawn from their posterior distribution. Can also be a data frame of values (see Details).
#' @param returnPosterior logical set to \code{TRUE} to change output to include two additional data frames (see Value).
#' @param ... additional parameters
#' @importFrom Dire drawPVs
#' @export
drawPVs.sdf <- function(data, mml, npv=5L,
                        stochasticBeta=FALSE, normalApprox=TRUE, newStuDat=NULL,
                        newStuItems=NULL, returnPosterior=FALSE,
                        construct=NULL, ...) {
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
    getDataArgs <- mml$object$getDataArgs
    sCard <- mml$object$sCard
    if("formula" %in% names(mml$object$Call)) {
      construct0 <- mml$object$Call$formula[[2]]
    }
  } else{
    getDataArgs <- mml$getDataArgs
    sCard <- mml$sCard
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
  newDat <- checkNewData(mml, sCard, data, getDataArgs, newStuDat, newStuItems)
  #draw plausible values
  pvs <- drawPVs(x=mml,
                 npv=npv,
                 stochasticBeta=stochasticBeta,
                 normalApprox=normalApprox,
                 newStuDat=newDat[['stuDat']],
                 newStuItems=newDat[['stuItems']],
                 returnPosterior=returnPosterior,
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

#' @export
drawPVs.edsurvey.data.frame <- function(data, mml, ...) {
  drawPVs.sdf(data=data, mml=mml, ...)
}

#' @export
drawPVs.light.edsurvey.data.frame <- function(data, mml, ...) {
  drawPVs.sdf(data=data, mml=mml, ...)
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

checkNewData <- function(mml, sCard, data, getDataArgs, newStuDat, newStuItems){
  ### check if new studat / stuitems are given
  if(is.null(newStuDat) || is.null(newStuItems)) {
    # get data from "data" argument
    getDataArgs$data <- data
    # supressWarings so it does not again warn about recodes; user has already seen any warnings
    suppressWarnings(edf <- do.call(getData, getDataArgs))
    stuDatColnames <- getStuDatColnames(mml)
    incomplete <- !complete.cases(edf[ , stuDatColnames])
    edf <- edf[!incomplete, ]
    if(is.null(newStuDat)){
      # make stuDat format
      newStuDat <- edf[ , stuDatColnames]
    }
    if(is.null(newStuItems)){
      if (is.null(sCard)) {
        ### this is TIMSS data
        scoreDict <- getAttributes(data, "scoreDict")
        polyParamTab <- getAttributes(data, "polyParamTab")
        polyParamTab <- polyParamTab[polyParamTab$ItemID %in% colnames(edf), ]
        dichotParamTab <- getAttributes(data, "dichotParamTab")
        dichotParamTab <- dichotParamTab[dichotParamTab$ItemID %in% colnames(edf), ]
        items <- c(polyParamTab$ItemID, dichotParamTab$ItemID)
        # format stuItemsWide
        stuItemsWide <- edf[ , c(mml$idVar, items)]
        stuItemsWide <- data.table(stuItemsWide)
        # if unweighed, then a weight column named "one" with values of 1 was created. recreate that here
        if("one" %in% stuDatColnames & !"one" %in% colnames(edf)) {
          edf$one <- 1
        }

        # scoring 
        edf <- scoreTIMSS(edf, polyParamTab, dichotParamTab, scoreDict)
        # creat stuItems

        newStuItems <- as.data.frame(melt(as.data.table(edf[ , c(polyParamTab$ItemID,dichotParamTab$ItemID, mml$idVar)]),
                                       id.vars=mml$idVar,
                                       measure.vars=c(polyParamTab$ItemID,dichotParamTab$ItemID)))
        colnames(newStuItems) <- c(mml$idVar, 'key', 'score')
      } else {
        ### this is NAEP data
        items <- unique(sCard$key)
        # format stuItemsWide
        stuItemsWide <- edf[ , c(mml$idVar, items)]
        stuItemsWide <- data.table(stuItemsWide)
        # if unweighed, then a weight column named "one" with values of 1 was created. recreate that here
        if("one" %in% stuDatColnames & !"one" %in% colnames(edf)) {
          edf$one <- 1
        }
        # may issue a warning about item classes. Not useful in this context.
        suppressWarnings(stuItemsLong <- melt(stuItemsWide, id.vars=mml$idVar, measure.vars=items))
        colnames(stuItemsLong) <- c('id', 'key', 'answer')
        stuItemsLong <- stuItemsLong[!is.na(answer)]
        stuItems <- merge(stuItemsLong, sCard, by=c('key', 'answer'), all.x=TRUE)  # score through merge
        newStuItems <- as.data.frame(stuItems)[ , c("id", "key", "score")]
        colnames(newStuItems)[1] <- mml$idVar
      }
    }
  }
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

