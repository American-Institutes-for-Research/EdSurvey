#' @title Connect to NAEP Data
#'
#' @description Opens a connection to a Main NAEP, or Long-Term Trend NAEP data file residing
#'              on the disk. Returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value indicating the full filepath location and name
#'             of the (.dat) data file
#' @param defaultWeight a character value that indicates the default weight
#'                      specified in the resulting \code{edsurvey.data.frame}.
#'                      Default value is \code{origwt} if not specified.
#' @param defaultPvs a character value that indicates the default plausible value
#'                   specified in the resulting \code{edsurvey.data.frame}.
#'                   Default value is \code{composite} if not specified.
#' @param omittedLevels a character vector indicating which factor levels/labels
#'                      should be excluded. When set to the default value of
#'                      \code{c('Multiple',NA,'Omitted')}, adds the vector to
#'                      the \code{edsurvey.data.frame}.
#' @param frPath a character value indicating the file location of the \code{.fr2}
#'               parameter layout file included with the data companion to
#'               parse the specified \code{path} data file.
#'               The default value of \code{NULL} will attempt to search the parent directory for the corresponding \code{.fr2} file for the specified \code{path} data file.
#' @param xmlPath a character value indicating the file path of the \code{.xml} parameter layout file included as part of the NAEPEX companion. This file provides necessary
#'                 information required to read and parse the (.dat) data file. The default value of \code{NULL} will attempt to search
#'                 the parent directory for the corresponding \code{.xml} file for the specified \code{path} data file.
#' 
#' @details
#' The \code{frPath} file layout information will take precedence over the \code{xmlPath} file when the \code{xmlPath} is not explicitly set, or when the \code{xmlPath} file cannot be located.
#'
#' The \code{readNAEP} function includes both scaled scores and theta scores, with the latter having names ending in \code{\_theta}.
#'
#' When a NAEP administration includes a linking error variable those variables are included and end in \code{_linking}.
#' When present, simply use the \code{_linking} version of a variable to get a standard error estimate that includes linking error.
#'
#' This function supports both the Main NAEP data files, and Long-Term Trend NAEP data files.
#' A table outlining the differences can be found on the \href{https://nces.ed.gov/nationsreportcard/about/ltt_main_diff.aspx}{NAEP Nations Report Card website}.
#'
#' @return An \code{edsurvey.data.frame} for a NAEP data file.
#'
#' @seealso \code{\link{edsurvey.data.frame}} \code{\link{getData}}
#' @author Tom Fink and Ahmad Emad
#' @example \man\examples\readNAEP.R
#' @importFrom xml2 read_xml as_list xml_find_all
#' @export
readNAEP <- function(path, defaultWeight = "origwt", defaultPvs = "composite", omittedLevels = c('Multiple',NA,'Omitted'), frPath = NULL, xmlPath = NULL) {

  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  filePathInfo <- validate_NAEPFilePaths(path, frPath, xmlPath)

  #check if this is a long-term trend file
  isLTT <- grepl("long.*term.*trend", filePathInfo$FileDesc$Assessment_Code, ignore.case = TRUE)
  dataSchLaf <- NULL #default
  schLabelsFile <- NULL

  if(isLTT){

    if(filePathInfo$PreferredLayoutMethod == "XML"){
      xmlLayout <- parseNAEP_XML(filePathInfo$StudentXML)
      labelsFile <- xmlLayout$FileFormat #only assign the 'FileFormat", as xml returns a list object with much more data
    } else { #FR2
      labelsFile <- readMRC_LTT(filePathInfo$StudentFR2, filePathInfo$FileDesc$Subject) #FR2 FILE
    }

    #build the weights and pvvars list
    weights <- buildNAEP_LTT_WeightList(labelsFile)
    pvs <- buildNAEP_LTT_PVList(labelsFile)

    dataLaf <- laf_open_fwf(filename=filePathInfo$StudentDAT, column_types=labelsFile$dataType,
                            column_names=labelsFile$variableName, column_widths=as.numeric(labelsFile$Width))

    #in some rare cases the LaF cleaned names differ from the fileFormat, ensure they are consistent
    labelsFile$variableName <- names(dataLaf)

  }else{ #parse as main NAEP data file
    if(filePathInfo$PreferredLayoutMethod == "XML"){
      xmlLayout <- parseNAEP_XML(filePathInfo$StudentXML)
      labelsFile <- xmlLayout$FileFormat #only assign the 'FileFormat", as xml returns a list object with much more data
    } else { #FR2
      labelsFile <- readMRC(filePathInfo$StudentFR2) #FR2 FILE
    }

    if(!is.null(filePathInfo$SchoolDAT)) {
      if(filePathInfo$PreferredLayoutMethod == "XML"){
        xmlLayoutSch <- parseNAEP_XML(filePathInfo$SchoolXML)
        schLabelsFile <- xmlLayoutSch$FileFormat #only assign the 'FileFormat", as xml returns a list object with much more data
      } else { #FR2
        schLabelsFile <- readMRC(filePathInfo$SchoolFR2) #FR2 FILE
      }

      schLabelsFile <- schLabelsFile[order(schLabelsFile$Start),]
      widths <- schLabelsFile$Width
      dataTypes <- as.character(schLabelsFile$dataType)
      varNames = as.character(tolower(schLabelsFile$variableName))
      dataSchLaf <- laf_open_fwf(filename=filePathInfo$SchoolDAT, column_types=dataTypes,
                                 column_names=varNames, column_widths=widths)
    }
    labelsFile <- labelsFile[order(labelsFile$Start),]
    widths <- labelsFile$Width
    dataTypes <- as.character(labelsFile$dataType)
    varNames = as.character(tolower(labelsFile$variableName))

    dataLaf <- laf_open_fwf(filename=filePathInfo$StudentDAT, column_types=dataTypes,
                            column_names=varNames, column_widths=widths)

    #Defining PVs and JKs
    ##############################################################
    ## Accomodation not permitted weights and PVs
    pvs = list()
    pv_subset <- subset(labelsFile, select = c('Type','variableName'), labelsFile$Labels %in% c("PV", "PV2", "PVT"))
    uniquePvTypes = unique(pv_subset$Type)
    for (i in uniquePvTypes) {
      vars <- tolower(pv_subset$variableName[pv_subset$Type == i])
      temp_list <- list(varnames = vars)
      pvs[[i]] <- temp_list
    }

    #gets the weights (can have 'origwt' or 'aorigwt' or both)
    weights <- list()
    if(sum("JK" %in% labelsFile$Labels) > 0){ #origwt
      weight_temp <-  tolower(varNames[labelsFile$Labels == "JK"])
      jksuffix <- gsub("[^0-9]","", weight_temp)
      base <- gsub(jksuffix[1],"", weight_temp[1])
      weights[["origwt"]] <- list(jkbase = base, jksuffixes = jksuffix)
    }
    if(sum("JK2" %in% labelsFile$Labels) > 0){ #aorigwt (a = accomodations)
      weight_temp <-  tolower(varNames[labelsFile$Labels == "JK2"])
      jksuffix <- gsub("[^0-9]","", weight_temp)
      base <- gsub(jksuffix[1],"", weight_temp[1])
      weights[["aorigwt"]] <- list(jkbase = base, jksuffixes = jksuffix)
    }

  }#end if(isLTT)

  # set default weight
  if(missing(defaultWeight) || !any(defaultWeight %in% names(weights))) {
    attributes(weights)$default <- names(weights)[1]
  } else {
    attributes(weights)$default <- defaultWeight
  }

  if(all(is.null(defaultPvs)) || all(is.na(defaultPvs))){
    warning(paste0("Argument ", dQuote("defaultPvs"), " not specified. There will not be a default PV value."))
  } else {
    if(!defaultPvs[1] %in% names(pvs)){
      defPV <- names(pvs[1])
      defi <- 1
      while( grepl("theta", defPV, fixed=TRUE) && length(pvs) > defi){
        defi <- defi + 1
        defPV <- names(pvs[defi])
      }
      if(length(pvs)>0){
        warning(paste0("Updating name of default plausible value since ",sQuote(defaultPvs[1]), " not found. Setting to ", sQuote(defPV), "."))
        defaultPvs <- defPV
      } else {
        warning(paste0("No plausible values found. If plausible value(s) expected, check the ", sQuote("defaultPvs"), " argument."))
      }
    }#end if(!defaultPvs %in% names(pvs))
  }

  if(length(pvs) > 0) {
    attributes(pvs)$default <- defaultPvs[1]
  }

  #achievementLevelsHelp located in descriptionOfFile.R
  #each file will only have one achievement level scale associated with the file
  levels <- achievementLevelsHelp(filePathInfo$FileDesc$Grade_Level,
                                  filePathInfo$FileDesc$Year,
                                  filePathInfo$FileDesc$Subject,
                                  filePathInfo$FileDesc$Assessment_Code)

  #apply the achievement levels only to the non-theta pvs
  #levels will only have one row here for NAEP data files
  for(i in seq_along(pvs)){
    if(!grepl("_theta$", names(pvs)[i], ignore.case = TRUE)){
      pvs[[i]]$achievementLevel <- levels #there will be only one value in levels
    }
  }

  #convert achievementLevels to list, this bypasses the automatic apply to all pvs
  levelName <- filePathInfo$FileDesc$Subject
  levels <- list(levels)
  names(levels) <- levelName

  # add reporting sample default condition if the column exists
  if("rptsamp" %in% tolower(names(dataLaf))) {
    defaultConditions <- quote(tolower(rptsamp)=="reporting sample")
  }
  else {
    defaultConditions <- NULL
  }

  #get psu and stratum variables for Taylor series variance
  checkVars <- tolower(labelsFile$variableName)
  psCheck <- TRUE
  if(psCheck && all(c("jkpair", "jkrepl") %in% checkVars)){
    ps <- c(stratum="jkpair", psu="jkrepl")
    psCheck <- FALSE
  }
  if(psCheck && all(c("repgrp", "dropwt") %in% checkVars)){
    ps <- c(stratum="repgrp", psu="dropwt")
    psCheck <- FALSE
  }
  if(psCheck && all(c("jkpair", "jkunit") %in% checkVars)){
    ps <- c(stratum="jkpair", psu="jkunit")
    psCheck <- FALSE
  }
  #note: this is the default psu/stratum vars for all normal NAEP files and newer LTT files
  if(psCheck && all(c("repgrp1", "jkunit") %in% checkVars)){
    ps <- c(stratum="repgrp1", psu="jkunit")
    psCheck <- FALSE
  }
  if(psCheck){ #no match was found!
    ps <- NULL
  }
  if(all(is.null(ps)) || all(is.na(ps))) {
    psuVar <- NULL
    stratumVar <- NULL
  } else {
    psuVar <- ps["psu"]
    stratumVar <- ps["stratum"]
  }

  #provide more detail as part of the assessment code for users
  assessCode <- filePathInfo$FileDesc$Assessment_Code
  if(grepl(".*", filePathInfo$FileDesc$Assessment_Sample)){
    assessCode <- paste0(assessCode, " - ", filePathInfo$FileDesc$Assessment_Sample)
  }
  # build the result list and return
  res <- edsurvey.data.frame(userConditions = list(),
                      defaultConditions = list(defaultConditions),
                      dataList = buildNAEP_dataList(dataLaf,
                                                    labelsFile,
                                                    dataSchLaf,
                                                    schLabelsFile),
                      weights = weights,
                      pvvars = pvs,
                      subject = filePathInfo$FileDesc$Subject,
                      year = filePathInfo$FileDesc$Year,
                      assessmentCode = assessCode,
                      dataType = filePathInfo$FileDesc$Data_Type,
                      gradeLevel = filePathInfo$FileDesc$Grade_Level,
                      achievementLevels = levels,
                      omittedLevels = omittedLevels,
                      survey = "NAEP",
                      country = "USA",
                      psuVar = psuVar,
                      stratumVar = stratumVar,
                      jkSumMultiplier = 1,
                      fr2Path=ifelse(filePathInfo$PreferredLayoutMethod=="XML", filePathInfo$StudentXML, filePathInfo$StudentFR2)
                      )


  f <- filePathInfo$FileDesc #reassign for the shorthand below
  #apply the linking-error parameters
  if("dbapba" %in% colnames(res)) {
    foundScale <- FALSE
    if(res$subject == "Science") {
      foundScale <- TRUE
      subscaleWeights <- c(1,0,0,0)
      subscales <- c("univariate_scale", "physical", "earth", "life")
      composite <- c("univariate_scale")
    }
    if(res$subject == "Civics") {
      foundScale <- TRUE
      subscaleWeights <- 1
      subscales <- c("civics")
      composite <- c("civics")
    }
    if(res$subject == "Geography") {
      if(f[["Grade_Level"]] %in% c("Grade 4", "Grade 8", "Grade 12")) {
        foundScale <- TRUE
        subscaleWeights <- c(0.40, 0.30, 0.30)
        subscales <- c("spaceplace", "envsociety", "spatial_dyn")
        composite <- c("composite")
      }
    }
    if(res$subject == "History") {
      if(f[["Grade_Level"]] == "Grade 4") {
        foundScale <- TRUE
        subscaleWeights <- c(0.25, 0.35, 0.25, 0.15)
        subscales <- c("democracy", "cultures", "technology", "world_role")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 8") {
        foundScale <- TRUE
        subscaleWeights <- c(0.3, 0.30, 0.20, 0.20)
        subscales <- c("democracy", "cultures", "technology", "world_role")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 12") {
        foundScale <- TRUE
        subscaleWeights <- c(0.25, 0.25, 0.25, 0.25)
        subscales <- c("democracy", "cultures", "technology", "world_role")
        composite <- c("composite")
      }
    }
    if(res$subject == "Mathematics") {
      if(f[["Grade_Level"]] == "Grade 4") {
        foundScale <- TRUE
        subscaleWeights <- c(0.40, 0.10, 0.15, 0.2, 0.15)
        subscales <- c("num_oper", "da_stat_prob", "algebra", "measurement", "geom")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 8") {
        foundScale <- TRUE
        subscaleWeights <- c(0.2, 0.15, 0.30, 0.15, 0.20)
        subscales <- c("num_oper", "da_stat_prob", "algebra", "measurement", "geom")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 12") {
        foundScale <- TRUE
        subscaleWeights <- c(0.1, 0.25, 0.35, 0.3)
        subscales <- c("num_oper", "da_stat_prob", "algebra", "measurementgeom")
        composite <- c("composite")
      }
    }
    if(res$subject == "Reading") {
      if(f[["Grade_Level"]] == "Grade 4") {
        foundScale <- TRUE
        subscaleWeights <- c(0.50, 0.50)
        subscales <- c("literary", "information")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 8") {
        foundScale <- TRUE
        subscaleWeights <- c(0.45, 0.55)
        subscales <- c("literary", "information")
        composite <- c("composite")
      }
      if(f[["Grade_Level"]] == "Grade 12") {
        foundScale <- TRUE
        subscaleWeights <- c(0.30, 0.70)
        subscales <- c("literary", "information")
        composite <- c("composite")
      }
    }
    if(res$subject == "Geography") {
      if(f[["Grade_Level"]] %in% c("Grade 4", "Grade 8", "Grade 12")) {
        foundScale <- TRUE
        subscaleWeights <- c(0.40, 0.30, 0.30)
        subscales <- c("spaceplace", "envsociety", "spatial_dyn")
        composite <- c("composite")
      }
    }
    if(!foundScale) {
      warning("Could not find construct weights for this NAEP. Linking error cannot be calculated.")
    } else {
      res <- linkVarAugment(res, subscaleWeights=subscaleWeights, subscales=subscales, composite=composite)
    }
  } # end if("dbapba" %in% colnames(res))

  #applicable for main NAEP at this point
  if(!isLTT) {
    
    if(filePathInfo$PreferredLayoutMethod=="XML"){
      #XML file already contains the IRT parameters and the XML parser returns the formats as needed for EdSurvey
      res$scoreCard = xmlLayout$IRTParams$ScoreCard
      res$dichotParamTab = xmlLayout$IRTParams$DichotParamTab
      res$polyParamTab = xmlLayout$IRTParams$PolyParamTab
      res$adjustedData = xmlLayout$IRTParams$AdjsutedData
      res$testData = xmlLayout$IRTParams$TestData
      res$scoreDict = xmlLayout$IRTParams$ScoreDict
      res$scoreFunction = scoreDefault
    }else{
      res <- appendIRTAttributes_NAEP(esdf = res,
                                      year = filePathInfo$FileDesc$Year,
                                      subject = filePathInfo$FileDesc$Subject,
                                      region = filePathInfo$FileDesc$Assessment_Code,
                                      level = filePathInfo$FileDesc$Grade_Level,
                                      fr2Path = filePathInfo$StudentFR2,
                                      filePathInfo = filePathInfo)
    }
  }


  return(res)
}

linkVarAugment <- function(data, subscaleWeights, subscales, composite) {
  PVVars <- getAttributes(data = data, attribute = "pvvars")
  PVVarsDefault <- attributes(PVVars)$default
  # actual variables
  scaledPVs <- list()
  thetaPVs <- list()
  for(i in 1:length(subscales)) {
    scaledPVs <- c(scaledPVs, list(getPlausibleValue(subscales[i], data = data)))
    thetaPVs <- c(thetaPVs, list(getPlausibleValue(paste0(subscales[i], "_theta"), data = data)))
  }
  PVs <- getPlausibleValue(composite, data = data)
  # use default weights
  W <- attr(getAttributes(data = data, attribute = "weights"), "default")
  repWs <- getWeightJkReplicates(W, data)

  stratumVar <- getStratumVar(data = data)
  PSUvar <- getPSUVar(data)
  # the DBA subset
  if(!"dbapba" %in% colnames(data)) {
    stop(paste0("Cannot find variable ", dQuote("dbapba"), " on data."))
  }
  if(!stratumVar %in% colnames(data)) {
    stop(paste0("Cannot find variable ", dQuote("stratumVar"), " on data."))
  }
  if(!PSUvar %in% colnames(data)) {
    stop(paste0("Cannot find variable ", dQuote("PSUvar"), " on data."))
  }
  DBASS <- data$dbapba %in% "DBA"
  nDBA <- sum(DBASS)  # number in DBA subset
  # the PBA subset
  PBASS <- data$dbapba %in% "PBA"
  nPBA <- sum(PBASS) # number in the PBA subset
  if(nDBA + nPBA < length(data$dbapba)) {
    warning("Some rows are neither PBA nor DBA.")
  }

  #  a map from the DBA PV to the PBA PB
  RAM <- getRAM()
  # number of subscales
  K <- length(subscaleWeights)
  if(K != length(scaledPVs)) {
    stop(paste0("The argument ", dQuote("scaledPVs"), " is a list with one element per subscale, so it should have the same length as ", dQuote("subscaleWeights"), " and ",dQuote("thetaPVs"),". Each element of ", dQuote("scaledPVs"), " is a vector of plausible values."))
  }
  if(K != length(thetaPVs)) {
    stop(paste0("The argument ", dQuote("thetaPVs"), " is a list with one element per subscale, so it should have the same length as ", dQuote("subscaleWeights"), " and ",dQuote("scaledPVs"),". Each element of ", dQuote("thetaPVs"), " is a vector of plausible values."))
  }
  if(length(unique(sapply(c(thetaPVs, scaledPVs, list(PVs)), length) )) != 1) {
    stop(paste0("The arguments ", dQuote("thetaPVs"), " and ", dQuote("scaledPVs"), " are lists with one element per subscale, each a vector of the same length. Each vector should also be the same length as the vector ", dQuote("PVs"), "."))
  }
  # subscale weights called "beta" in documentation
  beta <- subscaleWeights
  # DBA weights
  wd <- data[DBASS, W]
  # PBA weights
  wp <- data[PBASS, W]

  ### measurement variance agumentation
  # uses full sample weights and varries PVs according to the RAM (Table 1, page 3)

  # create PV data frame to minimize calls to getData
  pvData <- data[,PVs]

  # for each PV (column in the RAM)
  RAMi <- 1
  thetaPVscores <- data[DBASS, unlist(thetaPVs)]
  scaledPVscores <- data[PBASS, unlist(scaledPVs)]
  repWsDBA <- data[DBASS, repWs]
  repWsPBA <- data[PBASS, repWs]
  for(n in 1:ncol(RAM)) {
    # for each RAM column (5)
    for(j in 1:nrow(RAM)) {
      ynj <- rep(0, nDBA)
      znj <- rep(0, nPBA)
      # RAM says which PBA PV to use
      PBApv <- RAM[j, n]
      for(k in 1:K) {
        # DBA PV is the jth one
        # scaledPVs[[k]] is for the kth subscale, in that [j] is for the jth plausible value
        theta_k <- thetaPVscores[ , thetaPVs[[k]][j]]
        # PBApv from RAM
        # PBAPVs[[k]], similar to DBA, inside of that we select the plausilbe value from the RAM for row j
        x_k <- scaledPVscores[ , scaledPVs[[k]][PBApv] ]
        # mean, std estimates for DBA (theta scale)
        mu_t <- getMu(theta_k, wd)
        s_t <- getS(theta_k, wd, mu=mu_t)
        # mean, std estimates for PBA (reporting scale)
        mu_x <- getMu(x_k, wp)
        s_x <- getS(x_k, wp, mu=mu_x)
        # variable transformations
        a <- s_x/s_t
        b <- mu_x - a * mu_t
        data[DBASS, paste0(subscales[k], "_linking_imp_", nrow(RAM)*(n-1)+j)] <- (a*theta_k + b)
        data[PBASS, paste0(subscales[k], "_linking_imp_", nrow(RAM)*(n-1)+j)] <- x_k
        # y is the sum over the k subscales
        ynj <- ynj + beta[k] * (a*theta_k + b)
        znj <- znj + beta[k] * x_k
      }
      # write composite variable for DBA
      data[DBASS, paste0(composite, "_linking_imp_", nrow(RAM)*(n-1)+j)] <- ynj
      RAMi <- RAMi + 1
      # write composite variable for PBA
      compositePBA <- pvData[PBASS ,PVs[PBApv]]
      data[PBASS, paste0(composite, "_linking_imp_", nrow(RAM)*(n-1)+j)] <- compositePBA
      if(max(abs(znj - compositePBA)) > 0.02) {
        warning("Inacurate data or subscale weights, linking error is inacurately estimated due to low precision inputs. Disagreement on composite of ", max(abs(znj - data[PBASS, paste0("comp", nrow(RAM)*(n-1)+j)])), "\n")
      }
    }
  }

  for(i in 1:length(repWs)) {
    # y1 is for DBA
    y1 <- rep(0, nDBA)
    # z1 is for PBA
    z1 <- rep(0, nPBA)
    # DBA ith replicate weight
    wd <- repWsDBA[ , repWs[i]]
    # PBA ith replicate weight
    wp <- repWsPBA[ , repWs[i]]
    for(k in 1:K) {
      # DBA is theta scale
      theta_k <- thetaPVscores[ , thetaPVs[[k]][1]]
      # get mean, stdev, DBA, use ith replicate weight
      mu_t <- getMu(theta_k, wd)
      s_t <- getS(theta_k, wd, mu=mu_t)
      # get mean, stdev, PBA, use ith replicate weight
      x_k <- scaledPVscores[ , scaledPVs[[k]][1] ]
      mu_x <- getMu(x_k, wp)
      s_x <- getS(x_k, wp, mu=mu_x)
      # variablle transformations
      a <- s_x/s_t
      b <- mu_x - a * mu_t
      data[ DBASS, paste0(subscales[k], "_linking_samp_", i)] <- (a*theta_k + b)
      data[!DBASS, paste0(subscales[k], "_linking_samp_", i)] <- x_k
      # sum y1
      y1 <- y1 + beta[k] * (a*theta_k + b)
      z1 <- z1 + beta[k] * x_k
    }
    data[DBASS, paste0(composite, "_linking_samp_", i)] <- y1

    compositePBA <- pvData[PBASS ,PVs[1]]
    data[PBASS, paste0(composite, "_linking_samp_", i)] <- compositePBA#z1
    if(max(abs(z1 - compositePBA)) > 0.02) {
      warning("Inacurate data or subscale weights, linking error is inacurately estimated due to low precision inputs. Disagreement on composite of ", max(abs(z1 - data[PBASS, paste0(composite, "_linking_samp_", i)])), "\n")
    }
  }
  # final RAM index
  RAMiFinal <- RAMi -1
  # construct composite PV var
  newPV <- list(list(estVarnames=PVs,
                     impVarnames=paste0(composite,"_linking_imp_",1:RAMiFinal),
                     sampVarnames=paste0(composite,"_linking_samp_",1:length(repWs)),
                     achievementLevel=data[["pvvars"]][[composite]][["achievementLevel"]]))
  names(newPV) <- paste0(composite, "_linking")
  for(k in 1:K) {
    # sometimes
    if(subscales[k] != composite) {
      newPVi <- list(estVarnames=scaledPVs[[k]],
                     impVarnames=paste0(subscales[k],"_linking_imp_",1:RAMiFinal),
                     sampVarnames=paste0(subscales[k],"_linking_samp_",1:length(repWs)))
      newPV[[paste0(subscales[k], "_linking")]] <- newPVi
	}
  }
  pvv <- c(data[["pvvars"]], newPV)
  attributes(pvv)$default <- PVVarsDefault
  data[["pvvars"]] <- pvv
  return(data)
}

# get RAM table for how the two sets of plausible values are permuted to approximate
# integration over the entire possible set of 20! cases.
getRAM <- function() {
  RAM <- matrix(NA,nrow=20,ncol=5)
  RAM[,1] <- 1:20
  RAM[,2] <- c(14, 11, 18, 15, 20, 17,  3, 16, 12,  6, 19, 2,  1, 13,  5,  7,  4,  9,  8, 10)
  RAM[,3] <- c( 6,  4,  3,  9, 11, 10,  2, 17, 19, 13,  7, 8, 15, 18, 20, 12,  5, 16, 14,  1)
  RAM[,4] <- c( 2, 14,  7,  9, 20, 13, 16,  8,  4, 11, 19, 1, 18, 10,  6, 12, 15,  3,  5, 17)
  RAM[,5] <- c(19, 10, 13,  8, 17, 12,  1, 11,  5,  6, 18, 9, 15,  2,  3,  7, 14, 20, 16,  4)
  return(RAM)
}

# @author Paul Bailey & Ahmad Emad
#this function
readMRC <- function(filename) {
  # read NAEP machine readable file.
  # this has layout information on it
  t <- try(mrcFile <- readLines(filename), silent=TRUE)

  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=TRUE)
  mrcFile <- unlist(mrcFile)

  # read in the variables from the file,t his is based on the information ETS
  # shared with us
  if( any(nchar(mrcFile) < 90) ) {
    stop(paste0("Malformed fr2 file: some lines have fewer than the required 90 characters. Lines ", pasteItems(which(nchar(mrcFile)<90)), "."))
  }
  
  specs <- getNAEP_FR2Specs(mrcFile)
  variableName <- specs$variableName # name of the variable
  Start <- specs$Start # start column in file
  Width <- specs$Width # number of characters in variable
  End <- specs$End # end column in file
  Decimal <- specs$Decimal # digits (from the right) to be considered decimals
  Labels <- specs$Labels # variable label
  NumValue <- specs$NumValue # number of numeric codes (e.g. one could would be 1="Yes")
  
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }

  # parse the numeric codes
  labelValues <- specs$labelValues

  # keep the original labels
  oLabels <- Labels
  # Finding the plausible weights and jacknife replicates.
  # normally there is just one set of PVs (with multiple subjects or subscales).
  # When there are multiple sets then one starts with an "A" and is the
  # accommodations permitted values
  # main PVs:
  Labels[grepl("plausible", tolower(Labels)) & grepl("value", tolower(Labels)) & "A" != substring(variableName, 1, 1)] <- "PV"
  # theta PVs, these do not say "value"
  Labels[grepl("plausible", tolower(Labels)) & grepl("theta", tolower(Labels))                                       ] <- "PVT"
  # accommodations perimted PVs:
  Labels[grepl("plausible", tolower(Labels)) & grepl("value", tolower(Labels)) & "A" == substring(variableName, 1, 1)] <- "PV2"

  # normally there is just one set of weights. When there are multiple sets
  # then one starts with an "A" and is the accommodations permitted values
  Labels[grepl("weight", tolower(Labels)) & grepl("replicate", tolower(Labels)) & "A" != substring(variableName, 1, 1)] <- "JK"
  Labels[grepl("weight", tolower(Labels)) & grepl("replicate", tolower(Labels)) & "A" == substring(variableName, 1, 1)] <- "JK2"

  pvWt <- character(length(mrcFile)) # the number of the PV (e.g. for a subject or subscale with five PVs this would show values from 1 to 5 for five variables)
  Type <- character(length(mrcFile)) # this is the subject or subscale

  # Check if there is at least one Plausible Value, and then finding their names
  tempValue <- applyPV("PV", Labels, pvWt, oLabels, Type)
  Labels <- tempValue[["Labels"]]
  pvWt <- tempValue[["pvWt"]]
  Type <- tempValue[["Type"]]

  # note theta PVs
  tempValue <- applyPV("PVT", Labels, pvWt, oLabels, Type)
  Labels <- tempValue[["Labels"]]
  pvWt <- tempValue[["pvWt"]]
  Type <- tempValue[["Type"]]

  # note AP PVs
  tempValue <- applyPV("PV2", Labels, pvWt, oLabels, Type)
  Labels <- tempValue[["Labels"]]
  pvWt <- tempValue[["pvWt"]]
  Type <- tempValue[["Type"]]
  # Check if there is at least one JK replicate.

  if(sum(Labels == "JK")>0) {
    # get the number of the JK replicate
    pvWt[Labels == "JK"] <- as.numeric(gsub("[^\\d]+", "", oLabels[Labels=="JK"], perl=TRUE))
    pvWt[Labels == "JK2"] <- as.numeric(gsub("[^\\d]+", "", oLabels[Labels=="JK2"], perl=TRUE))
  }

  # identify weights
  labels <- tolower(Labels)
  weights <- ifelse(4*grepl("origwt", variableName, ignore.case=TRUE) + grepl("wgt", variableName) + grepl("student", labels) + grepl("weight", labels) + grepl("unadjusted", labels) + grepl("overall", labels) + grepl("unpoststratified", labels) - 5 * grepl("replicate", labels) >= 4, TRUE, FALSE)
  # For now, assume all variables are characters.
  dataType <- rep("character", length(mrcFile))
  # Create appropriate data type for variables.
  dataType[Decimal >0 & Width < 8] <- "integer"
  dataType[Decimal >0 & Width >= 8] <- "numeric"
  labelValues <- as.character(labelValues)
  mrcFileCSV <- data.frame(variableName, Start, End, Width, Decimal, Labels, labelValues, pvWt, Type, dataType, weights, stringsAsFactors=FALSE)
  mrcFileCSV <- mrcFileCSV[order(mrcFileCSV$Start),]
  #mrcFileCSV$labelValues <- as.character(mrcFileCSV$labelValues)

  mrcFileCSV$variableName <- make.names(mrcFileCSV$variableName, unique = TRUE) #make.names will ensure names match to LaF object and are valid for R
  return(mrcFileCSV)
}

# @author Tom Fink
# parses the MRC file and returns a file format data.frame specifically for the long-term trend data files/layout
# this is to ensure the original readMRC function doesn't break compatibility for previously QCed data files
readMRC_LTT <- function(filename, subject) {
  # read NAEP machine readable file.
  # this has layout information on it
  t <- try(mrcFile <- readLines(filename), silent=TRUE)

  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=TRUE)
  mrcFile <- unlist(mrcFile)

  # read in the variables from the file,t his is based on the information ETS
  # shared with us
  if( any(nchar(mrcFile) < 90) ) {
    stop(paste0("Malformed fr2 file: some lines have fewer than the required 90 characters. Lines ", pasteItems(which(nchar(mrcFile)<90)), "."))
  }
  
  specs <- getNAEP_FR2Specs(mrcFile)
  variableName <- specs$variableName # name of the variable
  Start <- specs$Start # start column in file
  Width <- specs$Width # number of characters in variable
  End <- specs$End # end column in file
  Decimal <- specs$Decimal # digits (from the right) to be considered decimals
  Labels <- specs$Labels # variable label
  NumValue <- specs$NumValue # number of numeric codes (e.g. one could would be 1="Yes")
  
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
  
  # parse the numeric codes
  labelValues <- specs$labelValues

  labels <- tolower(Labels)
  weights <- rep(FALSE, times = length(variableName)) #default to all false, to be calculated later
  pvWt <- rep("", times = length(variableName))
  Type <- rep("", times = length(variableName))

  # For now, assume all variables are characters.
  dataType <- rep("character", length(mrcFile))
  # Create appropriate data type for variables.
  # integer isn't really the right type here, but no way to determine accurate type
  # the default of 'character' should be sufficient
  dataType[Decimal >0 & Width < 8] <- "integer"
  dataType[Decimal >0 & Width >= 8] <- "numeric"

  labelValues <- as.character(labelValues)
  mrcFileCSV <- data.frame(variableName, Start, End, Width, Decimal, Labels, labelValues, pvWt, Type, dataType, weights, stringsAsFactors=FALSE)
  mrcFileCSV <- mrcFileCSV[order(mrcFileCSV$Start),]

  #apply the Weights and PVVars (to identify and mark them)
  mrcFileCSV <- identifyNAEP_LTT_Weights(mrcFileCSV)
  mrcFileCSV <- identifyNAEP_LTT_PV(mrcFileCSV, subject = subject)

  mrcFileCSV$variableName <- make.names(mrcFileCSV$variableName, unique = TRUE) #make.names will ensure names match to LaF object and are valid for R

  return(mrcFileCSV)
}

# identifies the NAEP Long-Term Trend Weights and set's the logical value to TRUE in the fileFormat
# the modified file format object is then returned
identifyNAEP_LTT_Weights <- function(fileFormatDF){
  #check first if origwt is present, 'weight' is sometimes on newer LTT files but not the overall JK weight we want with the replicates
  isWgt <- grepl("^origwt$", fileFormatDF$variableName, ignore.case = TRUE)
  if(!any(isWgt)){
    isWgt <- grepl("^weight$", fileFormatDF$variableName, ignore.case = TRUE)
  }
  fileFormatDF$weights <- isWgt
  return(fileFormatDF)
}

# returns a list of weights based on the identified weights from the file format for use with package
buildNAEP_LTT_WeightList <- function(fileFormatDF){

  wgtVars <- fileFormatDF$variableName[fileFormatDF$weights==TRUE]
  res <- list()
  #should only be one weight defined, but just in case
  for(i in seq_along(wgtVars)){
    #this is true for all LTT weights. the replicates use the same base variable name regardless of the weight name
    #the number of replicates differs from year to year
    repVars <- grep("^srwt\\d{1,2}$", fileFormatDF$variableName, ignore.case = TRUE, value = TRUE)
    repVars <- gsub("srwt", "", repVars, ignore.case = TRUE) #leave the numeric value only

    res[[i]] <- list(jkbase = "srwt", jksuffixes = repVars)
  }

  names(res) <- tolower(wgtVars)
  #mark the replicate
  return(res)
}

# identifies the NAEP Long-Term Trend plausible values and sets the appropriate pvWT and Type values in the file format
# the modified file format object is then returned
identifyNAEP_LTT_PV <- function(fileFormatDF, subject){

  subject <- tolower(subject)
  subject <- ifelse(subject=="mathematics", "math", subject)
  #RLTTR
  pvvars <- c()
  if(subject == "reading"){
    vars1 <- grep("^(rrp|rth)sct\\d{1,2}$", fileFormatDF$variableName, ignore.case = TRUE, value = TRUE)
    vars2 <- grep("^(theta|lmrval|redtht|redval|rltt(th|rp|r|t))\\d{1,2}$", fileFormatDF$variableName, ignore.case = TRUE, value = TRUE)
    pvvars <- c(vars1, vars2)
  }
  if(subject == "math"){
    vars1 <- grep("^(mrp|mth)sct\\d{1,2}$", fileFormatDF$variableName, ignore.case = TRUE, value = TRUE)
    vars2 <- grep("^(theta|lmmval|mtht|medval|mltt(th|rp|r|t))\\d{1,2}$", fileFormatDF$variableName, ignore.case = TRUE, value = TRUE)
    pvvars <- c(vars1, vars2)
  }

  if(length(pvvars)==0){
    warning(paste0("No plausible values found in this NAEP file."))
  }

  baseVars <- unique(gsub("\\d+$", "", pvvars))

  for(i in seq_along(baseVars)){
    repVars <- pvvars[grepl(paste0("^", baseVars[i]), pvvars, ignore.case = TRUE)]
    repVarsNum <- as.numeric(gsub(baseVars[i],"", repVars, ignore.case = TRUE))
    fileFormatDF$pvWt[fileFormatDF$variableName %in% repVars] <- repVarsNum

    isTheta <- all(grepl("theta", fileFormatDF$Labels[fileFormatDF$variableName %in% repVars], ignore.case = TRUE))

    fileFormatDF$Type[fileFormatDF$variableName %in% repVars] <- ifelse(isTheta,paste0(subject, "_theta"), subject)
  }

  return(fileFormatDF)
}

# build the NAEP Long-Term Trend plausible value list
buildNAEP_LTT_PVList <- function(fileFormatDF){

  res <- list()
  pvBase <- unique(fileFormatDF$Type[nchar(fileFormatDF$Type)>0])

  for(i in seq_along(pvBase)){
    repVars <- fileFormatDF$variableName[fileFormatDF$Type==pvBase[i]]
    res[[i]] <- list(varnames = tolower(repVars))
  }
  names(res) <- pvBase
  return(res)
}

# @author Paul Bailey & Ahmad Emad
applyPV <- function(pv, Labels, pvWt, oLabels, Type) {
  # the instances where Labels == pv is the cases we are looking at.
  if(sum(Labels==pv)>0) {
    # there are some relevant variables

    # example text for oLabels: "Plausible NAEP math value #3 (num & oper)"
    pvWt[Labels==pv] <- sapply(strsplit(oLabels[Labels==pv],"#", fixed=TRUE), function(x) gsub("[^0-9]", "", x[[2]]))
    # make oLabels lower case
    oLabels[Labels == pv] <- tolower(oLabels[Labels==pv])
    # edit oLabels to Keep only letters
    # 1) remove punctuation
    oLabels[Labels == pv] <- gsub("[[:punct:]]", "", oLabels[Labels==pv])
    # 2) remove numbers
    oLabels[Labels == pv] <- gsub("[0-9]", "", oLabels[Labels==pv])

    # break oLabels into words and remove common words
    temp <- strsplit(oLabels[Labels==pv], " ", fixed=TRUE)
    temp <- lapply(temp, function(x) x[x!=""])
    # this code gets rid of words that are common to all Plausible values,
    # for example, "plausible", "value", and "NAEP"
    words <- temp[[1]]
    summation <- sapply(temp, function(x) words %in% x)
    summation[summation == TRUE] <- 1
    summation[summation == FALSE] <- 0
    summation <- rowSums(summation)
    removeWords <- words[summation == length(temp)]
    # check if there is more than one plausible value (and so all words are "common")
    if(length(removeWords) != length(words)) {
      # Remove common words
      temp <- lapply(temp, function(x) { x[!x %in% removeWords]} )
      # concatenate remaining words with "_"
      temp <- sapply(temp, function(x) paste0(x, collapse="_"))
    # in the case of thetas, theta will be removed by removeWords, which is fine
    # we want to be sure it is at the end anyways
    if(pv == "PVT") {
      temp <- paste0(temp,"_theta")
    }
      Type[Labels == pv] <- temp
    }
    else {
      # if there is only one, get rid of words like plausible, value, naep
      Type[Labels == pv] <- gsub("plausible|naep|value", "", oLabels[Labels == pv])
      Type[Labels == pv] <- trimws(Type[Labels == pv])
      Type[Labels == pv] <- gsub(" ", "_", Type[Labels == pv])
    # all plausible values for a particular subscale/composite now all share a name
    # and that name has no spaces but underscores in it.
    }
  }
  # these are accommodations permitted PVs
  Type[Labels == "PV2"] <- paste0(Type[Labels == "PV2"], "_ap")
  return(list(Labels=Labels, Type=Type, pvWt=pvWt))
}


# for a case-sensitive file system this will change the path to resolve
# correctly when the name is known up to the case.
# Assumes that there is not another file with the same name in the directory
# when ignoring the case. Essentially, this makes a case-sensitive file
# system work as if case-insensitive (when file names would not be overloaded)
# Note that for the NAEP data this is a safe assumption.
# @author Paul Bailey
ignoreCaseFileName <- function(f) {
  bn <- basename(f) # the name of the file in the folder
  dr <- dirname(f) # the name of the directory
  ff <- list.files(dr, pattern=bn, ignore.case=TRUE, full.names=TRUE)
  if(length(ff) == 0) {
    stop(paste0("Could not find file ", dQuote(f),"."))
  }
  if(length(ff) > 1) {
    stop(paste0("File name ", dQuote(f)," is ambigious with respect to capitalization. Please remove one of the following files and try again.", pasteItems(dQuote(ff), final="or"),"."))
  }
  ff
}

#builds the NAEP dataList object
buildNAEP_dataList <- function(stuLaf, stuFF, schLaf, schFF){

  dataList <- list()

  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(lafObject = stuLaf,
                                        fileFormat = stuFF,
                                        levelLabel = "Student",
                                        forceMerge = TRUE,
                                        parentMergeLevels=NULL,
                                        parentMergeVars = NULL,
                                        mergeVars = NULL,
                                        ignoreVars = NULL,
                                        isDimLevel = TRUE)

  #school datafile won't always be present, only add it if applicable
  if(!is.null(schLaf)){
      dataList[["School"]] <- dataListItem(lafObject = schLaf,
                                           fileFormat = schFF,
                                           levelLabel = "School",
                                           forceMerge = FALSE,
                                           parentMergeLevels = c("Student"),
                                           parentMergeVars = c("scrpsu"),
                                           mergeVars = c("sscrpsu"),
                                           ignoreVars = NULL,
                                           isDimLevel = FALSE)
  }

  return(dataList)
}

#1) path: validates and returns the 'path' argument for a valid Data (.dat) fixed-width data file for as well as the 'school' data (.dat) file
#2) frPath: validates and returns information about the .fr2 layout file, and it's full file path (if available)
#3) xmlPath: validates and returns information about the .xml layout file, and it's full file path (if available)
validate_NAEPFilePaths <- function(path, frPath, xmlPath){
  #==== .dat File checking ====
  if(length(path) != 1) {
    stop(paste0("The argument ", dQuote("path"), " must be of length 1."))
  }
  if(!file.exists(path)) {
    stop(paste0("The specified ", dQuote("path"), " is invalid or cannot be found: ", dQuote(path), "."))
  }
  if(!grepl("[.]dat$", path, ignore.case = TRUE)) {
    stop(paste0("The specified ", dQuote("path"), " must have a .dat file extension: ", dQuote(path), "."))
  }

  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  origDir <- dirname(path)
  searchDir <- file.path(dirname(path), "..") #search up one directory level from where file specified

  fileBN <- basename(path)
  fileBN_NoExt <- toupper(sub("[.]dat$", "", fileBN, ignore.case = TRUE))


  #==== .fr2 File checking ====
  fr2Specified <- FALSE
  #check if frPath or xmlPath were passed
  #flags for if the paremter was passed by user explicitly
  if(!is.null(frPath) && is.character(frPath)){
    fr2Specified <- TRUE
    if(length(frPath) != 1) {
      stop(paste0("The argument ", dQuote("frPath"), " must be of length 1."))
    }
    if(!file.exists(frPath)){
      stop(paste0("The specified ", dQuote("frPath"), " is invalid or cannot be found: ", dQuote(frPath), "."))
    }
    if(!grepl("[.]fr2$", frPath, ignore.case = TRUE)) {
      stop(paste0("The specified ", dQuote("frPath"), " must have a .fr2 file extension: ", dQuote(frPath), "."))
    }
    frPath <- suppressWarnings(normalizePath(unique(frPath), winslash = "/"))
    searchDir <- c(searchDir, dirname(frPath)) #expand out searchDir as well
  }

  #==== .xml File checking ====
  xmlSpecified <- FALSE
  #check if frPath or xmlPath were passed
  #flags for if the paremter was passed by user explicitly
  if(!is.null(xmlPath) && is.character(xmlPath)){
    xmlSpecified <- TRUE
    if(length(xmlPath) != 1) {
      stop(paste0("The argument ", dQuote("xmlPath"), " must be of length 1."))
    }
    if(!file.exists(xmlPath)){
      stop(paste0("The specified ", dQuote("xmlPath"), " is invalid or cannot be found: ", dQuote(xmlPath), "."))
    }
    if(!grepl("[.]xml$", xmlPath, ignore.case = TRUE)) {
      stop(paste0("The specified ", dQuote("xmlPath"), " must have a .xml file extension: ", dQuote(xmlPath), "."))
    }
    xmlPath <- suppressWarnings(normalizePath(unique(xmlPath), winslash = "/"))
    searchDir <- c(searchDir, dirname(xmlPath)) #expand out searchDir as well
  }

  #this function will throw specific NAEP filename errors if present
  datFileDesc <- descriptionOfFile(fileBN_NoExt) #function in descriptionOfFile.R file, does more error checking of specific filename there

  #prep default ooutputs
  studentFileOut <- NULL
  schoolFileOut <- NULL

  #if LTT file there is no school level file that we will need to search
  isLTT <- grepl("Long.*Term.*Trend", datFileDesc$Assessment_Code, ignore.case = TRUE)
  searchSchool <- !isLTT #default to check for school file if not LTT file
  searchStudent <- FALSE #assumes

  #check if the school file was specified and search for the student file instead
  if(searchSchool && !grepl("student", datFileDesc$Data_Type, ignore.case = TRUE)){
    warning("Input file was a school level file. Attempting to read in student level file instead. EdSurvey will automatically link the school file.")
    searchStudent <- TRUE
  } else {
    studentFileOut <- path #user passed correct student level file!
  }

  #do one single list.files for both student/school .fr2 and .xml files here in case folders are slow or have lots of contents
  dataFileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "(C|T)", substr(fileBN_NoExt, 6, 99), "[.]dat$")
  dataFileMatches <- list.files(origDir, dataFileRegex, full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
  dataFileBN <- basename(dataFileMatches)

  if(searchSchool){
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "C", substr(fileBN_NoExt, 6, 99), "[.]dat$") #the 'C' signifies the school level file in the 5th character index
    foundFiles <- dataFileMatches[grepl(fileRegex, dataFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      schoolFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    } else {
      if(!isLTT){
        warning(paste0("Could not find school level data file matching filename pattern: ", fileRegex, "\n",
                       "No school level data will be available for analysis." ))
      }
    }
  }

  if(searchStudent){
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "T", substr(fileBN_NoExt, 6, 99), "[.]dat$") #the 'T' signifies the student level file in the 5th character index
    foundFiles <- dataFileMatches[grepl(fileRegex, dataFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      studentFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    } else {
      stop(paste0("Could not find student level data file matching filename pattern: ", fileRegex, "\n",
                     "Be sure to specify the student level data file." ))
    }
  }

  #do one single list.files for both student/school .fr2 and .xml files here in case folders are slow or have lots of contents
  layoutFileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "(C|T)", substr(fileBN_NoExt, 6, 99), "[.](fr2|xml)$")
  layoutFileMatches <- list.files(searchDir, layoutFileRegex, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  layoutFileBN <- basename(layoutFileMatches)

  #==== .fr2 Layout Files ====
  fr2StudentFileOut <- NULL #default
  if(fr2Specified){ #if specified, ensure it's the student level .fr2 file
    fr2BN <- toupper(basename(frPath))
    isStudentFR2 <- substr(fr2BN, 5, 5) == "T"
  }
  if(fr2Specified && isStudentFR2){
    fr2StudentFileOut <- frPath
  } else { #do the search
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "T", substr(fileBN_NoExt, 6, 99), "[.]fr2$") #the 'T' signifies the student level file in the 5th character index
    foundFiles <- layoutFileMatches[grepl(fileRegex, layoutFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      fr2StudentFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    }
  }

  fr2SchoolFileOut <- NULL #default
  if(searchSchool) {
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "C", substr(fileBN_NoExt, 6, 99), "[.]fr2$") #the 'C' signifies the school level file in the 5th character index
    foundFiles <- layoutFileMatches[grepl(fileRegex, layoutFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      fr2SchoolFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    }
  }

  #==== .xml Layout Files ====
  xmlStudentFileOut <- NULL #default
  if(xmlSpecified){ #if specified, ensure it's the student level .fr2 file
    xmlBN <- toupper(basename(xmlPath))
    isStudentXML <- substr(xmlBN, 4, 4) == "T"
  }
  if(xmlSpecified && isStudentXML){
    xmlStudentFileOut <- frPath
  } else { #do the search
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "T", substr(fileBN_NoExt, 6, 99), "[.]xml$") #the 'T' signifies the student level file in the 5th character index
    foundFiles <- layoutFileMatches[grepl(fileRegex, layoutFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      xmlStudentFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    }
  }

  xmlSchoolFileOut <- NULL #default
  if(searchSchool) {
    fileRegex <- paste0("^", substr(fileBN_NoExt, 1, 4), "C", substr(fileBN_NoExt, 6, 99), "[.]xml$") #the 'C' signifies the school level file in the 5th character index
    foundFiles <- layoutFileMatches[grepl(fileRegex, layoutFileBN, ignore.case = TRUE)]

    if(length(foundFiles)>0){
      xmlSchoolFileOut <- foundFiles[1] #default to first if more than one, but should only be one
    }
  }

  #recalc the file description with the true student file
  datFileDesc <- descriptionOfFile(toupper(sub("[.]dat$", "", basename(studentFileOut), ignore.case = TRUE)))
  yr <- datFileDesc$Year
  
  #determine what layout method to use based on what was passed, as well as what is available
  if(fr2Specified && !is.null(fr2StudentFileOut)){
    layoutMethod <- "FR2"
  } else if (xmlSpecified && !is.null(xmlStudentFileOut)){
    layoutMethod <- "XML"
  } else {
    #determine default if not specified based on the file year, starting NAEP 2022 the XML file will be the default method as the .FR2 file is phased out
    if (!is.null(fr2StudentFileOut) && (yr < 2022)){ #FR2 is still preferred method if neither is specified for now, can be swapped later if designed to have XML as default
      layoutMethod <- "FR2"
    } else if (!is.null(xmlStudentFileOut) && (yr >= 2022)){
      layoutMethod <- "XML"
    } else if (!is.null(fr2StudentFileOut)){
      layoutMethod <- "FR2"
    } else if (!is.null(xmlStudentFileOut)){
      layoutMethod <- "XML"
    } else {
      stop(paste0("Unable to locate a matching .fr2 or .xml layout file for: ", dQuote(path), "\n",
                  "Try setting the ", dQuote("frPath"), " or ", dQuote("xmlPath"), " parameter to the known layout file location."))
    }
  }
  #output list
  retList <- list(FileDesc = datFileDesc,
                  StudentDAT = studentFileOut,
                  StudentFR2 = fr2StudentFileOut,
                  StudentXML = xmlStudentFileOut,
                  SchoolDAT = schoolFileOut,
                  SchoolFR2 = fr2SchoolFileOut,
                  SchoolXML = xmlSchoolFileOut,
                  FR2Specified = fr2Specified,
                  XMLSpecified = xmlSpecified,
                  PreferredLayoutMethod = layoutMethod)

  return(retList)
}

appendIRTAttributes_NAEP <- function(esdf, year, subject, region, level, fr2Path, filePathInfo) {
  
  #grab only the digits from the level (e.g. Grade 4 ==> 4)
  levelVal <- as.integer(regmatches(level, regexpr("\\d{1,2}", level, ignore.case = TRUE)))
  levelType <- tolower(regmatches(level, regexpr("\\w+", level, ignore.case = TRUE)))

  # use NAEPirtparams package and filter to needed params
  params <- NAEPirtparams::parameters

  #filter parameters for our file details
  paramsFilter <- params[params$year == year & tolower(params$subject) == tolower(subject) & params$level == levelVal & tolower(params$levelType) == levelType, ]
  
  hasNAEPirtparams <- nrow(paramsFilter) > 0 #eval to T/F
  if (hasNAEPirtparams) {
    paramsFilter$NAEPid <- tolower(paramsFilter$NAEPid)

    if ("accom" %in% paramsFilter$accommodations){
      # Everything no-accom
      paramsFilter <- paramsFilter[!paramsFilter$accommodations=="accom", ]
    } else {
      paramsFilter <- paramsFilter[!paramsFilter$accommodations=="accom", ]
    }

    if (tolower(region) == "state"){
      paramsFilter <- paramsFilter[tolower(paramsFilter$assessmentCode)=="state", ]
    } else {
      # This is National or not indicated
      paramsFilter <- paramsFilter[!tolower(paramsFilter$assessmentCode)=="state", ]
    }

    #no parameters exist for thie file, break early
    if(nrow(paramsFilter)==0){
      return(esdf)
    }

    # get deleted and/or adjusted items
    adjust <- NAEPirtparams::adjustments
    adjustData <- adjust[adjust$level==levelVal & tolower(adjust$subject) == tolower(subject) & adjust$year==year, ]
    if ('accom' %in% adjustData$accommodations){
      # Everything no-accom for now
      adjustData <- adjustData[!adjustData$accommodations=="accom", ]
    } else {
      adjustData <- adjustData[!adjustData$accommodations=="accom", ]
    }
    deletedItems <- tolower(adjustData[adjustData$adjustment=="Deleted", "NAEPid"])
    deletedItems <- deletedItems[deletedItems %in% colnames(esdf)]
    adjustedData <- adjustData[adjustData$adjustment=="Collapsed", c("NAEPid", "from", "to")]
    adjustedData$NAEPid <- tolower(adjustedData$NAEPid)
    if (length(deletedItems) > 0) {
      paramsFilter <- paramsFilter[!paramsFilter$NAEPid %in% deletedItems, ]
    }

    # get paramTabs
    paramTabs <- naepParamTabs(paramsFilter)
    polyParamTab <- paramTabs$polyParamTab
    dichotParamTab <- paramTabs$dichotParamTab

    # building testDat
    # filter transformation constants
    transf <- NAEPirtparams::transformations
    transFilter <- transf[transf$level==levelVal & transf$year==year & tolower(transf$subject) == tolower(subject), ]

    if ("accom" %in% transFilter$accommodations){
      # Everything no-accom for now
      transFilter <- transFilter[!transFilter$accommodations=="accom",]
    } else {
      transFilter <- transFilter[!transFilter$accommodations=="accom",]
    }

    if ("State" == region){
      transFilter <- transFilter[tolower(transFilter$assessmentCode) == "state", ]
    } else {
      # This is National or not indicated
      transFilter <- transFilter[!tolower(transFilter$assessmentCode) == "state",]
    }

    # filter to testDat
    testDat <- transFilter[, c("subtest", "location", "scale", "subtestWeight")]

    #more than one test scale
    if (nrow(testDat) > 1 && anyNA(testDat$subtestWeight)) {
      compositeVars <- grepl("(composite|univariate)", testDat$subtest, ignore.case = TRUE)

      if(any(compositeVars)){
        testDat <- testDat[!(compositeVars & is.na(testDat$subtestWeight)), ] #drop any composite vars with no subtest weight (ensure ! is evaluated after the & with the quotes!)
      }
    }

    #test that if there are subtest weights, that they equal up to 100%
    if(!all(is.na(testDat$subtestWeight))){
      testSum <- sum(testDat$subtestWeight[!is.na(testDat$subtestWeight)])
      if(abs(testSum - 1) > 0.02){
        warning(paste0("The IRT Test subscales don't equal 100% when it is expected."))
      }
    }

    # default scoreCard
    scoreCard <- defaultNAEPScoreCard()

    # check that there are no duplicate items in your paramTabs
    hasPolyDupes <- anyDuplicated(unique(polyParamTab[ , c("ItemID", "subtest")]))
    hasDichotDupes <- anyDuplicated(unique(polyParamTab[ , c("ItemID", "subtest")]))
    hasTestDupes <- anyDuplicated(unique(polyParamTab[ , c("subtest")]))

    if (hasPolyDupes > 0 || hasDichotDupes > 0 || hasTestDupes > 0) {
      #THIS SHOULD NEVER HIT SINCE WE ARE ONLY USING NAEPirtparameters package
      warning("You have duplicate data in your NAEP IRT parameters table(s). Use a DCT file with the function setNAEPScoreCard to use mml.sdf.")
      return(esdf) #return unmodified edsurvey.data.frame
    } else {
      # NAEP score card
      scoreDict <- getNAEPScoreCard(fr2Path, polyParamTab$ItemID, dichotParamTab$ItemID, adjustedData, scoreCard)
      # update dichotparamtab: 1/k for missing value
      dichotScores <- scoreDict[scoreDict$key %in% dichotParamTab$ItemID & !scoreDict$answer %in% defaultNAEPScoreCard()$resCat, ]
      if (nrow(dichotScores) > 0) {
        Ks <- aggregate(answer~key, dichotScores, length)
        Ks$answer <- 1 / Ks$answer
        dichotParamTab <- merge(dichotParamTab, Ks, by.x = 'ItemID', by.y = 'key', all.x = TRUE)
        dichotParamTab$missingValue <- ifelse(is.na(dichotParamTab$answer), dichotParamTab$missingValue, dichotParamTab$answer)
        dichotParamTab$answer <- NULL
      }
    } #end if (hasPolyDupes > 0 || hasDichotDupes > 0 || hasTestDupes > 0)

    #check if composite/overall and add that in as the "test" for the param tabs
    pvvars <- names(esdf$pvvars)
    pvvars <- pvvars[!grepl("(_theta|_linking|_ap)$", pvvars, ignore.case = TRUE)] #remove thetas, linking, and accomodation pvvars from checks!
    compositePV <- pvvars[grepl("(composite|composite_ap|univariate|overall|scale_compos)", pvvars, ignore.case = TRUE)] #should only have 1 composite record!

    if (length(compositePV) > 0 && nrow(testDat) > 0) {
      if(length(compositePV) > 1){
        warning(paste0("Multiple Composite Plausible Values found! Only first one will be used."))
        compositePV <- compositePV[1]
      }
      if (!any(compositePV %in% testDat$subtest)) {
        if(nrow(dichotParamTab)>0){
          dichotParamTab$test <- compositePV
        } else{
          dichotParamTab$test <- character(0)
        }
        if(nrow(polyParamTab)>0){
          polyParamTab$test <- compositePV
        } else{
          polyParamTab$test <- character(0)
        }
        testDat$test <- compositePV
      }
    }

    #set the scorePoints for the polyParamTab here
    polyScorePoints <- unique(scoreDict[scoreDict$key %in% polyParamTab$ItemID, c("key", "scorePoints")])
    polyParamTab <- merge(polyParamTab, polyScorePoints, by.x = "ItemID", by.y = "key", all.x = TRUE)

    #get unique list of the subtest/subscales for validation
    scaleNames <- unique(c(testDat$subtest, testDat$test))

    #validate that all of test names match the pvvar names
    if (!all(pvvars %in% scaleNames)) {

      badPV <- pvvars[!(pvvars %in% scaleNames)]
      badScales <- scaleNames[!(scaleNames %in% pvvars)]

      if(length(badPV) > 0){

        fixDF <- data.frame(pvName = c("alg_fnctns", "alg_functns",
                                       "da_stat_prob",  "data_an_stat", "data_anal_prob", "data_analstat", "data_anal_stat",
                                       "envsociety",
                                       "measurementgeom",
                                       "num_oper",
                                       "spaceplace",
                                       "spatial_dyn",
                                       "world_role"),
                            scaleName = c("algebra", "algebra",
                                          "data", "data", "data", "data", "data",
                                          "environment",
                                          "measurement",
                                          "number",
                                          "space",
                                          "spatial",
                                          "world")
        )
        fixDF <- fixDF[fixDF$pvName %in% badPV & fixDF$scaleName %in% badScales, ]

        for(i in seq_len(nrow(fixDF))){
          testDat$subtest[testDat$subtest == fixDF[i, "scaleName"]] <- fixDF[i, "pvName"]
          dichotParamTab$subtest[dichotParamTab$subtest == fixDF[i, "scaleName"]] <- fixDF[i, "pvName"]
          polyParamTab$subtest[polyParamTab$subtest == fixDF[i, "scaleName"]] <- fixDF[i, "pvName"]
        }


        #recheck again if any issues still present
        scaleNames <- unique(c(testDat$subtest, testDat$test))
        badPV <- pvvars[!(pvvars %in% scaleNames)]
        badScales <- scaleNames[!(scaleNames %in% pvvars)]

        if (length(badPV) > 0 && length(badScales) > 0) {
          warning(paste0("Mismatched Plausible Value names (", paste0(sQuote(badPV), collapse = ", "), ") and IRT Scale names (", paste0(sQuote(badScales), collapse = ", "), ")!"))
        }

      }# end if(length(badPV) > 0)
    } #end if (!all(pvvars %in% scaleNames))


    # set mml.sdf related objects as attributes
    esdf <- setAttributes(esdf, "scoreCard", scoreCard)
    esdf <- setAttributes(esdf, "dichotParamTab", dichotParamTab)
    esdf <- setAttributes(esdf, "polyParamTab", polyParamTab)
    esdf <- setAttributes(esdf, "adjustedData", adjustedData)
    esdf <- setAttributes(esdf, "testData", testDat)
    esdf <- setAttributes(esdf, "scoreDict", scoreDict)
    esdf <- setAttributes(esdf, "scoreFunction", scoreDefault)

  }#end if (year %in% unique(params$year) & subject %in% unique(params$subject) & level %in% unique(params$level))

  #search for an associated .dct file and apply it if found
  if(!hasNAEPirtparams){
    searchDir <- file.path(dirname(filePathInfo$StudentDAT), "../AM") #search AM folder specifically as STATA files have .dct format that's completely different
    searchFN <- gsub("[.]dat$", "", basename(filePathInfo$StudentDAT), ignore.case = TRUE)
    
    dctFP <- list.files(searchDir, paste0("^", searchFN, "[.]dct$"), full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
    
    if(length(dctFP) > 0){
      dctFP <- dctFP[1] #use the first one if multiple found(they should be the same)
      tryCatch(esdf <- suppressWarnings(setNAEPScoreCard(esdf, dctFP)), error=function(e) {} ) #use this function to set the IRT attributes, check why getNAEPScorecard throwing NA coercion warnings?
    }
  }

  return(esdf) #return original if data not found in NAEPirtparams, or the modified esdf if data located
}

#' default scorer
#' scores column on edf identified by polyParamTab$ItemID, dichotParamTab$ItemID using a crosswalk in scoreDict





#' @title Assessment scoring
#'
#' @description Score assessments
#'
#' @param edf the data
#' @param polyParamTab see \code{\link{mml.sdf}}
#' @param dichotParamTab see \code{\link{mml.sdf}}
#' @param scoreDict a data frame; see Details.
#' @details
#' the \code{scoreDict} is a data frame in long format with columns \code{key}, \code{answer}, and \code{score}.
#' the function maps, within the item identified by \code{key} from \code{answer} to \code{score}.
#'
#' @return  a data frame with the columns in the \code{scoreDict} \code{key} column mapped from \code{answer} to \code{score}.
#'
#' @seealso \code{\link{mml.sdf}}
#' @author Paul Bailey and  Tom Fink
#'
#' @export
scoreDefault <- function(edf, polyParamTab, dichotParamTab, scoreDict) {
  # for NAEP
  edf$score <- NULL
  for (item in c(polyParamTab$ItemID, dichotParamTab$ItemID)) {
    itemDict <- scoreDict[scoreDict$key %in% item, c("answer", "score")]
    if(nrow(itemDict) < 1) {
      stop("could not score item ", item, " check that it is on the scoreDict")
    }
    edf <- merge(edf, itemDict, by.x=item, by.y="answer", all.x=TRUE, all.y=FALSE)
    # update to score
    edf[ , item] <- edf$score
    edf$score <- NULL
  }
  return(edf)
}

#determines the .FR2 file layout and returns a list of vectors with the .fr2 file details (e.g., Variable Names, Start Position, Width, End Position, Decimal, Labels, Num Values)
getNAEP_FR2Specs <- function(txt){
  
  #original fr2 file column specifications
  variableName <- trimws(substring(txt, 1, 8)) # name of the variable
  Start <- trimws(substring(txt, 9, 12)) # start column in file
  Width <- trimws(substring(txt, 13, 14)) # number of characters in variable
  Decimal <- trimws(substring(txt, 15, 15)) # digits (from the right) to be considered decimals
  Labels <- trimws(substring(txt, 21, 70)) # variable label
  scoreKey <- trimws(substring(txt, 71, 80)) # score key (e.g. 00010, 0100, 1000)
  NumValue <- trimws(substring(txt, 89, 90)) # number of numeric codes (e.g. one could would be 1="Yes")
  
  startOK <- all(grepl("\\d+", Start))
  widthOK <- all(grepl("\\d+", Width[nchar(Width)>0]))
  decimalOK <- all(grepl("(\\d+|[*])", Decimal[nchar(Decimal)>0])) #sometimes a '*' character is used, be sure to allow for that
  numValueOK <- all(grepl("\\d+", NumValue))
  
  if(startOK && widthOK && decimalOK && numValueOK){
    
    #get the value labels from the foils
    labelValues <- rep("", times = length(variableName))
    valLblIndex <- which(NumValue > 0, arr.ind = TRUE) #only process items with defined value labels
    NumValue = as.numeric(NumValue)
    Decimal[!grepl("\\d+", Decimal)] <- NA #convert non-numeric values to NA
    
    for (i in valLblIndex) {
        # if it has numeric codes
        # read in up to 26 character per code, plus 2 characters for the number
        Ncodes <- NumValue[i]-1 #be cautious here, for the seq call to work properly, a 0 value indicates 1 value label item
        codeValSeq <- seq(91, (91+Ncodes*28), by = 28)
        codeLabelSeq <- seq(93, (93+Ncodes*28), by = 28)
        values <- as.numeric(trimws(substring(txt[i], codeValSeq, codeValSeq+1)))
        labels <- trimws(substring(txt[i], codeLabelSeq, codeLabelSeq+19))
        labelValues[i] <- paste(values, labels, collapse ="^", sep="=")
    }
    
    return(list(variableName = variableName,
                Start = as.numeric(Start),
                Width = as.numeric(Width),
                End = (as.numeric(Start) + as.numeric(Width) - 1),
                Decimal = as.numeric(Decimal),
                Labels = Labels,
                NumValue = NumValue, #converted to numeric earlier
                labelValues = labelValues,
                scoreKey = scoreKey,
                fr2Version = 1)
           )
  }
  
  #the 'updated .fr2 file column specifications as of NAEP 2022 files
  variableName <- trimws(substring(txt, 1, 32)) # name of the variable
  Start <- trimws(substring(txt, 33, 36)) # start column in file
  Width <- trimws(substring(txt, 37, 38)) # number of characters in variable
  Decimal <- trimws(substring(txt, 39, 39)) # digits (from the right) to be considered decimals
  Labels <- trimws(substring(txt, 45, 94)) # variable label
  scoreKey <- trimws(substring(txt, 95, 104)) # score key (e.g. 00010, 0100, 1000)
  NumValue <- trimws(substring(txt, 113, 114)) # number of numeric codes (e.g. one could would be 1="Yes")
  
  startOK <- all(grepl("\\d+", Start))
  widthOK <- all(grepl("\\d+", Width[nchar(Width)>0]))
  decimalOK <- all(grepl("(\\d+|[*])", Decimal[nchar(Decimal)>0])) #sometimes a '*' character is used, be sure to allow for that
  numValueOK <- all(grepl("\\d+", NumValue))
  
  if(startOK && widthOK && decimalOK && numValueOK){
    
    #get the value labels from the foils
    labelValues <- rep("", times = length(variableName))
    valLblIndex <- which(NumValue > 0, arr.ind = TRUE) #only process items with defined value labels
    NumValue = as.numeric(NumValue)
    Decimal[!grepl("\\d+", Decimal)] <- NA #convert non-numeric values to NA
    
    for (i in valLblIndex) {
      # if it has numeric codes
      # read in up to 26 character per code, plus 2 characters for the number
      Ncodes <- NumValue[i]-1 #be cautious here, for the seq call to work properly, a 0 value indicates 1 value label item
      codeValSeq <- seq(115, (115+Ncodes*28), by = 28)
      codeLabelSeq <- seq(117, (117+Ncodes*28), by = 28)
      values <- as.numeric(trimws(substring(txt[i], codeValSeq, codeValSeq+1)))
      labels <- trimws(substring(txt[i], codeLabelSeq, codeLabelSeq+19))
      labelValues[i] <- paste(values, labels, collapse ="^", sep="=")
    }
    
    return(list(variableName = variableName,
                Start = as.numeric(Start),
                Width = as.numeric(Width),
                End = (as.numeric(Start) + as.numeric(Width) - 1),
                Decimal = as.numeric(Decimal),
                Labels = Labels,
                NumValue = NumValue, #converted to numeric earlier
                labelValues = labelValues,
                scoreKey = scoreKey,
                fr2Version = 2)
    )
  }
  
  #throw error if .FR2 is not expected format
  stop(paste0("The NAEP .FR2 layout is corrupt and unable to be parsed. Consider trying the NAEP .XML layout version if available."))
}

