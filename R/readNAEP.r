#' @title Connect to NAEP Data
#'
#' @description Opens a connection to an NAEP data file residing
#'              on the disk and returns an \code{edsurvey.data.frame} with 
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
#' @param frPath a character value indicating the location of the \code{fr2}
#'               parameter layout file included with the data companion to
#'               parse the specified \code{filepath} data file
#' @details
#' The function uses the \code{frPath} file layout (.fr2) data to read in the
#' fixed-width data file (.dat) and builds the \code{edsurvey.data.frame}.
#'
#' NAEP includes both scaled scores and theta scores, with the latter having names ending in \code{\_theta}.
#'
#' When a NAEP administration includes a linking error variable those variables are included and end in \code{_linking}.
#' When present, simply use the \code{_linking} version of a variable to get a standard error estimate that includes linking error.
#' 
#' @return An \code{edsurvey.data.frame} containing the following elements:
#'    \item{userConditions}{a list containing all user conditions set using the \code{subset.edsurvey.data.frame} method}
#'    \item{defaultConditions}{the default conditions to be applied to the \code{edsurvey.data.frame}}
#'    \item{data}{an \code{LaF} object containing a connection to the student dataset on disk}
#'    \item{dataSch}{an \code{LaF} object containing a connection to the school dataset on disk}
#'    \item{dataTch}{not applicable for NAEP data; returns \code{NULL}}
#'    \item{weights}{a list containing the weights found on the \code{edsurvey.data.frame}}
#'    \item{pvvar}{a list containing the plausible values found on the \code{edsurvey.data.frame}}
#'    \item{subject}{the subject of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{year}{the year of assessment of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{assessmentCode}{the code of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{dataType}{the type of data (whether student or school) contained in the \code{edsurvey.data.frame}}
#'    \item{gradeLevel}{the grade of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{achievementLevels}{default NAEP achievement cutoff scores}
#'    \item{omittedLevels}{the levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}}
#'    \item{fileFormat}{a \code{data.frame} containing the parsed information from the student .fr2 file associated with the data}
#'    \item{fileFormatSchool}{a \code{data.frame} containing the parsed information from the school .fr2 file associated with the data}
#'    \item{fileFormatTeacher}{not applicable for NAEP data; returns \code{NULL}}
#'    \item{survey}{the type of survey data contained in the \code{edsurvey.data.frame}}
#' @author Tom Fink and Ahmad Emad
#' @example \man\examples\readNAEP.R
#' @export
readNAEP <- function(path, defaultWeight = "origwt", defaultPvs = "composite", omittedLevels = c('Multiple',NA,'Omitted'), frPath = NULL) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/")) #give better error later if file not found

  hasFRpath <- FALSE
  if(missing(frPath) || !is.character(frPath)){
    # the frPath is not a character, so warn the user we are not using it
    if(!missing(frPath)) {
      warning(paste0("Ignoring non-character ", dQuote("frPath"), " argument. Using default instead."))
    }
    frPath <- NULL
  }
  if(!is.null(frPath)){
    frPath <- suppressWarnings(normalizePath(unique(frPath), winslash = "/")) #give better error later if file not found
    hasFRpath <- TRUE
  }
  
  if(length(path) != 1) {
    stop(paste0("The argument ", dQuote("path"), " must be of length 1."))
  }
  if(!file.exists(path)){
    stop(paste0("The specified ", dQuote("path"), ", cannot be found: ", dQuote(path), "."))
  }
  
  if(hasFRpath && length(frPath) != 1) {
    stop(paste0("The argument ", dQuote("frPath"), " must be of length 1."))
  }
  if(hasFRpath && !file.exists(frPath)){
    stop(paste0("The specified ", dQuote("frPath"), " file cannot be found: ", dQuote(frPath), ". Be sure to specify a full .fr2 file path and not a directory. The path may be case-sensitive."))
  }
  
  filedir <- dirname(path) # the directory for the file
  filename <- gsub("\\.dat$","", basename(path), ignore.case = TRUE) # the file name (less a trailing .dat, if included)
  
  if(hasFRpath){
    testFileName <- gsub("\\.fr2$","", basename(frPath), ignore.case = TRUE)
    
    if(testFileName != filename){
      stop(paste0("The base file names (ignoring the file extension) must match between the ", dQuote("path"), " (", dQuote(filename),")", " and ", dQuote("frPath"), " (", dQuote(testFileName), ") arguments."))
    }
    frPath <- dirname(frPath) #strip off the file name after we know it's a match
  }
  
  #all files are to be expected to be a string of length 8 with the 4th from last character being either C (school) or T (student) (not including the file extension)
  #ex: M36NC2PM.dat is a school level file and M36NT2PM.dat is a student level file
  isSchoolFile <- tolower(substr(filename, nchar(filename) - 3, nchar(filename) - 3)) %in% "c" #logical flag | example: M36NC2PM (from NAEPprimer)
  
  schoolFileRegex <- paste0("^", substr(filename,1,4), "(C|c)", substr(filename, 6, nchar(filename)), "\\.dat$")
  schoolFR2Regex <- paste0("^", substr(filename,1,4), "(C|c)", substr(filename, 6, nchar(filename)), "\\.fr2$")
  schoolFile <- list.files(filedir, schoolFileRegex, ignore.case = TRUE, full.names = TRUE)
  
  studentFileRegex <- paste0("^", substr(filename,1,4), "(T|t)", substr(filename, 6, nchar(filename)), "\\.dat$")
  studentFR2Regex <- paste0("^", substr(filename,1,4), "(T|t)", substr(filename, 6, nchar(filename)), "\\.fr2$")
  studentFile <- list.files(filedir, studentFileRegex, ignore.case = TRUE, full.names = TRUE)
  
  #check if a school level file was passed as 'path' argument.  if so, find the associates school level file
  if(isSchoolFile) {
    warning("Input file was a school level file. Attempting to read in student level file instead. EdSurvey will automatically link the school file.")
  }
  
  if(length(studentFile) < 1){
    stop(paste0("Could not find student level data file matching filename: ", studentFileRegex, "\n",
                "In specified directory: ", filedir, "." ))
  }
  if(length(studentFile) > 1){
    stop(paste0("Multiple student level data files matching filename: ", studentFileRegex, "\n",
                "In specified directory: ", filedir, "\n",
                "Please ensure only one file exists in the directory with this name and retry (regardless of filename case)." ))
  }
  
  #if it's a long term trend file (4th character in base filename is "L") then don't check for a school file
  checkSchoolFile <- !grepl("^...L", basename(studentFile), ignore.case = TRUE)
  hasSchoolFile <- checkSchoolFile #default to TRUE if not a long-term trend file
  
  if(length(schoolFile) < 1 && checkSchoolFile){
    warning(paste0("Could not find school level data file matching filename: ", schoolFileRegex, "\n",
                "No school level data will be available for analysis." ))
    hasSchoolFile <- FALSE
  }
  if(length(schoolFile) > 1 && checkSchoolFile){
    stop(paste0("Multiple school level data files matching filename: ", schoolFileRegex, "\n",
                "In specified directory: ", filedir, "\n",
                "Please ensure only one file exists in the directory with this name and retry (regardless of filename case)." ))
  }
  
  #if no frPath specified, then we will search the ../select/parms folder for matching ones
  if(!hasFRpath){
    fr2SearchDir <- file.path(filedir, "..", "select", "parms") #up on level 
  }else{
    fr2SearchDir <- frPath
  }
  
  studentFR2 <- list.files(fr2SearchDir, studentFR2Regex, full.names = TRUE, ignore.case = TRUE)
  if(length(studentFR2) == 0) {
    stop(paste0("Unable to find matching .fr2 control file: ", dQuote(studentFR2Regex), "\n",
                "In specified directory: ", dQuote(fr2SearchDir), "." ))
  }
  if(length(studentFR2) > 1) {
    studentFR2 <- studentFR2[1] #limit to just one if multiple found, assuming it will be correct file
  }
    
  #if school file, lets search for it
  if(hasSchoolFile){
    schoolFR2 <- list.files(fr2SearchDir, schoolFR2Regex, full.names = TRUE, ignore.case = TRUE)                
    
    if(length(schoolFR2) == 0) {
      stop(paste0("Unable to find matching .fr2 control file: ", dQuote(schoolFR2Regex), "\n",
                  "In specified directory: ", dQuote(fr2SearchDir), "." ))
    }
    if(length(schoolFR2) > 1) {
      schoolFR2 <- schoolFR2[1] #limit to just one if multiple found, assuming it will be correct file
    }
  }
  
  #Getting description of data
  f <- list()
  if(filename != "sdfexample") {
    f <- descriptionOfFile(filename)
  }
  else {
    f[['filename']] <- filename
  }
  
  #check if this is a long-term trend file
  isLTT <- grepl("^long.*term.*trend$", f$Assessment_Code, ignore.case = TRUE)
  dataSchLaf <- NULL #default
  schLabelsFile <- NULL
  
  if(isLTT){
    #no school level file, only a student level file
    labelsFile <- readMRC_LTT(studentFR2, f[["Subject"]])

    #build the weights and pvvars list
    weights <- buildNAEP_LTT_WeightList(labelsFile)
    pvs <- buildNAEP_LTT_PVList(labelsFile)
    
    dataLaf <- laf_open_fwf(filename=studentFile, column_types=labelsFile$dataType,
                            column_names=labelsFile$variableName, column_widths=as.numeric(labelsFile$Width))
    
    #in some rare cases the LaF cleaned names differ from the fileFormat, ensure they are consistent
    labelsFile$variableName <- names(dataLaf)

  }else{ #parse as normal NAEP data file
    #parse the fr2 control files and create the LaF objects for the edsurvey.data.frame
    labelsFile <- readMRC(studentFR2)
    if(hasSchoolFile) {
      schLabelsFile <- readMRC(schoolFR2)
      schLabelsFile <- schLabelsFile[order(schLabelsFile$Start),]
      widths <- schLabelsFile$Width
      dataTypes <- as.character(schLabelsFile$dataType)
      varNames = as.character(tolower(schLabelsFile$variableName))
      dataSchLaf <- laf_open_fwf(filename=schoolFile, column_types=dataTypes,
                                 column_names=varNames, column_widths=widths)
    }
    labelsFile <- labelsFile[order(labelsFile$Start),]
    widths <- labelsFile$Width
    dataTypes <- as.character(labelsFile$dataType)
    varNames = as.character(tolower(labelsFile$variableName))
    
    dataLaf <- laf_open_fwf(filename=studentFile, column_types=dataTypes,
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
    
    weight_temp <-  tolower(varNames[labelsFile$Labels == "JK"])
    jksuffix <- gsub("[^0-9]","", weight_temp)
    base <- gsub(jksuffix[1],"", weight_temp[1])
    
    # setup weights
    if(sum("JK2" %in% labelsFile$Labels) == 0) {
      # one set of weights
      weights <- list(origwt=list(jkbase=base, jksuffixes=jksuffix))
      names(weights) <- (varNames[labelsFile$weights])[1]
    } else{ 
      # there is two sets of weights
      weight_tempAP <-  tolower(varNames[labelsFile$Labels == "JK2"])
      jksuffixAP <- gsub("[^0-9]","", weight_tempAP)
      baseAP <- gsub(jksuffixAP[1],"", weight_tempAP[1])
      weights <- list(origwt=list(jkbase=base, jksuffixes=jksuffix), aorigwt=list(jkbase=baseAP, jksuffixes=jksuffixAP))
      names(weights) <- (varNames[labelsFile$weights])[1:2] # first two weights
    }
  }#end if(isLTT)
  
  # set default weight
  if(missing(defaultWeight) || !any(defaultWeight %in% names(weights))) {
    attributes(weights)$default <- names(weights)[1]
  } else {
    attributes(weights)$default <- defaultWeight
  }
  
  if(is.null(defaultPvs) || all(is.na(defaultPvs))){
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

  if(length(pvs)>0){
    attributes(pvs)$default <- defaultPvs[1]
  }
  
  #achievementLevelsHelp located in descriptionOfFile.R
  #each file will only have one achievement level scale associated with the file
  levels <- achievementLevelsHelp(f$Grade_Level, f$Year, f$Subject, f$Assessment_Code)
  
  #apply the achievement levels only to the non-theta pvs
  #levels will only have one row here for NAEP data files
  for(i in seq_along(pvs)){
    if(!grepl("_theta$", names(pvs)[i], ignore.case = TRUE)){
      pvs[[i]]$achievementLevel <- levels #there will be only one value in levels
    }
  }
  
  #convert achievementLevels to list, this bypasses the automatic apply to all pvs
  levelName <- f$Subject
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
  if(is.null(ps) || is.na(ps)){
    psuVar <- NULL
    stratumVar<- NULL
  }else{
    psuVar <- ps["psu"]
    stratumVar <- ps["stratum"]
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
                      subject = f[["Subject"]],
                      year = f[["Year"]],
                      assessmentCode = f[["Assessment_Code"]],
                      dataType = f[["Data_Type"]],
                      gradeLevel = f[["Grade_Level"]],
                      achievementLevels = levels,
                      omittedLevels = omittedLevels,
                      survey = "NAEP",
                      country = "USA",
                      psuVar = psuVar,
                      stratumVar = stratumVar,
                      jkSumMultiplier = 1,
                      fr2Path=studentFR2)
  
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
  variableName <- trimws(substring(mrcFile, 1, 8)) # name of the variable
  Start <- as.numeric(trimws(substring(mrcFile, 9, 12))) # start column in file
  Width <- as.numeric(trimws(substring(mrcFile, 13, 14))) # number of characters in variable
  End <- Start + Width -1 # end column in file
  Decimal <- as.numeric(trimws(substring(mrcFile, 15, 15))) # digits (from the right) to be considered decimals
  Labels <- trimws(substring(mrcFile, 21, 70)) # variable label
  NumValue <- as.numeric(trimws(substring(mrcFile, 89, 90))) # number of numeric codes (e.g. one could would be 1="Yes")
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
  
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
  # parse the numeric codes
  labelValues <- character(length(mrcFile))
  for (j in 1:length(mrcFile)) {
    # for each line:
    Ncodes <- NumValue[j]-1
    if (Ncodes > 0) {
      # if it has numeric codes
      # read in up to 26 character per code, plus 2 characters for the number
      codeValSeq <- seq(91, (91+Ncodes*28), by = 28)
      codeLabelSeq <- seq(93, (93+Ncodes*28), by = 28)
      values <- as.numeric(trimws(substring(mrcFile[j], codeValSeq, codeValSeq+1)))
      labels <- trimws(substring(mrcFile[j], codeLabelSeq, codeLabelSeq+19))
      labelValues[j] <- paste(values, labels, collapse ="^", sep="=")
    }
  }
  
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
  variableName <- trimws(substring(mrcFile, 1, 8)) # name of the variable
  Start <- as.numeric(trimws(substring(mrcFile, 9, 12))) # start column in file
  Width <- as.numeric(trimws(substring(mrcFile, 13, 14))) # number of characters in variable
  End <- Start + Width -1 # end column in file
  Decimal <- as.numeric(trimws(substring(mrcFile, 15, 15))) # digits (from the right) to be considered decimals
  Labels <- trimws(substring(mrcFile, 21, 70)) # variable label
  NumValue <- as.numeric(trimws(substring(mrcFile, 89, 90))) # number of numeric codes (e.g. one could would be 1="Yes")
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
  
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
  # parse the numeric codes
  labelValues <- character(length(mrcFile))
  for (j in 1:length(mrcFile)) {
    # for each line:
    Ncodes <- NumValue[j]-1
    if (Ncodes > 0) {
      # if it has numeric codes
      # read in up to 26 character per code, plus 2 characters for the number
      codeValSeq <- seq(91, (91+Ncodes*28), by = 28)
      codeLabelSeq <- seq(93, (93+Ncodes*28), by = 28)
      values <- as.numeric(trimws(substring(mrcFile[j], codeValSeq, codeValSeq+1)))
      labels <- trimws(substring(mrcFile[j], codeLabelSeq, codeLabelSeq+19))
      labelValues[j] <- paste(values, labels, collapse ="^", sep="=")
    }
  }
  
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

getMu <- function(x, w) {
  sum(x*w) / sum(w)
}

getS <- function(x, w, mu=NULL) {
  if(is.null(mu)) {
    mu <- getMu(x, w)
  }
  sqrt(sum( w* ((x-mu)^2)) / sum(w))
}