#' @title Connect to HSTS Data
#'
#' @description Opens a connection to a High School Transcript Study (HSTS) data files for years 2019.
#'              Returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param dataFilePath a character value to the root directory path of extracted set of ASCII data files (.txt or .dat file extension).
#'                    \code{readHSTS} will search within sub-directories of this parameter for expected data files based on the specified \code{year} parameter.
#'
#' @param spssPrgPath a character value to the directory path of where the extracted set of .sps program files are located.
#'                    The data file and associated SPSS program filenames *must match* (having different file extensions) to determine which files are associated together.
#'                    \code{readHSTS} will search within sub-directories of this parameter for expected SPSS programe files based on the specified \code{year} parameter.
#'
#' @param year a character value to indicate the year of the dataset.  Only one year is supported for a single \code{readHSTS} data call.
#'               The year is required to help determine specific study information.  Only 2019 study is currently supported.
#'
#' @param verbose a logical value that will determine if you want verbose output while the \code{readHSTS} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#'                
#' @details The HSTS data has a complex structure and unique characteristics all handled internally within \code{EdSurvey}. 
#'           The structure allows for automatic dynamic linking across all various data 'levels' based the requested variables. The \code{student} data level is the primary analysis unit. 
#'           Dynamic linking for variables that include both \code{tests} and \code{transcript} level details will result in an error, as they cannot be simultaneously returned in a single call. 
#'           Situations may arise where the analyst must derive variables for analysis. See the documentation for \code{merge} and \code{$<-} functions for more detail. All merge operations are done at the \code{student} level (the main analysis unit).
#'            
#' File Layout for HSTS 2019:
#'  \itemize{
#'        \item School (school.dat) - School level variables.
#'        \itemize{
#'          \item School Catalog (catalog.dat) - Catalog variables joined to School data. Variables renamed to begin with \code{SchCat_} to distinguish from Transcript Catalog.  Cannot be merged with any \code{Student} data.
#'        }
#'        \item Student (student.dat) - Student level variables.  Primary analysis unit, all merged/cached data must be at this level.
#'        \itemize{
#'          \item NAEP Math (naepmath.dat) - Subset of students containing NAEP Math variables.  Variables begin with \code{math_} to ensure they are unique from the NAEP Science variables.
#'          \item NAEP Science (naepsci.dat) - Subset of students containing NAEP Science variables.  Variables begin with \code{sci_} to ensure they are unique from the NAEP Math variables.
#'          \item Tests (tests.dat) - Students may have many test records.  Contains ACT/SAT testing score details for students.  Cannot be merged together with any Transcript or Transcript Catalog data.
#'          \item Transcripts (trnscrpt.dat) - Students may have many transcript records.  Contains transcript level details. Cannot be merged together with Test data.
#'            \itemize{
#'              \item Transcript Catalog (catalog.dat) - Each transcript record is associated to a catalog record for giving context to the transcript record.  2019 uses \href{https://nces.ed.gov/scedfinder}{SCED codes} for categorizing courses.
#'            }
#'        }
#'  }
#' @return
#'  an \code{edsurvey.data.frame} for the HSTS dataset.
#'
#' @seealso \code{\link{showCodebook}}, \code{\link{searchSDF}}, \code{\link{edsurvey.data.frame}}, \code{\link{merge.edsurvey.data.frame}}, and \code{\link{getData}}
#' @author Tom Fink
#' @export
readHSTS <- function(dataFilePath = getwd(),
                     spssPrgPath = dataFilePath,
                     year = c("2019"),
                     verbose = TRUE) {
  # temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)

  dataFilePath <- suppressWarnings(normalizePath(unique(dataFilePath), winslash = "/"))
  spssPrgPath <- suppressWarnings(normalizePath(unique(spssPrgPath), winslash = "/"))

  # gather datafiles and .sps syntax files
  txtDataFiles <- list.files(path = dataFilePath, pattern = "[.](txt|dat)$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  spssFormatFiles <- list.files(path = spssPrgPath, pattern = "[.]sps$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  linkingErrorFiles <- list.files(path = unique(c(dataFilePath, spssPrgPath)), pattern = "^Linking_Factors_HSTS[.]xlsx$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  
  #validate that we have data and spss program files
  if(length(txtDataFiles)==0 || is.null(txtDataFiles)){
    stop(paste0("Cannot find any data files in path ", sQuote(dataFilePath), "."))
  }
  if (length(spssFormatFiles) == 0 || is.null(spssFormatFiles)) {
    stop(paste0("Cannot find any SPSS (.sps) files in path ", sQuote(spssPrgPath), "."))
  }

  year <- match.arg(as.character(year), c("2019"), several.ok = FALSE)

  # get the filenames without the extension
  fnTxt <- tolower(gsub("[.](txt|dat)$", "", basename(txtDataFiles), ignore.case = TRUE))
  fnSPSS <- tolower(gsub("[.]sps$", "", basename(spssFormatFiles), ignore.case = TRUE))

  chkFiles <- getHSTS_DataFileNames(year)

  if (year == 2019) {
    fileFound <- chkFiles %in% fnTxt
    if (!all(fileFound)) {
      stop(paste0("Expected 2019 HSTS .dat Data File(s) not found: ", pasteItems(chkFiles[!fileFound])))
    }
    layoutFound <- chkFiles %in% fnSPSS
    if (!all(layoutFound)) {
      stop(paste0("Expected 2019 HSTS .sps SPSS Script File(s) not found: ", pasteItems(chkFiles[!layoutFound])))
    }
    spsFileEnc <- "UTF8"

    # special case for 2019 where we want to create essentially TWO catalogs (one for School and one For Transcripts)
    chkFiles <- c(chkFiles, "school_catalog")

    txtDataFiles <- c(txtDataFiles, txtDataFiles[which(grepl("catalog", fnTxt, ignore.case = TRUE))])
    fnTxt <- c(fnTxt, "school_catalog")
    spssFormatFiles <- c(spssFormatFiles, spssFormatFiles[which(grepl("catalog", fnSPSS, ignore.case = TRUE))])
    fnSPSS <- c(fnSPSS, "school_catalog")
  }

  # placeholders to store LaF objects and the parsed File Format objects from the .sps file
  spssFileFormat <- vector(mode = "list", length = length(txtDataFiles))
  lafObj <- vector(mode = "list", length = length(txtDataFiles))

  for (i in seq_along(chkFiles)) {
    spssIn <- spssFormatFiles[i]
    datIn <- txtDataFiles[i]

    ff <- parseScript_SPSS(spssIn, verbose = FALSE, outputFormat = "data.frame", encoding = spsFileEnc)
    ff$variableName <- tolower(ff$variableName)

    # special case for 2019 where field names can be same between both math and science naep files
    if (chkFiles[i] %in% c("naepmath") && year == 2019) {
      ff$variableName <- ifelse(ff$variableName %in% c("schoolid", "studenti"), ff$variableName, paste0("math_", ff$variableName)) # exclude merge vars, but ensure var names are unique across subjects
    }
    if (chkFiles[i] %in% c("naepsci") && year == 2019) {
      ff$variableName <- ifelse(ff$variableName %in% c("schoolid", "studenti"), ff$variableName, paste0("sci_", ff$variableName)) # exclude merge vars, but ensure var names are unique across subjects
    }
    if (chkFiles[i] %in% c("school_catalog") && year == 2019) {
      ff$variableName <- ifelse(ff$variableName %in% c("schoolid"), ff$variableName, paste0("schcat_", ff$variableName)) # exclude merge vars, but ensure var names are unique across subjects
    }
    lo <- laf_open_fwf(datIn, ff$dataType, ff$Width, ff$variableName)

    lafObj[[i]] <- lo
    names(lafObj)[i] <- chkFiles[i]

    spssFileFormat[[i]] <- ff
    names(spssFileFormat)[i] <- chkFiles[i]
  }

  if (year == 2019) {
    dataList <- buildHSTS_2019_dataList(fnTxt, lafObj, spssFileFormat)

    #precalc dim0 for speed boost to not have it calculate in edsf constructor
    studentN <- dataList$Student$nrow
    allCols <- unique(unlist(lapply(dataList, function(x){x$fileFormat$variableName})))
    colN <- length(allCols) + 1 #add one for ROWID from the 'cache' to match colnames() function☺
    dim0 <- c(studentN, colN) #be sent to ESDF
    
    linkingErrorSpecs <- NULL
    if(length(linkingErrorFiles)>0 && file.exists(linkingErrorFiles[1])){
      if(length(linkingErrorFiles) > 1){
        warning("Multiple HSTS 2019 Linking Error Data files located, only the first will be used.")
      }
      linkingErrorSpecs <- getHSTS19_LinkingErrorSpecs(linkingErrorFiles[1])
    }
    
  }

  yrAttrib <- getDataAttributes_HSTS(year) #year specific attributes
  
  esdf <- edsurvey.data.frame(userConditions = list(),
                              defaultConditions = NULL,
                              dataList = dataList,
                              weights = yrAttrib$weights,
                              pvvars = yrAttrib$pvvars,
                              subject = c("Mathematics", "Science"),
                              year = year,
                              assessmentCode = "Transcript Study",
                              dataType = "Transcript Study",
                              gradeLevel = "High School",
                              achievementLevels = yrAttrib$achievementLevels,
                              omittedLevels = yrAttrib$omittedLevels,
                              survey = "HSTS",
                              country = "USA",
                              psuVar = yrAttrib$psuVar, 
                              stratumVar = yrAttrib$stratumVar, 
                              jkSumMultiplier = 1,
                              validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                              reqDecimalConversion = FALSE, #decimal conversion is not needed
                              dim0 = dim0,
                              cacheDataLevelName = "Student") 
  
  #apply the linking errors to the edsurvey.data.frame
  if(!is.null(linkingErrorSpecs) && year == 2019){
    #math linking error
    esdf <- linkVarAugmentAB(data = esdf, 
                             linkingErrorSpecs = linkingErrorSpecs$math,
                             subscaleWeights = c(0.1, 0.3, 0.25, 0.35), 
                             subscales = c("math_num_oper", "math_meas_geo", "math_data_anal", "math_algebra"), 
                             composite = "math_composite",
                             dbapbaVarName = "math_dbapba",
                             weightVar = "math_finlnkwt")
    #science linking error
    esdf <- linkVarAugmentAB(data = esdf, 
                             linkingErrorSpecs =linkingErrorSpecs$sci,
                             subscaleWeights = c(1), 
                             subscales = c("sci_univariate"), 
                             composite = "sci_univariate",
                             dbapbaVarName = "sci_dbapba",
                             weightVar = "sci_finlnkwt")
  }
  
  return(esdf)
}

buildHSTS_2019_dataList <- function(filenames, lafList, ffList) {
  # for HSTS the main STUDENT file will be the 'base' level
  # The 'catalog' and 'transcript' data cannot be comingled together in a single data call
  dataList <- list()

  dataList[["School"]] <- dataListItem(
    lafObject = lafList[["school"]],
    fileFormat = ffList[["school"]],
    levelLabel = "School",
    forceMerge = FALSE,
    parentMergeLevels = NULL,
    parentMergeVars = NULL,
    mergeVars = NULL,
    ignoreVars = NULL,
    isDimLevel = FALSE
  )

  iVar <- unique(c(ffList[["school"]]$variableName))
  iVar <- intersect(iVar, ffList[["school_catalog"]]$variableName)
  dataList[["School_Catalog"]] <- dataListItem(
    lafObject = lafList[["school_catalog"]],
    fileFormat = ffList[["school_catalog"]],
    levelLabel = "School_Catalog",
    forceMerge = FALSE,
    parentMergeLevels = c("School"),
    parentMergeVars = c("schoolid"),
    mergeVars = c("schoolid"),
    ignoreVars = iVar,
    conflictLevels = c(
      "Student", "NAEP_Math", "NAEP_Sci",
      "Test", "Transcript", "Transcript_Catalog"
    ),
    isDimLevel = FALSE
  )

  # The BASE data level (main unit of analysis)
  iVar <- unique(c(ffList[["school"]]$variableName, ffList[["school_catalog"]]$variableName))
  iVar <- intersect(iVar, ffList[["student"]]$variableName)
  dataList[["Student"]] <- dataListItem(
    lafObject = lafList[["student"]],
    fileFormat = ffList[["student"]],
    levelLabel = "Student",
    forceMerge = FALSE,
    parentMergeLevels = c("School"),
    parentMergeVars = c("schoolid"),
    mergeVars = c("schoolid"),
    ignoreVars = iVar,
    isDimLevel = TRUE
  )

  # get ignore vars
  iVar <- unique(c(ffList[["student"]]$variableName))
  iVar <- intersect(iVar, ffList[["naepmath"]]$variableName)
  dataList[["NAEP_Math"]] <- dataListItem(
    lafObject = lafList[["naepmath"]],
    fileFormat = ffList[["naepmath"]],
    levelLabel = "NAEP_Math",
    forceMerge = FALSE,
    parentMergeLevels = c("Student"),
    parentMergeVars = c("studenti"),
    mergeVars = c("studenti"),
    ignoreVars = iVar,
    isDimLevel = FALSE
  )

  iVar <- unique(c(
    ffList[["student"]]$variableName,
    ffList[["naepmath"]]$variableName
  ))
  iVar <- intersect(iVar, ffList[["naepsci"]]$variableName)
  dataList[["NAEP_Science"]] <- dataListItem(
    lafObject = lafList[["naepsci"]],
    fileFormat = ffList[["naepsci"]],
    levelLabel = "NAEP_Science",
    forceMerge = FALSE,
    parentMergeLevels = c("Student"),
    parentMergeVars = c("studenti"),
    mergeVars = c("studenti"),
    ignoreVars = iVar,
    isDimLevel = FALSE
  )



  # TEST DATA CANNOT BE SIMULTANEOUSLY MERGED WITH CATALOG OR TRANSCRIPT DATA
  iVar <- unique(c(
    ffList[["school"]]$variableName,
    ffList[["student"]]$variableName,
    ffList[["naepmath"]]$variableName, ffList[["naepsci"]]$variableName
  ))
  iVar <- intersect(iVar, ffList[["tests"]]$variableName)
  dataList[["Test"]] <- dataListItem(
    lafObject = lafList[["tests"]],
    fileFormat = ffList[["tests"]],
    levelLabel = "Test",
    forceMerge = FALSE,
    parentMergeLevels = c("Student"),
    parentMergeVars = c("studenti"),
    mergeVars = c("studenti"),
    ignoreVars = iVar,
    conflictLevels = c("Transcript", "Transcript_Catalog", "School_Catalog"),
    isDimLevel = FALSE
  )


  # CATALOG DATA CANNOT BE SIMULTANEOUSLY MERGED WITH TEST OR TRANSCRIPT DATA
  iVar <- unique(c(
    ffList[["school"]]$variableName,
    ffList[["student"]]$variableName,
    ffList[["naepmath"]]$variableName, ffList[["naepsci"]]$variableName,
    ffList[["catalog"]]$variableName
  ))
  iVar <- intersect(iVar, ffList[["trnscrpt"]]$variableName)
  dataList[["Transcript"]] <- dataListItem(
    lafObject = lafList[["trnscrpt"]],
    fileFormat = ffList[["trnscrpt"]],
    levelLabel = "Transcript",
    forceMerge = FALSE,
    parentMergeLevels = c("Student"),
    parentMergeVars = c("studenti"),
    mergeVars = c("studenti"),
    ignoreVars = iVar,
    conflictLevels = c("Test", "School_Catalog"),
    isDimLevel = FALSE
  )

  iVar <- unique(c(
    ffList[["school"]]$variableName,
    ffList[["student"]]$variableName,
    ffList[["naepmath"]]$variableName, ffList[["naepsci"]]$variableName
  ))
  iVar <- intersect(iVar, ffList[["catalog"]]$variableName)
  #special variables that are duplicates that we want to handle specially
  renameVar <- c("catsrce", "cattype")
  iVar <- iVar[!iVar %in% renameVar]
  
  #rename LaF object column names
  idx <- which(lafList[["catalog"]]@column_names %in% renameVar)
  lafList[["catalog"]]@column_names[idx] <- paste0("trncat_", lafList[["catalog"]]@column_names[idx])
  
  #rename FileFormat variable names
  idx <- which(ffList[["catalog"]]$variableName %in% renameVar)
  ffList[["catalog"]]$variableName[idx] <- paste0("trncat_", ffList[["catalog"]]$variableName[idx])
  
  dataList[["Transcript_Catalog"]] <- dataListItem(lafObject = lafList[["catalog"]],
                                               fileFormat = ffList[["catalog"]],
                                               levelLabel = "Transcript_Catalog",
                                               forceMerge = FALSE,
                                               parentMergeLevels = c("Transcript"),
                                               parentMergeVars = c("catlogid"),
                                               mergeVars = c("catlogid"),
                                               ignoreVars = iVar,
                                               conflictLevels = c("Test", "School_Catalog"),
                                               isDimLevel = FALSE)
  return(dataList)
}

getHSTS_DataFileNames <- function(year) {
  # define a named list that we will try to grab with the "year" being the name
  chkList <- list("2019" = c("catalog", "naepmath", "naepsci", "school", "student", "tests", "trnscrpt"))

  year <- as.character(year) # coerce to character for name check
  res <- chkList[[year]]

  if (is.null(res)) {
    stop("Undefined Year Specified in getHSTS_DataFileNames: ", dQuote(year), ".")
  }
  return(res)
}

getDataAttributes_HSTS <- function(year) {
  weights <- list()
  pvvars <- list()
  aL <- list()
  omittedLevels <- c()
  psuVar <- c()
  stratumVar <- c()
  
  
  if(year == 2019) {
    #get weight detail, 2019 doesn't include any taylor psu/stratum variables only has JK variance
    weights[["finstuwt"]] <- list(jkbase = "repwt", jksuffixes = as.character(1:62)) #student file (unlinked weight)
    weights[["math_finlnkwt"]] <- list(jkbase = "math_lrepwt", jksuffixes = as.character(1:62)) #math linked weight (for math NAEP plausible values)
    weights[["sci_finlnkwt"]] <- list(jkbase = "sci_lrepwt", jksuffixes = as.character(1:62)) #science linked weight (for science NAEP plausible values)
    #don't set a default weight to force user selection in analysis functions, do this by setting empty string value
    attributes(weights)$default <- ""
    
    #mathematics
    alMath <- achievementLevelsHelp("Grade 12", "2019", "Mathematics", "N") #from descriptionOfFile.R file used for readNAEP
    pvvars[["math_num_oper"]] <- list(varnames = paste0("math_pv1", sprintf("%02d", 1:20)), achievementLevel = alMath)
    pvvars[["math_meas_geo"]] <- list(varnames = paste0("math_pv2", sprintf("%02d", 1:20)), achievementLevel = alMath)
    pvvars[["math_data_anal"]] <- list(varnames = paste0("math_pv3", sprintf("%02d", 1:20)), achievementLevel = alMath)
    pvvars[["math_algebra"]] <- list(varnames = paste0("math_pv4", sprintf("%02d", 1:20)), achievementLevel = alMath)
    pvvars[["math_composite"]] <- list(varnames = paste0("math_pvc", sprintf("%02d", 1:20)), achievementLevel = alMath)

    # science
    alSci <- achievementLevelsHelp("Grade 12", "2019", "Science", "N") # from descriptionOfFile.R file used for readNAEP
    pvvars[["sci_univariate"]] <- list(varnames = paste0("sci_pvun", paste0(1:20)), achievementLevel = alSci)
    
    aL <- list(Mathematics = alMath,
               Science = alSci)
    
    omittedLevels <- c(NA, "(Missing)", "Missing", "Omitted", "No recorded grade point average", 
                       "Not reported", "Multiple responses/Don't know/Omitted/Missing",
                       "Missing graduation credits data", "NAEP assessment not taken",
                       "Not applicable", "Incomplete transcript")
    
    psuVar <- NULL
    stratumVar <- NULL
  }
  
  return(list(weights = weights,
              pvvars = pvvars,
              achievementLevels = aL,
              omittedLevels = omittedLevels,
              psuVar = psuVar,
              stratumVar = stratumVar))
}

getHSTS19_LinkingErrorSpecs <- function(filePath) {
  
  math1 <- readxl::read_excel(filePath, sheet = "MRPS1") #numbers & operation
  math2 <- readxl::read_excel(filePath, sheet = "MRPS6") #measurement & geography
  math3 <- readxl::read_excel(filePath, sheet = "MRPS4") #data analysis
  math4 <- readxl::read_excel(filePath, sheet = "MRPS5") #algebra
  
  sci1 <- readxl::read_excel(filePath, sheet = "SRPS")
  
  linkErr <- list(math = vector("list", length = 4), sci = vector("list", length = 1))
  names(linkErr$math) <- c("math_num_oper", "math_meas_geo", "math_data_anal", "math_algebra")
  names(linkErr$sci) <- c("sci_univariate")

  #for the math items (4 subscale score plausible values)
  for(i in 1:4){
    m <- get(paste0("math", i))
    m <- setDT(m)
    
    dba <- subset(m, m$DBAPBA == 1) #subset to digital based assessment
    pba <- subset(m, m$DBAPBA == 2) #subset to paper based assessment
    
    #build matrix for replicate weight sample adjustments (A and B with 62 records for each replicate weight)
    sampdba <- data.table::melt(dba, id.vars = "DBAPBA", measure.vars = list(A = grep("^[A]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                         B = grep("^[B]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE)),
                              variable.name = "JKRep")
    samppba <- data.table::melt(pba, id.vars = "DBAPBA", measure.vars = list(A = grep("^[A]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                             B = grep("^[B]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE)),
                                variable.name = "JKRep")
    
    sampMatrix <- list(Digital = data.matrix(sampdba[ , c("A", "B")]),
                       Paper = data.matrix(samppba[ , c("A", "B")]))
    
    #build matrix for plausible value imputation adjustments (A and B each with 5 plausible values with 20 replicates each)
    
    matAdba <- matrix(nrow = 20, ncol = 5)
    matBdba <- matrix(nrow = 20, ncol = 5)
    matCdba <- matrix(nrow = 20, ncol = 5)
    matApba <- matrix(nrow = 20, ncol = 5)
    matBpba <- matrix(nrow = 20, ncol = 5)
    matCpba <- matrix(nrow = 20, ncol = 5)
    
    for(ii in 1:5){
      zdba <- data.table::melt(dba, id.vars = "DBAPBA", measure.vars = list(A = grep(paste0("^[A]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                                       B = grep(paste0("^[B]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                                       C = grep(paste0("^[C]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE)),
                                        variable.name = "PVRep")
      
      zpba <- data.table::melt(pba, id.vars = "DBAPBA", measure.vars = list(A = grep(paste0("^[A]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE),
                                                                            B = grep(paste0("^[B]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE),
                                                                            C = grep(paste0("^[C]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE)),
                               variable.name = "PVRep")
      
      matAdba[ , ii] <- zdba$A
      matBdba[ , ii] <- zdba$B
      matCdba[ , ii] <- zdba$C
      matApba[ , ii] <- zpba$A
      matBpba[ , ii] <- zpba$B
      matCpba[ , ii] <- zpba$C
      
    } #end for(ii in 1:5)
    
    linkErr$math[[i]] <- list(repWSamp = sampMatrix, pvvarImp = list(Digital = list(A = matAdba, B = matBdba, C = matCdba),
                                                                     Paper = list(A = matApba, B = matBpba, C = matCpba)))
  } #end for(i in 1:4)

  #for the science items (1 univariate scale score)
  for(i in 1:1){
    s <- get(paste0("sci", i))
    s <- setDT(s)
    
    dba <- subset(s, s$DBAPBA == 1) #digital based assessment
    pba <- subset(s, s$DBAPBA == 2) #paper based assessment
    
    #build matrix for replicate weight sample adjustments (A and B with 62 records for each replicate weight)
    sampdba <- data.table::melt(dba, id.vars = "DBAPBA", measure.vars = list(A = grep("^[A]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                        B = grep("^[B]samp\\d{1,2}$", colnames(dba), value = TRUE, ignore.case = TRUE)),
                             variable.name = "JKRep")
    samppba <- data.table::melt(pba, id.vars = "DBAPBA", measure.vars = list(A = grep("^[A]samp\\d{1,2}$", colnames(pba), value = TRUE, ignore.case = TRUE),
                                                                           B = grep("^[B]samp\\d{1,2}$", colnames(pba), value = TRUE, ignore.case = TRUE)),
                                variable.name = "JKRep")
    
    sampMatrix <- list(Digital = data.matrix(sampdba[ , c("A", "B")]),
                       Paper = data.matrix(samppba[ , c("A", "B")]))
    
    #build matrix for plausible value imputation adjustments for dba and pba (A and B each with 5 plausible values with 20 replicates each)
    matAdba <- matrix(nrow = 20, ncol = 5)
    matBdba <- matrix(nrow = 20, ncol = 5)
    matCcba <- matrix(nrow = 20, ncol = 5)
    matApba <- matrix(nrow = 20, ncol = 5)
    matBpba <- matrix(nrow = 20, ncol = 5)
    matCpba <- matrix(nrow = 20, ncol = 5)
    
    for(ii in 1:5){
      zdba <- data.table::melt(dba, id.vars = "DBAPBA", measure.vars = list(A = grep(paste0("^[A]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                       B = grep(paste0("^[B]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE),
                                                                       C = grep(paste0("^[C]meas", ii, "_\\d{1,2}$"), colnames(dba), value = TRUE, ignore.case = TRUE)),
                            variable.name = "PVRep")
      zpba <- data.table::melt(pba, id.vars = "DBAPBA", measure.vars = list(A = grep(paste0("^[A]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE),
                                                                            B = grep(paste0("^[B]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE),
                                                                            C = grep(paste0("^[C]meas", ii, "_\\d{1,2}$"), colnames(pba), value = TRUE, ignore.case = TRUE)),
                               variable.name = "PVRep")
      
      matAdba[ , ii] <- zdba$A
      matBdba[ , ii] <- zdba$B
      matCdba[ , ii] <- zdba$C
      matApba[ , ii] <- zpba$A
      matBpba[ , ii] <- zpba$B
      matCpba[ , ii] <- zpba$C
    } #end for(ii in 1:5)
    
    
    linkErr$sci[[i]] <- list(repWSamp = sampMatrix, pvvarImp = list(Digital = list(A = matAdba, B = matBdba, C = matCdba),
                                                                    Paper = list(A = matApba, B = matBpba, C = matCpba)))
  } #end for(i in 1:1)
  
  return(linkErr)
}

linkVarAugmentAB <- function(data, linkingErrorSpecs, subscaleWeights, subscales, composite, weightVar, dbapbaVarName = "dbapba") {
  
  PVVars <- getAttributes(data = data, attribute = "pvvars")
  PVVarsDefault <- attributes(PVVars)$default
  # actual variables
  scaledPVs <- list()
  for(i in 1:length(subscales)) {
    scaledPVs <- c(scaledPVs, list(getPlausibleValue(subscales[i], data = data)))
  }
  names(scaledPVs) <- subscales
  PVs <- getPlausibleValue(composite, data = data)
  # the DBA subset
  if(!dbapbaVarName %in% colnames(data)) {
    stop(paste0("Cannot find variable ", dQuote(dbapbaVarName), " on data."))
  }
  dbapba <- getData(data, varnames = dbapbaVarName, drop = TRUE, omittedLevels = FALSE) #drop NA's here for HSTS as only subset of students have NAEP math or sci score
  DBASS <- dbapba %in% c("DBA", "Digital")
  nDBA <- sum(DBASS)  # number in DBA subset
  # the PBA subset
  PBASS <- dbapba %in% c("PBA", "Paper")
  nPBA <- sum(PBASS) # number in the PBA subset

  # number of subscales
  K <- length(subscaleWeights)
  if(K != length(scaledPVs)) {
    stop(paste0("The argument ", dQuote("scaledPVs"), " is a list with one element per subscale, so it should have the same length as ", dQuote("subscaleWeights"), " and ",dQuote("thetaPVs"),". Each element of ", dQuote("scaledPVs"), " is a vector of plausible values."))
  }
  
  #get the weight to be used
  wList <- getAttributes(data = data, attribute = "weights")
  W <- wList[[weightVar]]
  repWs <- paste0(W$jkbase, W$jksuffixes)
  repWsDBA <- data[DBASS, repWs]
  repWsPBA <- data[PBASS, repWs]
  
  # subscale weights called "beta" in documentation
  beta <- subscaleWeights
  
  ### measurement variance agumentation
  # create PV data frame to minimize calls to getData
  pvData <- data[ ,PVs]
  
  # for each PV (column in the RAM)
  scaledPV <- data[ , unlist(scaledPVs)]
  
  for(i in 1:length(repWs)) {
    # y1 is for DBA
    y1 <- rep(0, nDBA)
    # z1 is for PBA
    x1 <- rep(0, nPBA)
    for(k in 1:K) {
      # get mean, stdev, PBA, use ith replicate weight
      y_k <- scaledPV[DBASS, scaledPVs[[k]][1] ]
      x_k <- scaledPV[PBASS, scaledPVs[[k]][1] ]
      # variable transformations.  The linking error specs subscales need to match the ordering of the subscales object and subscaleWeights
      A <- linkingErrorSpecs[[subscales[k]]]$repWSamp$Digital[i, "A"]
      B <- linkingErrorSpecs[[subscales[k]]]$repWSamp$Digital[i, "B"]
      
      data[DBASS, paste0(subscales[k], "_linking_samp_", i)] <- (A*y_k + B)
      data[PBASS, paste0(subscales[k], "_linking_samp_", i)] <- x_k
      # sum y1
      y1 <- y1 + beta[k] * (A*y_k + B) #ERRORS here (originally 'theta_k'), trying 'y_k' to match above?
      x1 <- x1 + beta[k] * x_k
    }
    data[DBASS, paste0(composite, "_linking_samp_", i)] <- y1
    
    compositePBA <- pvData[PBASS ,PVs[1]]
    data[PBASS, paste0(composite, "_linking_samp_", i)] <- compositePBA
    if(max(abs(x1 - compositePBA)) > 0.02) {
      warning("Inacurate data or subscale weights, linking error is inacurately estimated due to low precision inputs. Disagreement on composite of ", max(abs(x1 - compositePBA)), "\n")
    }
  }
  
  for(n in 1:5) {
    # for each RAM column (5)
    for(j in 1:20) {
      y1 <- rep(0, nDBA)
      x1 <- rep(0, nPBA)
      for(k in 1:K) {
        A <- linkingErrorSpecs[[subscales[k]]]$pvvarImp$Digital$A[j, n]
        B <- linkingErrorSpecs[[subscales[k]]]$pvvarImp$Digital$B[j, n]
        C <- linkingErrorSpecs[[subscales[k]]]$pvvarImp$Paper$C[j, n]
        
        y_k <- scaledPV[DBASS, scaledPVs[[k]][j] ]
        x_k <- scaledPV[PBASS, scaledPVs[[k]][C] ] ###C calculated above using paper RAM
        
        # variable transformations
        data[DBASS, paste0(subscales[k], "_linking_imp_", 20*(n-1)+j)] <- (A*y_k + B)
        data[PBASS, paste0(subscales[k], "_linking_imp_", 20*(n-1)+j)] <- x_k
        # y is the sum over the k subscales
        y1 <- y1 + beta[k] * (A*y_k + B)
        x1 <- x1 + beta[k] * x_k
      }
      # write composite variable for DBA
      data[DBASS, paste0(composite, "_linking_imp_", 20*(n-1)+j)] <- y1
      # write composite variable for PBA
      jpv <- linkingErrorSpecs[[subscales[k]]]$pvvarImp$Paper$C[j, n]
      compositePBA <- pvData[PBASS ,PVs[jpv]]
      data[PBASS, paste0(composite, "_linking_imp_", 20*(n-1)+j)] <- compositePBA
      if(max(abs(x1 - compositePBA)) > 0.02) {
        warning("Inacurate data or subscale weights, linking error is inacurately estimated due to low precision inputs. Disagreement on composite of ", max(abs(x1 - compositePBA)), "\n")
      }
    }
  }
  
  # construct composite PV var
  newPV <- list(list(estVarnames=PVs,
                     impVarnames=paste0(composite,"_linking_imp_",1:100),
                     sampVarnames=paste0(composite,"_linking_samp_",1:length(repWs)),
                     achievementLevel=data[["pvvars"]][[composite]][["achievementLevel"]]))
  names(newPV) <- paste0(composite, "_linking")
  for(k in 1:K) {
    # sometimes
    if(subscales[k] != composite) {
      newPVi <- list(estVarnames=scaledPVs[[k]],
                     impVarnames=paste0(subscales[k],"_linking_imp_",1:100),
                     sampVarnames=paste0(subscales[k],"_linking_samp_",1:length(repWs)))
      newPV[[paste0(subscales[k], "_linking")]] <- newPVi
    }
  }
  pvv <- c(data[["pvvars"]], newPV)
  attributes(pvv)$default <- PVVarsDefault
  data[["pvvars"]] <- pvv
  return(data)
}

