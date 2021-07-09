#function to determine if/when a cacheMetaFile requires it to reprocessed due to code changes or other modifications
#cacheFileMetaVer is a single value to indicate the .meta file version of the .meta file you wish to test
#returns a logical value.  
#TRUE == The cache .meta file requires to be updated and requires reprocessing
#FALSE == The cache .meta file does not require updating and reprocessing
#' @importFrom utils packageVersion
cacheMetaReqUpdate <- function(cacheFileMetaVer, surveyName=NULL){
  #if the cacheFileMetaVer is null it means the .meta file was build before we implemented this version
  if(is.null(cacheFileMetaVer)){
    return(TRUE)
  }
  
  #unlist the value of 'packageVersion'as it returns a list of integers
  cacheFileMetaVer <- as.numeric(unlist(cacheFileMetaVer)) #the version number of the .meta file stored for the survey when the .meta file was created
  currentPkgVer <- unlist(packageVersion("EdSurvey")) #the current version of the installed EdSurvey package
  
  if(length(cacheFileMetaVer)==0){ #no cacheFileMetaVer specified
    return(TRUE)
  }
  
  surveyDef <- c("NAEP", #NAEP Dataset
               "TIMSS", "TIMSS Advanced", "PIRLS", "ePIRLS", "ICILS", "ICCS", "CivED", #IEA Datasets
               "PIAAC", "PISA", "TALIS", #OECD Datasets
               "ECLS_K", "ECLS_K2011", "ELS", "HSLS", "B&B2001", "B&B2003", "HS&B", "BPS1994") #NCES longitudinal Dataset
  
  #build our lookup table
  surveyLookup <- data.frame(survey=surveyDef, cacheVer=vector("integer", length(surveyDef)), stringsAsFactors = FALSE)
  
  #specify the cacheFileVersion here that is the most up to date version for that specific survey type
  surveyLookup[surveyLookup$survey=="NAEP", "cacheVer"] <- 2
  surveyLookup[surveyLookup$survey=="TIMSS", "cacheVer"] <- 5
  surveyLookup[surveyLookup$survey=="TIMSS Advanced", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="PIRLS", "cacheVer"] <- 4
  surveyLookup[surveyLookup$survey=="ePIRLS", "cacheVer"] <- 4
  surveyLookup[surveyLookup$survey=="ICILS", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="ICCS", "cacheVer"] <- 6
  surveyLookup[surveyLookup$survey=="CivED", "cacheVer"] <- 6
  surveyLookup[surveyLookup$survey=="PIAAC", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="PISA", "cacheVer"] <- 6
  surveyLookup[surveyLookup$survey=="TALIS", "cacheVer"] <- 4
  surveyLookup[surveyLookup$survey=="ECLS_K", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="ECLS_K2011", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="ELS", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="HSLS", "cacheVer"] <- 2
  surveyLookup[surveyLookup$survey=="ECLS_B", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="B&B2001", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="B&B2003", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="HS&B", "cacheVer"] <- 1
  surveyLookup[surveyLookup$survey=="BPS1994", "cacheVer"] <- 1
  
  if(!any(surveyLookup$survey %in% surveyName)){
    warning("Survey name not recognized while checking cache (.meta) version. Forcing cache (.meta) file to be updated.")
    return(TRUE) 
  }
  
  testVal <- surveyLookup[surveyLookup$survey==surveyName, "cacheVer"]
  
  #test the .meta cache version vs the version specified
  return(cacheFileMetaVer < testVal)
}
