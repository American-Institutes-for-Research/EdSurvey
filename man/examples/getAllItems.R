\dontrun{
  #TIMSS Example
  t15 <- readTIMSS("~/TIMSS/2015", "usa", 4)
  
  showPlausibleValues(t15) #view constructs in console
  
  #ensure we have all data needed for mml.sdf on light.edsurvey.data.frame
  #must be specified ahead of time.  the 'getAllItems' function makes this easy
  mathItems <- getAllItems(t15, "mmat") #get mathematics items
  sciItems <- getAllItems(t15, "ssci") #get science items
  allItems <- getAllItems(t15, construct = "NULL")
  
  wgtVar <- "totwgt"
  psustr <- c(getPSUVar(t15, wgtVar), getStratumVar(t15, wgtVar))
  lsdf <- getData(data = t15,
                  varnames = c("ROWID", "mmat", mathItems, psustr, wgtVar),
                  omittedLevels = FALSE,
                  addAttributes = TRUE) #builds light.edsurvey.data.frame
  
  #as a light.edsurvey.data.frame all elements must be present
  mml.sdf(mmat ~ 1, lsdf, weightVar = "totwgt")
  
  #as edsurvey.data.frame elements retrieved automatically for user
  mml.sdf(mmat ~ 1, t15, weightVar = "totwgt") 
  
  #NAEP example
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
  
  allItems <- getAllItems(sdf, construct = NULL)
  algebraItems <- getAllItems(sdf, construct = "algebra")
}
