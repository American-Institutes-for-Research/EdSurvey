\dontrun{
  # root argument will vary by operating system conventions
  downloadTALIS(root = "C:/EdSurveyData", years = 2018)
  
  # cache=TRUE will download then process the datafiles
  downloadTALIS(root = "C:/EdSurveyData", years = 2015, cache = TRUE)
  
  # set verbose=FALSE for silent output
  # if year not specified, download all years
  downloadTALIS(root="C:/EdSurveyData", verbose = FALSE)
}
