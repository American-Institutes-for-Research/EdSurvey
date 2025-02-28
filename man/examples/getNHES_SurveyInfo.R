\dontrun{
#retrieves the NHES survey meta-data to a data.frame
surveyInfo <- getNHES_SurveyInfo()
  
#View the survey data where the year is equal to 2016 in RStudio
View(subset(surveyInfo, surveyInfo$Year==2016))
}