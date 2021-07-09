\dontrun{
  #Return an edsurvey.data.frame for only the PISA YAFS dataset.
  #Either omit, or set the esdf_PISA2012_USA to a NULL value.
  yafs <- readPISA_YAFS(datPath = "C:/EdSurveyData/PISA YAFS/2016/PISA_YAFS2016.dat",
                            spsPath = "C:/EdSurveyData/PISA YAFS/2016/PISA_YAFS2016.sps",
                            esdf_PISA2012_USA = NULL)
  
  #If wanting to analyze the PISA YAFS dataset in conjunction with the PISA 2012 
  #United States of America (USA) dataset, it should be read in first to an edsurvey.data.frame.
  #Then pass the resulting edsurvey.data.frame as a parameter for the
  #esdf_PISA2012_USA argument. No other edsurvey.data.frames are supported.
  usa2012 <- readPISA("C:/EdSurveyData/PISA/2012", database = "INT", countries = "usa")
  
  yafs <- readPISA_YAFS(datPath = "C:/EdSurveyData/PISA YAFS/2016/PISA_YAFS2016.dat",
                            spsPath = "C:/EdSurveyData/PISA YAFS/2016/PISA_YAFS2016.sps",
                            esdf_PISA2012_USA = usa2012)
}
