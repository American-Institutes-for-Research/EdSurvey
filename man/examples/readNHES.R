\dontrun{
rootPath <- "~/"

#get instructions for obtaining NHES data 
downloadNHES()

#get SPSS *.sav file paths of all NHES files for 2012 and 2016
filesToImport <- list.files(path = file.path(rootPath, "NHES", c(2012, 2016)), 
                            pattern="\\.sav$", 
                            full.names = TRUE, 
                            recursive = TRUE)

#import all files to edsurvey.data.frame.list object
esdfList <- readNHES(savFiles = filesToImport, surveyCode = "auto", 
                     forceReread = FALSE, verbose = TRUE)

viewNHES_SurveyCodes() #view NHES survey codes in console

#get the full file path to the 2016 ATES NHES survey
path_ates2016 <- list.files(path = file.path(rootPath, "NHES", "2016"), 
                            pattern=".*ates.*[.]sav$", full.names = TRUE)

#explicitly setting the surveyCode parameter (if required)
esdf <- readNHES(savFiles = path_ates2016, surveyCode = "ATES_2016", 
                 forceReread = FALSE, verbose = TRUE)

#search for variables in the edsurvey.data.frame
searchSDF("sex", esdf)
}
