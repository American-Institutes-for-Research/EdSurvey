\dontrun{
#download SSOCS data for years 2016 and 2018 
downloadSSOCS(years = c(2016, 2018))

rootPath <- "~/"# may need to change this 
#get SAS *.sas7bdat file paths of all SSOCS files for 2016 and 2018
filesToImport <- list.files(path = file.path(rootPath, "SSOCS", c(2016, 2018)),
                            pattern="\\.sas7bdat$",
                            full.names = TRUE)
  
#import all files to edsurvey.data.frame.list object
esdfList <- readSSOCS(sasDataFiles = filesToImport,
                      years = c(2016, 2018),
                      forceReread = FALSE,
                      verbose = TRUE)

#reading in the 2018 to an edsurvey.data.frame object
esdf <- readSSOCS(sasDataFiles = file.path(rootPath, "SSOCS/2018/pu_ssocs18.sas7bdat"),
                  years = 2018,
                  forceReread = FALSE,
                  verbose = TRUE)
  
#search for variables in the edsurvey.data.frame containing the word 'bully'
searchSDF(string="bully", data=esdf)
}