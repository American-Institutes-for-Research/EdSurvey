\dontrun{

fld <- "~/EdSurveyData/BTLS"
datPath <- file.path(fld, "ASCII Data File", "BTLS2011_12.dat")
spsPath <- file.path(fld, "Input Syntax for Stata and SPSS", "BTLS2011_12.sps")

#read in the data to an edsurvey.data.frame
btls <- readBTLS(datPath, spsPath, verbose = TRUE)

dim(btls)
}
