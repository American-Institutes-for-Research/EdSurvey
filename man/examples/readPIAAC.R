\dontrun{
# the following call returns an edsurvey.data.frame to PIAAC for Canada
can <- readPIAAC("~/PIAAC/Cycle 1/", countries = "can")

# extract a data.frame with a few variables
gg <- getData(can, c("c_d05","ageg10lfs"))  
head(gg)

# conduct an analysis on the edsurvey.data.frame
edsurveyTable(~ c_d05 + ageg10lfs, data = can)

# the following call returns an edsurvey.data.frame to PIAAC for Canada
can <- readPIAAC("~/PIAAC/Cycle 1/", countries = "can", us)

# There are two years of usa data for round 1: 2012-2014 and 2017. 
# The user must specify which usa year they want with the optional "usaOption" argument. 
# Otherwise, the read function will return usa 2012-2014. See "?readPIACC()" for more info. 

# read in usa 2012-2014 
usa12 <- readPIAAC("/Users/ebuehler/EdSurveyData/PIAAC/Cycle 1", countries = "usa", usaOption="12_14")
# read in usa 2017 
usa17 <- readPIAAC("/Users/ebuehler/EdSurveyData/PIAAC/Cycle 1", countries = "usa", usaOption="17")
# if reading in all piaac data, the user can still specify usa option. 
# Otherwise, by default 2012-1014 will be used when reading in all piaac data. 
all_piaac <- readPIAAC("/Users/ebuehler/EdSurveyData/PIAAC/Cycle 1", countries = "*", usaOption="17")
}
