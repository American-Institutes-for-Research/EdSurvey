\dontrun{
#TALIS 2018 - school level data for all countries
talis18 <- readTALIS(path = "~/TALIS/2018", 
                     isced = "b", 
                     dataLevel = "school", 
                     countries = "*")

#unweighted summary
result <- summary2(talis18, "tc3g01", weightVar = "")

#print usa results to console
result$usa

# the following call returns an edsurvey.data.frame to TALIS 2013 
# for US teacher-level data at secondary level
usa2013 <- readTALIS(path = "~/TALIS/2013", isced = "b",
                     dataLevel = "teacher", countries = "usa")

# extract a data.frame with a few variables
gg <- getData(usa2013, c("tt2g05b", "tt2g01"))  
head(gg)

# conduct an analysis on the edsurvey.data.frame
edsurveyTable(tt2g05b ~ tt2g01, data = usa2013) 
}
