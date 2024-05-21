\dontrun{
# read-in student file including weight file as default
els_df <- readELS("~/ELS/2002") #student level with weights)
d <- getData(data=els_df, varnames=c("stu_id", "bysex", "bystlang"))
summary(d)



# read-in with parameters specified (student level with weights)
els_wgt_df <- readELS(path = "~/ELS/2002", 
                      filename = "els_02_12_byf3pststu_v1_0.sav", 
                      wgtFilename = "els_02_12_byf3stubrr_v1_0.sav",
                      verbose = TRUE, 
                      forceReread = FALSE)

# read-in with parameters specified (school level, no separate weight replicate file)
els_sch_df <- readELS(path = "~/ELS/2002", 
                      filename = "els_02_12_byf1sch_v1_0.sav", 
                      wgtFilename = NA,
                      verbose = TRUE, 
                      forceReread = FALSE)
}
