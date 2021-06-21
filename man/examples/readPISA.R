\dontrun{
# the following call returns an edsurvey.data.frame to 
# PISA 2012 International Database for Singapore
sgp2012 <- readPISA(path = "~/PISA/2012", database = "INT", countries = "sgp")

# extract a data.frame with a few variables
gg <- getData(sgp2012, c("cnt","read","w_fstuwt"))  
head(gg)

# conduct an analysis on the edsurvey.data.frame
edsurveyTable(read ~ st04q01 + st20q01, data = sgp2012)
}
