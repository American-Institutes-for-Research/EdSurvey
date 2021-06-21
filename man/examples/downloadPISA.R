\dontrun{
# download PISA 2012 data (for all three databases)
downloadPISA(years = 2012, database = c("INT","CBA","FIN"), root="~/")

# download PISA 2009, 2012, and 2015 data (International Database only) 
# to C:/PISA/2009, C:/PISA/2012, and C:/PISA/2015 folders, respectively
downloadPISA(years = c(2009,2012,2015), root="~/")  
}
