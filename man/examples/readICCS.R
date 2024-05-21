\dontrun{
eng <- readCivEDICCS("~/ICCS/2009/", countries = c("eng"),
                     gradeLvl = 8, dataSet = "student")
gg <- getData(getData=eng, varnames=c("famstruc", "totwgts", "civ"))
head(gg)
edsurveyTable(formula=civ ~ famstruc, data=eng)
}
