\dontrun{
eng <- readCivEDICCS("~/ICCS/2009/", countries = c("eng"),
                     gradeLvl = 8, dataSet = "student")
gg <- getData(eng, c("famstruc", "totwgts", "civ"))
head(gg)
edsurveyTable(civ ~ famstruc, eng)
}
