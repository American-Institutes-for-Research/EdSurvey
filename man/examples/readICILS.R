\dontrun{
pol <- readICILS("~/ICILS/2013", countries = "pol", dataSet = "student")
gg <- getData(data=pol, varnames=c("idstud", "cil", "is1g18b"))
head(gg)
edsurveyTable(formula=cil ~ is1g18b, pol)
}
