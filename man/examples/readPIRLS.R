\dontrun{
nor <- readPIRLS("~/PIRLS/2011", countries = c("nor"))
gg <- getData(data=nor, varnames=c("itsex", "totwgt", "rrea"))
head(gg)
edsurveyTable(formula=rrea ~ itsex, nor)
}
