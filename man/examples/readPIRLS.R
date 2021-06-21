\dontrun{
nor <- readPIRLS("~/PIRLS/2011", countries = c("nor"))
gg <- getData(nor, c("itsex", "totwgt", "rrea"))
head(gg)
edsurveyTable(rrea ~ itsex, nor)
}
