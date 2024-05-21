\dontrun{
usa <- read_ePIRLS("~/ePIRLS/2016", countries = c("usa"))
gg <- getData(data=usa, varnames=c("itsex", "totwgt", "erea"))
head(gg)
edsurveyTable(formula=erea ~ itsex, data=usa)
}
