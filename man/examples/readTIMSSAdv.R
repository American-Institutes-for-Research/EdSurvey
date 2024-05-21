\dontrun{
swe <- readTIMSSAdv("~/TIMSSAdv/2015",
                    countries = c("swe"), subject = "math")
gg <- getData(data=swe, varnames=c("itsex", "totwgt", "malg"))
head(gg)
edsurveyTable(formula=malg ~ itsex, swe)
}
