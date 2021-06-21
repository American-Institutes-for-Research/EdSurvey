\dontrun{
swe <- readTIMSSAdv("~/TIMSSAdv/2015",
                    countries = c("swe"), subject = "math")
gg <- getData(swe, c("itsex", "totwgt", "malg"))
head(gg)
edsurveyTable(malg ~ itsex, swe)
}
