## ----eval=FALSE---------------------------------------------------------------
#  install.packages(c("EdSurvey", "tidyEdSurvey"))

## ----echo=FALSE,warning=FALSE,message=FALSE-----------------------------------
library("EdSurvey")

## -----------------------------------------------------------------------------
naep_primer <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
naep_primer

## -----------------------------------------------------------------------------
summary2("composite", data=naep_primer)

## -----------------------------------------------------------------------------
searchSDF(string="parent", data=naep_primer)

## -----------------------------------------------------------------------------
edsurveyTable(composite ~ pared, data=naep_primer)

## ----recode pared-------------------------------------------------------------
naep_primer$pared_recode <- ifelse(naep_primer$pared %in% "Did not finish H.S.", "less than HS", "unknown")
naep_primer$pared_recode <- ifelse(naep_primer$pared %in% "Graduated H.S.", "HS", naep_primer$pared_recode)
naep_primer$pared_recode <- ifelse(naep_primer$pared %in% "Some ed after H.S.", "any after HS", naep_primer$pared_recode)
naep_primer$pared_recode <- ifelse(naep_primer$pared %in% "Graduated college", "any after HS", naep_primer$pared_recode)
# the tidyEdSurvey package allows this call to with to work
require(tidyEdSurvey)
with(naep_primer, table(pared_recode, pared))

## ----use pared_recode---------------------------------------------------------
lm1 <- lm.sdf(composite ~ pared_recode, data=naep_primer)
summary(lm1)

## -----------------------------------------------------------------------------
waldTest(lm1, "pared_recode")

## ----eval=FALSE---------------------------------------------------------------
#  downloadTIMSS("~/EdSurveyData/", years=2015)
#  timss_NA15 <- readTIMSS("~/EdSurveyData/TIMSS/2015/", countries=c("usa", "can"), grade=8)
#  searchSDF(c("parent", "education"), data=timss_NA15)
#  edsurveyTable(data=timss_NA15, mmat ~ bsdgedup)

## ----eval=FALSE---------------------------------------------------------------
#  downloadPISA("~/EdSurveyData/", years=2015)
#  pisa_NA15 <- readPISA("~/EdSurveyData/PISA/2015/", countries=c("usa", "can", "max"))
#  searchSDF(c("parent", "education"), data=pisa_NA15)
#  edsurveyTable(data=pisa_NA15, math ~ hisced)

