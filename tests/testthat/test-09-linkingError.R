if(FALSE){
require(testthat)
context("nope")
d1 <- readNAEP("I:/NCES/NCES_Dev/EdSurvey Project/NAEP Data Companion/NAEP 2018 SS_Review 4/Y49CGH/Data/C49NT2AT.dat")

m19 <- readNAEP("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/G12_2019MR_SDT/Y50MR3/Data/M50NT3AT.dat") # math

edsurveyTable(composite ~ dsex, data=m19, jrrIMax=1)
edsurveyTable(composite ~ dsex, data=m19, jrrIMax=Inf)
edsurveyTable(composite_linking ~ dsex, data=m19, jrrIMax=1)
edsurveyTable(composite_linking ~ dsex, data=m19, jrrIMax=Inf)

edsurveyTable(composite ~ sdracem, data=m19, jrrIMax=1)
edsurveyTable(composite ~ sdracem, data=m19, jrrIMax=Inf)
edsurveyTable(composite_linking ~ sdracem, data=m19, jrrIMax=1)
edsurveyTable(composite_linking ~ sdracem, data=m19, jrrIMax=Inf)
}
