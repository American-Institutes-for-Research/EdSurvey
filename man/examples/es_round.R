\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# by default uses jackknife variance method using replicate weights
es1 <- edsurveyTable(formula=composite ~ dsex + b017451, data=sdf)
# turn on rounding by default
options(EdSurvey_round_output= TRUE)
es1
# turn off rounding by default
options(EdSurvey_round_output= FALSE)

# request rounding for this outpt
print(es1, use_es_round=TRUE)
# round, then print

# round the PCT column to one digit
es_round(es1, round_specific_element=list(PCT=roundn(1)))

}