\dontrun{
# read in the example data (generated, not real student data)

sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# create a table that shows only the breakdown of dsex
edsurveyTable(formula=composite ~ dsex, data=sdf, returnMeans=FALSE, returnSepct=FALSE)

# create a table with composite scores by dsex
edsurveyTable(formula=composite ~ dsex, data=sdf)

# add a second variable
edsurveyTable(formula=composite ~ dsex + b017451, data=sdf)

# add a second variable, do not omit any levels
edsurveyTable(formula=composite ~ dsex + b017451 + b003501, data=sdf, omittedLevels=FALSE)

# add a second variable, do not omit any levels, change aggregation level
edsurveyTable(formula=composite ~ dsex + b017451 + b003501, data=sdf, omittedLevels=FALSE,
	            pctAggregationLevel=0)

edsurveyTable(formula=composite ~ dsex + b017451 + b003501, data=sdf, omittedLevels=FALSE,
	            pctAggregationLevel=1)

edsurveyTable(formula=composite ~ dsex + b017451 + b003501, data=sdf, omittedLevels=FALSE,
	            pctAggregationLevel=2)

# variance estimation using the Taylor series 
edsurveyTable(formula=composite ~ dsex + b017451 + b003501, data=sdf, varMethod="Taylor")
}
