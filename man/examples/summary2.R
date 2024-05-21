\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# print out summary of weighted statistics of a continuous variable
summary2(data=sdf, variable="composite")
# print out summary of weighted statistics of a variable, including omitted levels
summary2(data=sdf, variable="b017451", omittedLevels = FALSE)
# make a crosstab
summary2(data=sdf, variable=c("b017451", "dsex"), omittedLevels = FALSE)

# print out summary of unweighted statistics of a variable
summary2(data=sdf, variable="composite", weightVar = NULL)
}
