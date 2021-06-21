\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# the results can be confusing because of omitted.levels and
# default_conditions
nrow(sdf)

# the default conditions and, depending on the columns selected, also the 
# omitted levels can change the number of rows used in any given analysis 
mydat1 <- getData(sdf, varnames=c("dsex", "mrpcm1"))
nrow(mydat1)

# here all rows are returned, though some are not in the reproting sample
# and have results that are typically not reported.
mydat2 <- getData(sdf, varnames=c("dsex", "mrpcm1"), omittedLevels=FALSE,
                  defaultConditions=FALSE)
nrow(mydat2)
}
