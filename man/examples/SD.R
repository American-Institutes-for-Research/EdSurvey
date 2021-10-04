\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get standard deviation for Male's composite score
SD(data = subset(sdf, dsex == "Male"), variable = "composite")

# get several standard deviations

# build an edsurvey.data.frame.list
sdfA <- subset(sdf, scrpsu %in% c(5,45,56))
sdfB <- subset(sdf, scrpsu %in% c(75,76,78))
sdfC <- subset(sdf, scrpsu %in% 100:200)
sdfD <- subset(sdf, scrpsu %in% 201:300)

sdfl <- edsurvey.data.frame.list(list(sdfA, sdfB, sdfC, sdfD),
                                 labels=c("A locations",
                                          "B locations",
                                          "C locations",
                                          "D locations"))

# this shows how these datasets will be described:
sdfl$covs

# SD results for each survey
SD(data = sdfl, variable = "composite")
# SD results more compactly and with comparisons
gap(variable="composite", data=sdfl, stDev=TRUE, returnSimpleDoF=TRUE)
}
