\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get the median of the composite
percentile(variable="composite", percentiles=50, data=sdf)

# get several percentiles
percentile(variable="composite", percentiles=c(0,1,25,50,75,99,100), data=sdf)
# build an edsurvey.data.frame.list
sdfA <- subset(sdf, scrpsu %in% c(5,45,56))
sdfB <- subset(sdf, scrpsu %in% c(75,76,78))
sdfC <- subset(sdf, scrpsu %in% 100:200)
sdfD <- subset(sdf, scrpsu %in% 201:300)

sdfl <- edsurvey.data.frame.list(datalist=list(sdfA, sdfB, sdfC, sdfD),
                                 labels=c("A locations",
                                           "B locations",
                                           "C locations",
                                           "D locations"))
# this shows how these datasets will be described:
sdfl$covs

percentile(variable="composite", percentiles=50, data=sdfl)
percentile(variable="composite", percentiles=c(25, 50, 75), data=sdfl)
}
