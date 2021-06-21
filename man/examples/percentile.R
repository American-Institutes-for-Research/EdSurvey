\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# get the median of the composite
percentile("composite", 50, sdf)

# get several percentiles
percentile("composite", c(0,1,25,50,75,99,100), sdf)
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

percentile("composite", 50, sdfl)
percentile("composite", c(25, 50, 75), sdfl)
}
