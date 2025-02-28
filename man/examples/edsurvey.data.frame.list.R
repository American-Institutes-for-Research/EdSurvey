\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# NOTE: the following code would not normally have to be run but is used here
# to generate demo data.
# Specifically, make subsets of sdf by the scrpsu variable,
# "Scrambled PSU and school code"
sdfA <- subset(sdf, scrpsu %in% c(5,45,56))
sdfB <- subset(sdf, scrpsu %in% c(75,76,78))
sdfC <- subset(sdf, scrpsu %in% 100:200)
sdfD <- subset(sdf, scrpsu %in% 201:300)

# construct an edsurvey.data.frame.list from these four data sets
sdfl <- edsurvey.data.frame.list(datalist=list(sdfA, sdfB, sdfC, sdfD),
                                 labels=c("A locations",
                                          "B locations",
                                          "C locations",
                                          "D locations"))

# alternative method of building
sdfl2 <- sdfA + sdfB + sdfC

# check contents
sdfA %in% sdfl
# note %in% checks by survey (NAEP 2005 Math for sdf,
# sdfA, sdfB, sdfC, and sdfD) not by subset, so this also return TRUE
sdfD %in% sdfl2

# this shows how these datasets will be described
sdfl$covs 
# get the gaps between Male and Female for each data set
gap1 <- gap(variable="composite", data=sdfl, dsex=="Male", dsex=="Female")
gap1

# make combine sdfA and sdfB
sdfl1a <- edsurvey.data.frame.list(datalist=list(sdfA, sdfB),
                                   labels=c("A locations",
                                            "B locations"))

# combine sdfC and sdfD
sdfl1b <- edsurvey.data.frame.list(datalist=list(sdfC, sdfD),
                                   labels=c("C locations",
                                            "D locations"))

# append to make sdf3 the same as sdfl
sdfl3 <- append.edsurvey.data.frame.list(sdfA=sdfl1a, sdfB=sdfl1b)
identical(sdfl, sdfl3) #TRUE

# append to make sdf4 the same as sdfl
sdfl4 <- append.edsurvey.data.frame.list(
  append.edsurvey.data.frame.list(sdfA=sdfl1a, sdfB=sdfC, labelsB = "C locations"),
  sdfD,
  labelsB = "D locations")
identical(sdfl, sdfl4) #TRUE

# show label deconflicting
t11 <- readTIMSS(path="~/TIMSS/2011", countries = c("fin", "usa"), gradeLvl = 4)
t15 <- readTIMSS(path="~/TIMSS/2015", countries = c("fin", "usa"), gradeLvl = 4)
# these would not be unique
t11$covs
t15$covs
# resulting values includes year now
t11_15 <- append.edsurvey.data.frame.list(sdfA=t11, sdfB=t15)
t11_15$covs

}
