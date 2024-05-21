\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# find the mean score gap in the primer data between males and females
gap(variable="composite", data=sdf, groupA=dsex=="Male", groupB=dsex=="Female")

# find the score gap of the quartiles in the primer data between males and females
gap(variable="composite", data=sdf,
    groupA=dsex=="Male", groupB=dsex=="Female", percentile=50)
gap(variable="composite", data=sdf,
    groupA=dsex=="Male", groupB=dsex=="Female", percentile=c(25, 50, 75))

# find the percent proficient (or higher) gap in the primer data between males and females
gap(variable="composite", data=sdf, groupA=dsex=="Male", groupB=dsex=="Female", 
    achievementLevel=c("Basic", "Proficient", "Advanced"))

# find the discrete achievement level gap--this is harder to interpret
gap(variable="composite", data=sdf, groupA=dsex=="Male", groupB=dsex=="Female",
    achievementLevel="Proficient", achievementDiscrete=TRUE)

# find the percent talk about studies at home (b017451) never or hardly
# ever gap in the primer data between males and females
gap(variable="b017451", data=sdf, groupA=dsex=="Male", groupB=dsex=="Female", 
    targetLevel="Never or hardly ever")

# example showing how to compare multiple levels
gap(variable="b017451",
    data=sdf,
    groupA=dsex=="Male",
    groupB=dsex=="Female",
    targetLevel="Infrequently",
    recode=list(b017451=list(from=c("Never or hardly ever",
                                    "Once every few weeks",
                                    "About once a week"),
                             to=c("Infrequently"))))

# make subsets of sdf by scrpsu, "Scrambled PSU and school code"
sdfA <- subset(sdf, scrpsu %in% c(5,45,56))
sdfB <- subset(sdf, scrpsu %in% c(75,76,78))
sdfC <- subset(sdf, scrpsu %in% 100:200)
sdfD <- subset(sdf, scrpsu %in% 201:300)

sdfl <- edsurvey.data.frame.list(datalist=list(sdfA, sdfB, sdfC, sdfD),
                                 labels=c("A locations", "B locations",
                                          "C locations", "D locations"))

gap(variable="composite", data=sdfl, groupA=dsex=="Male", groupB=dsex=="Female", percentile=c(50))
}

\dontrun{
# example showing using linking error with gap
# load Grade 4 math data
# requires NAEP RUD license with these files in the folder the user is currectly in
g4math2015 <- readNAEP("M46NT1AT.dat")
g4math2017 <- readNAEP("M48NT1AT.dat")
g4math2019 <- readNAEP("M50NT1AT.dat")

# make an edsurvey.data.frame.list from math grade 4 2015, 2017, and 2019 data
g4math <- edsurvey.data.frame.list(datalist=list(g4math2019, g4math2017, g4math2015),
                                   labels = c("2019", "2017", "2015"))

# gap analysis with linking error in variance estimation across surveys
gap(variable="composite", data=g4math,
    groupA=dsex=="Male", groupB=dsex=="Female", includeLinkingError=TRUE)
gap(variable="composite", data=g4math,
    groupA=dsex=="Male", groupB=dsex=="Female", percentiles = c(10, 25), 
    includeLinkingError=TRUE)
gap(variable="composite", data=g4math, groupA=dsex=="Male", groupB=dsex=="Female", 
    achievementDiscrete = TRUE, achievementLevel=c("Basic", "Proficient", "Advanced"), 
    includeLinkingError=TRUE)
}
