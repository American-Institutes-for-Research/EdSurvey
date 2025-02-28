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
