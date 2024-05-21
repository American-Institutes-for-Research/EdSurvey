\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# run a base R function on a column of edsurvey.data.frame
table(sdf$dsex)
# assignment
table(sdf$b013801)
sdf$books <- ifelse(sdf$b013801 %in% c("0-10", "11-25"), "0-25 books", "26+ books")
table(sdf$books, sdf$b013801)

# extract default omitted levels of NAEP primer data
getAttributes(data=sdf, attribute="omittedLevels")
#[1] "Multiple" NA         "Omitted"

# update default omitted levels of NAEP primer data
sdf <- setAttributes(data=sdf,
	                 attribute="omittedLevels",
	                 value=c("Multiple", "Omitted", NA, "(Missing)"))
getAttributes(data=sdf, attribute="omittedLevels")
#[1] "Multiple"  "Omitted"   NA          "(Missing)"
}
