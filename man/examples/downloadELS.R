\dontrun{
# root argument will vary by operating system conventions
downloadELS(years=2002, root = "C:/")

# cache=TRUE will download then process the datafiles
downloadELS(years=2002, root = "C:/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadELS(root="C:/", verbose = FALSE)
}
