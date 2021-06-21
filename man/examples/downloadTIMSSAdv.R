\dontrun{
# root argument will vary by operating system conventions
downloadTIMSSAdv(year=c(2008, 2015), root = "~/")

# cache=TRUE will download then process the datafiles
downloadTIMSSAdv(year=2015, root = "~/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadTIMSSAdv(root="~/", verbose = FALSE)
}
