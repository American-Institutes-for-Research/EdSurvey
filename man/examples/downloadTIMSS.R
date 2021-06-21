\dontrun{
# root argument will vary by operating system conventions
downloadTIMSS(year=c(2019, 2015, 2011), root = "~/")

# cache=TRUE will download then process the datafiles
downloadTIMSS(year=2015, root = "~/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadTIMSS(root="~/", verbose = FALSE)
}
