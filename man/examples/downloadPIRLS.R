\dontrun{
# root argument will vary by operating system conventions
downloadPIRLS(year=c(2006, 2011), root = "~/")

# cache=TRUE will download then process the datafiles
downloadPIRLS(year=2011, root = "~/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadPIRLS(root="~/", verbose = FALSE)
}
