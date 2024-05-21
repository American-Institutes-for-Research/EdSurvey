\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# search both the student and school files, returning levels for variable values
showCodebook(data=sdf, fileFormat=c("student","school"), labelLevels = TRUE, includeRecodes = FALSE)

# return codebook information for the student file, excluding variable value levels,
# including recoded variables
sdf <- recode.sdf(sdf, recode = list(dsex = list(from = c("Male"), to = c("MALE"))))
showCodebook(data=sdf, fileFormat=c("student"), labelLevels = FALSE, includeRecodes = TRUE)

# return codebook information for the student file, including variable value levels
# and recoded variables
showCodebook(data=sdf, fileFormat=c("student","school"), labelLevels = FALSE, includeRecodes = TRUE)

# return codebook information for all codebooks in an edsurvey.data.frame; commonly use View()
View(showCodebook(data=sdf))
}
