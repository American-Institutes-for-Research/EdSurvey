\dontrun{
# read-in student file with defaults
eclsk_df <- readECLS_K2011(path="~/ECLS_K/2011") #using defaults
d <- getData(eclsk_df, c("childid", "c1hgt1", "c1wgt1"))
summary(d)
}

\dontrun{
# read-in with parameters specified
eclsk_df <- readECLS_K2011(path = "~/ECLS_K/2011",
                           filename = "childK5p.dat",
                           layoutFilename = "ECLSK2011_K5PUF.sps",
                           forceReread = FALSE,
                           verbose = TRUE) 
}
