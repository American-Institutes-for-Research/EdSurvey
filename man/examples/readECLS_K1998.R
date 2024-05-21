\dontrun{
# read-in student file with defaults
eclsk_df <- readECLS_K1998(path="~/ECLS_K/1998") #using defaults
d <- getData(data=eclsk_df, varnames=c("childid", "gender", "race"))
summary(d)
}

\dontrun{
# read-in with parameters specified
eclsk_df <- readECLS_K1998(path = "~/ECLS_K/1998", 
                           filename = "eclsk_98_99_k8_child_v1_0.dat", 
                           layoutFilename = "Layout_k8_child.txt", 
                           verbose = TRUE, 
                           forceReread = FALSE)
}
