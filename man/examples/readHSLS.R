\dontrun{
# use function default values at working directory
hsls <- readHSLS("~/HSLS/2009")

# specify parameters with verbose output
hsls <- readHSLS(path="~/HSLS/2009", 
                 filename = "hsls_16_student_v1_0.sav", 
                 forceReread = FALSE, 
                 verbose = TRUE)

# specify parameters silent output
hsls <- readHSLS(path="~/HSLS/2009", 
                 filename = "hsls_16_student_v1_0.sav", 
                 forceReread = FALSE, 
                 verbose = FALSE)

#for restricted-use student data, replicate weights stored in separate file
hslsRUD <- readHSLS(path="~/HSLS/2009", 
                    filename = "hsls_16_student_v1_0.sav", 
                    wgtFilename = "hsls_16_student_BRR_v1_0.sav",
                    forceReread = FALSE, 
                    verbose = TRUE)
}
