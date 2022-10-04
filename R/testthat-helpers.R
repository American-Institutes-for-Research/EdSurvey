#this testthat-helpers.R file will be only for functions that are helpers only for testthat functions
#the benefits of having this in the /R folder are that they are loaded with devtools::load_all()
#see this for more details: https://blog.r-hub.io/2020/11/18/testthat-utility-belt/

#for certain tests that return an 'Iterations = ##' value, these
#numbers will have differing values between computer architectures and systems and are not to be compared
#e.g. 'Iterations = 11' will be returned for a Windows X64 call, but on MacOS M1 call it will a value of 'Iterations = 49'
dropIterations <- function(capturedText){
  iterationRegex <- "iterations.*[=].*\\d{1, }"
  idx <- which(grepl(iterationRegex, capturedText, ignore.case = TRUE), arr.ind = TRUE)
  
  return(capturedText[-idx])
}