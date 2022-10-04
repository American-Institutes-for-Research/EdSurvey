skip_on_cran()
require(testthat)
context("NHES data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)


source("REF-11-NHES.R") # has REF output in it
if(!exists("edsurveyHome")) {
  if (Sys.info()[['sysname']] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

test_that("NHES data reads in correctly",{
  #converted to provide instructions for user.
  #user should have the 2012, 2016, and 2019 year datafiles downloaded
  #expect_silent(downloadNHES(root = edsurveyHome, years=c(2012, 2016, 2019), verbose=FALSE))
  #test both auto and specifying a survey code
  expect_silent(pfi2012 <<- readNHES(file.path(edsurveyHome, "NHES", "2012", "NHES_12_PFI_v1_0.sav"), surveyCode = "auto", verbose = FALSE))
  expect_silent(pfi2012x <<- readNHES(file.path(edsurveyHome, "NHES", "2012", "NHES_12_PFI_v1_0.sav"), surveyCode = "PFI_2012", verbose = FALSE))
  
  expect_is(pfi2012, "edsurvey.data.frame")
  expect_equal(dim(pfi2012), c(17563, 718))
  expect_identical(pfi2012, pfi2012x)
  pfi2012x <- NULL #no longer needed after compare
  
  #test reading multiple files
  inFiles <- list.files(file.path(edsurveyHome, "NHES", "2016"), "[.]sav$", full.names = TRUE, ignore.case = TRUE)
  expect_silent(nhes2016 <<- readNHES(savFiles = inFiles, surveyCode = "auto", verbose = FALSE))
  expect_is(nhes2016, "edsurvey.data.frame.list")
  
  #split into their own objects for ease of use later
  ate2016 <<- nhes2016$datalist[[which(grepl("^adult", nhes2016$covs$subject, ignore.case = TRUE))]]
  ecpp2016 <<- nhes2016$datalist[[which(grepl("^early childhood", nhes2016$covs$subject, ignore.case = TRUE))]]
  pfi2016 <<- nhes2016$datalist[[which(grepl("^parent", nhes2016$covs$subject, ignore.case = TRUE))]]
  nhes2016 <- NULL
  
  expect_equal(dim(ate2016), c(47744, 355))
  expect_equal(dim(ecpp2016), c(5844, 643))
  expect_equal(dim(pfi2016), c(14075, 823))
  
  expect_silent(ecpp2019 <<- readNHES(savFiles = file.path(edsurveyHome, "NHES", "2019", "nhes_19_ecpp_v1_0.sav"), surveyCode = "auto", verbose = FALSE)) 
})

context("NHES getData")
test_that("NHES getData",{
  dat1 <- getData(pfi2012, c("age2011", "rcvdate", "path"))
  expect_equal(dim(dat1), c(17563, 3))
  withr::with_options(list(digits=7), co <- capture.output(base::summary(dat1$path)))
  expect_equal(co, dat1Summary.Ref)
  withr::with_options(list(digits=7), co <- capture.output(base::mean(dat1$age2011)))
  expect_equal(co, "[1] 11.98719")
  
  #test NHES ECPP 2016 variables
  dat2 <- getData(ecpp2016, c("cpdays", "p1sex", "p1mrsta"), omittedLevels = FALSE)
  expect_equal(dim(dat2), c(5844, 3))
  withr::with_options(list(digits=7), co <- capture.output(base::summary(dat2)))
  expect_equal(co, dat2Summary.Ref)
  
  #test with some continuous variables that have omittedLevels
  dat3 <- getData(ate2016, c("eehrs", "cenreg"))
  expect_equal(dim(dat3), c(36567, 2))
  withr::with_options(list(digits=7), co <- capture.output(base::summary(dat3)))
  expect_equal(co, dat3Summary.Ref)
  
  dat4 <- getData(ecpp2019, c("p1rel", "p1sex", "cenreg"))
  expect_equal(dim(dat4), c(7092, 3))
  withr::with_options(list(digits=7), co <- capture.output(base::summary(dat4)))
  expect_equal(co, dat4Summary.Ref)
})

context("NHES lm.sdf")
test_that("NHES lm.sdf", {
  #test summary with difficult QC item
  withr::with_options(list(digits = 7), co <- capture.output(lm.sdf(formula = fhwkhrs ~ segrades , 
                                                                     data = subset(pfi2016, !pfi2016$path=="H HOMESCHOOLER"),
                                                                     varMethod = "jackknife" ,weightVar = "fpwt" )))
  expect_equal(co, nhes_lm1)
  
  withr::with_options(list(digits = 7), co <- capture.output(lm.sdf(formula = carehourx ~ p1educ , 
                                                                    data = ecpp2019,
                                                                    varMethod = "jackknife" ,weightVar = "fewt")))
  expect_equal(co, nhes_lm2)
})

context("NHES recode and gap")
test_that("NHES recode and gap", {
  ate2016rc <- recode.sdf(ate2016, list(wkstatus = list(from = "1 WORKING 35 HOURS OR MORE PER WEEK",
                                                        to = "35 or More Hrs Per Week"),
                                        wkstatus = list(from = c("2 WORKING LESS THAN 35 HOURS PER WEEK", "4 NOT IN THE LABOR FORCE", "3 LOOKING FOR WORK"),
                                                        to = "35 or Less Hrs Per Week")))
  
  withr::with_options(list(digits = 7), co <- capture.output(gap(variable = "xxage",
                                                                 data = ate2016rc,
                                                                 groupA = wkstatus=="35 or More Hrs Per Week",
                                                                 groupB = wkstatus=="35 or Less Hrs Per Week",
                                                                 weightVar = "fawt")
                                                              ))
  expect_equal(co, nhes_gap1)
})


