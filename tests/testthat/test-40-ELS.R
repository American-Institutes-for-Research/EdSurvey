skip_on_cran()
require(testthat)
context("ELS data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)


source("REF-7-ELS.R") # has REF output in it
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

test_that("ELS data reads in correctly",{
  expect_silent(downloadELS(years=2002, root=edsurveyHome, verbose=FALSE))
  expect_silent(els <<- readELS(file.path(edsurveyHome, "ELS", "2002"), filename = "els_02_12_byf3pststu_v1_0.sav", wgtFilename = "els_02_12_byf3stubrr_v1_0.sav", verbose = FALSE))
  
  expect_is(els, "edsurvey.data.frame")
  expect_equal(dim(els), c(16197, 9013))
  
  expect_silent(elsSchl <<- readELS(file.path(edsurveyHome, "ELS", "2002"), filename = "els_02_12_byf1sch_v1_0.sav", wgtFilename = NULL, verbose = FALSE))
  expect_is(elsSchl, "edsurvey.data.frame")
  expect_equal(dim(elsSchl), c(1954, 926)) #21409 obs::18928 cols
})

context("ELS getData")
test_that("ELS getData",{
  withr::with_options(list(digits = 4), dat1 <- getData(els, c("stu_id", "bysex", "byrace"))) #digits=4 impacts rounding in summary call
  expect_equal(dim(dat1), c(15244, 3))
  withr::with_options(list(digits = 4), co <- capture.output(summary(dat1)))
  # ignore median  
  expect_equal(co[-4], dat1Summary.Ref[-4])
  
  #test ELS School variables
  withr::with_options(list(digits = 4), dat2 <- getData(elsSchl, c("sch_id", "byregion", "byurban"), omittedLevels = FALSE)) #digits=4 impacts rounding in summary call
  expect_equal(dim(dat2), c(1954, 3))
  withr::with_options(list(digits = 4), co <- capture.output(summary(dat2)))
  expect_equal(co[-4], dat2Summary.Ref[-4])
  
  #test with some continuous variables that have omittedLevels
  withr::with_options(list(digits = 7), dat3 <- getData(els, c("stu_id", "f1ses1", "f1ses1qu", "f1ses2", "f1ses2qu"))) #digits=4 impacts rounding in summary call
  expect_equal(dim(dat3), c(16160, 5))
  withr::with_options(list(digits = 7), co <- capture.output(summary(dat3)))
  expect_equal(co[-4], dat3Summary.Ref[-4])
})

context("ELS rename.sdf")
test_that("ELS rename.sdf", {
  elsRename <- rename.sdf(els, oldnames = "byfcomp",
                           newnames = "familyComposition")
  expect_equal("familyComposition" %in% colnames(elsRename),TRUE)
})

context("ELS weights")
test_that("ELS weights", {
  elsWgtNames <- names(els$weights)
  expect_equal(all(elsWgtNames %in% wgtNames.Ref),TRUE)
})


