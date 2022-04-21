skip_on_cran()
require(testthat)
context("HSLS data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)


source("REF-8-HSLS.R") # has REF output in it
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

test_that("HSLS data reads in correctly",{
  expect_silent(downloadHSLS(years=2009, root=edsurveyHome, verbose=FALSE))
  expect_silent(hsls <<- readHSLS(file.path(edsurveyHome, "HSLS", "2009"), filename = "hsls_16_student_v1_0.sav", verbose = FALSE))
  
  expect_is(hsls, "edsurvey.data.frame")
  expect_equal(dim(hsls), c(23503, 8510))
  
  expect_silent(hslsSchl <<- readHSLS(file.path(edsurveyHome, "HSLS", "2009"), filename = "hsls_09_school_v1_0.sav", wgtFilename = NULL, verbose = FALSE))
  expect_is(hslsSchl, "edsurvey.data.frame")
  expect_equal(dim(hslsSchl), c(944, 688))
})

context("HSLS getData")
test_that("HSLS getData",{
  dat1 <- getData(hsls, c("stu_id", "x1sex", "x1race"))
  expect_equal(dim(dat1), c(22496, 3))
  withr::with_options(list(digits=7), co <- capture.output(summary(dat1)))
  expect_equal(co, dat1Summary.Ref)
  
  #test HSLS School variables
  dat2 <- getData(hslsSchl, c("sch_id", "x1region", "x1locale"), omittedLevels = FALSE)
  expect_equal(dim(dat2), c(944, 3))
  withr::with_options(list(digits=7), co <- capture.output(summary(dat2)))
  expect_equal(co, dat2Summary.Ref)
  
  #test with some continuous variables that have omittedLevels
  dat3 <- getData(hsls, c("stu_id", "x1ses", "x2ses"))
  expect_equal(dim(dat3), c(18948, 3))
  withr::with_options(list(digits=7), co <- capture.output(summary(dat3)))
  expect_equal(co, dat3Summary.Ref)
  
  #ensure that values having nearly all omitted levels and '0=Zero' have the '0=Zero' label dropped and return numeric accurately
  dat4 <- hsls$x3tcredeng
  expect_equal(class(dat4), "numeric")
})

context("HSLS rename.sdf")
test_that("HSLS rename.sdf", {
  hslsRename <- rename.sdf(hsls, oldnames = "x1txmth",
                          newnames = "mathThetaScore")
  expect_equal("mathThetaScore" %in% colnames(hslsRename),TRUE)
})

context("HSLS weights")
test_that("HSLS weights", {
  hslsWgtNames <- names(hsls$weights)
  expect_equal(all(hslsWgtNames %in% wgtNames.Ref),TRUE)
})


