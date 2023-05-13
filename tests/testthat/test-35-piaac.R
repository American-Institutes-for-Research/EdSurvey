skip_on_cran()
require(testthat)
context("PIAAC data reads in correctly")
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)
source("REF-5-piaac.R") # has REF output in it

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

#able to toggle 'forceReread' for recaching the data if necessary
if(!exists("forceCacheUpdate")){
  forceCacheUpdate <- FALSE
}

test_that("PIAAC data reads in correctly", {
  expect_silent(downloadPIAAC(root=edsurveyHome, verbose = FALSE))
  usa <<- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("usa12_14"), verbose=FALSE, forceReread = forceCacheUpdate)
  usa17 <<- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("usa17"), verbose=FALSE, forceReread = forceCacheUpdate)
  nor <<- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("nor"), verbose=FALSE, forceReread = forceCacheUpdate)
  deu <<- readPIAAC(file.path(edsurveyHome, "PIAAC", "Cycle 1"), countries = c("deu"), verbose=FALSE, forceReread = forceCacheUpdate)
})

context("PIAAC $ assign")
test_that("PIAAC $ assign", {
  nor$skip <- ifelse(nor$c_q04d %in% "VALID SKIP", TRUE, FALSE)
  tab1 <- table(nor$skip, nor$c_q04d)
  gd <- getData(nor, c("skip", "c_q04d"), omittedLevels=FALSE)
  tab0 <- table(gd$skip, gd$c_q04d)
  expect_equal(tab0, tab1[,colnames(tab0)])
})


context("PIAAC data Wald test examples")
test_that("Wald test works correctly for PIAAC data", {
  
  myLogit1 <- logit.sdf(I(lit>270) ~ i_q04j + j_q02a, data = usa)
  wt <- EdSurvey::waldTest(model = myLogit1, coef = "i_q04j", H0 = 1)
  wt5 <- capture.output(wt)
  expect_equal(wt5, wt5REF)
  
  myLogit2 <- logit.sdf(I(lit>270) ~ i_q04j + j_q02a, data = nor)
  wt <- EdSurvey::waldTest(model = myLogit2, coef = "i_q04j", H0 = 1)
  wt6 <- capture.output(wt)
  expect_equal(wt6, wt6REF)
  
  myLogit3 <- logit.sdf(I(lit>270) ~ i_q04j + j_q02a, data = deu)
  wt <- EdSurvey::waldTest(model = myLogit3, coef = "i_q04j", H0 = 1)
  wt7 <- capture.output(wt)
  expect_equal(wt7, wt7REF)
})


