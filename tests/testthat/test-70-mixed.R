skip_on_cran()
require(testthat)
context('mixed.sdf')
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)

source("REF-mixed.R")

if(!exists("edsurveyHome")) {
  if (Sys.info()[['sysname']] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

test_that('mixed.sdf', {
  usa8 <- readTIMSS(paste0(edsurveyHome, "TIMSS/2003"), countries = c("usa"), gradeLvl = 8, verbose=FALSE)
  usa8dat <- getData(data=usa8, c("mmat", "idclass", "totwgt", "schwgt","itsex"), returnJKreplicates=FALSE, addAttributes=TRUE)
  usa8dat$wsum <- ave(usa8dat$totwgt, usa8dat$idclass, FUN=mean)
  usa8dat$w1 <- usa8dat$totwgt / usa8dat$wsum * usa8dat$schwgt
  usa8dat$w2 <- usa8dat$schwgt
  system.time(m1 <- mixed.sdf(mmat ~ itsex + (1|idclass), data=usa8dat, weightVars=c("w1", "w2"), weightTransformation=FALSE, verbose=FALSE))
  withr::with_options(list(digits=4),
                      co <- capture.output(summary(m1)))
  expect_equal(co, m1SummaryREF)
})

context("Interactions in REs")
test_that("Interactions in REs", {
  usa8 <- readTIMSS(paste0(edsurveyHome, "TIMSS/2003"), countries = c("usa"), gradeLvl = 8, verbose=FALSE)
  usa8dat <- getData(data=usa8, c("mmat", "idclass", "totwgt", "schwgt","itsex", "bsbgfbrn"), returnJKreplicates=FALSE, addAttributes=TRUE)
  usa8dat$wsum <- ave(usa8dat$totwgt, usa8dat$idclass, FUN=mean)
  usa8dat$w1 <- usa8dat$totwgt / usa8dat$wsum * usa8dat$schwgt
  usa8dat$w2 <- usa8dat$schwgt
  system.time(m2 <- mixed.sdf(mmat ~ itsex + bsbgfbrn + (1+bsbgfbrn|idclass), data=usa8dat, weightVars=c("w1", "w2"), weightTransformation=FALSE, verbose=FALSE))
  withr::with_options(list(digits=4),
                      co <- capture.output(summary(m2)))
  expect_equal(co, m2SummaryRef)
})

context('mixed.sdf Wald test')
test_that('mixed.sdf Wald test', {
  cntl <- readPISA(paste0(edsurveyHome, "PISA/2012"), countries="USA", verbose=FALSE)
  # model with one random effect
  m1 <- mixed.sdf(math ~ st29q03 + sc14q02 +st04q01+escs+ (1|schoolid),
                  data=cntl)
  wt <- waldTest(m1, coef=2:4)
  expect_is(wt, "edsurveyWaldTest")
  # the estimates vary from computer to computer
  expect_equal(unname(wt$result$chi2["chi2"]), 30.2097969661517, tol=0.001)
})

