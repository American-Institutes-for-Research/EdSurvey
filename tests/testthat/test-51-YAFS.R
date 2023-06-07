skip_on_cran()
require(testthat)
context("YAFS")
options(width = 500)
options(useFancyQuotes = FALSE)
source("REF-9-YAFS.R") # has REF output in it

if (!exists("edsurveyHome")) {
  if (Sys.info()[["sysname"]] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# able to toggle 'forceReread' for recaching the data if necessary
if (!exists("forceCacheUpdate")) {
  forceCacheUpdate <- FALSE
}

test_that("PISA YAFS data reads in correctly", {
  # read in PISA USA 2012
  expect_silent(pisa <<- readPISA(path = file.path(edsurveyHome, "PISA", "2012"), database = "INT", countries = "usa", verbose = FALSE, forceReread = forceCacheUpdate))
  # read PISA YAFS natively
  expect_silent(pisa_yafs <<- readPISA_YAFS(
    datPath = file.path(edsurveyHome, "PISA YAFS", "2016", "PISA_YAFS2016_Data.dat"),
    spsPath = file.path(edsurveyHome, "PISA YAFS", "2016", "PISA_YAFS2016_SPSS.sps"),
    esdf_PISA2012_USA = pisa
  ))
  expect_is(pisa_yafs, "edsurvey.data.frame")
  expect_equal(dim(pisa_yafs), c(4978, 1965))
  expect_equal(pisa_yafs$country, "USA")
  expect_equal(pisa_yafs$year, "2016")
})


context("PISA YAFS edsurveyTable")
test_that("PISA YAFS edsurveyTable", {
  lit <- edsurveyTable(
    formula = lit ~ 1,
    data = pisa_yafs,
    omittedLevels = TRUE,
    jrrIMax = Inf
  )
  withr::with_options(list(digits = 7), out <- capture.output(lit))
  expect_equal(out, lit.ref)
})

context("PISA YAFS achivementLevel one PV")
test_that("PISA YAFS edsurveyTable one PV", {
  achievementLevels_Lit <- achievementLevels(
    achievementVars = c("lit"),
    aggregateBy = NULL, data = pisa_yafs,
    weightVar = "w_yfstuwt", jrrIMax = Inf
  )

  withr::with_options(list(digits = 7), out <- capture.output(achievementLevels_Lit))
  expect_equal(out, achievement.ref)
})

context("PISA YAFS achivementLevel two PVs")
test_that("PISA YAFS edsurveyTable two PVs", {
  achievementLevels_tableA10 <- achievementLevels(
    achievementVars = c("num", "math"), aggregateBy = "math",
    data = pisa_yafs, returnVarEstInputs = TRUE,
    returnCumulative = FALSE, jrrIMax = Inf,
    cutpoints = list(
      c(
        "Customized ESO 2 and 3" = 226.00,
        "Customized ESO 4 and above" = 326.00
      ),
      c(
        "Customized PISA 2 and 4" = 420.07,
        "Customized PISA 5 and above" = 544.68
      )
    )
  )

  withr::with_options(list(digits = 7), out <- capture.output(achievementLevels_tableA10))
  expect_equal(out, achievement2.ref)
})

context("PISA YAFS cor.sdf two PVs")
test_that("PISA YAFS cor.sdf two PVs", {
  cor_lit.read <- cor.sdf(x = "lit", y = "read", data = pisa_yafs)

  withr::with_options(list(digits = 7), out <- capture.output(cor_lit.read))
  expect_equal(out, cor.ref)
})

context("PISA YAFS logit")
test_that("PISA YAFS logits", {
  logit_b_q26a.bq_q2 <- logit.sdf(
    formula = I(b_q26a %in% "No") ~ bq_q2,
    data = pisa_yafs,
    jrrIMax = Inf
  )

  withr::with_options(list(digits = 7), out <- capture.output(logit_b_q26a.bq_q2))
  expect_equal(out, logit.ref)
})
