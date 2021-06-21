skip_on_cran()
require(testthat)
context("PISA data reads in correctly")
require(EdSurvey)
require(WeMix)
options(width = 500)
options(useFancyQuotes=FALSE)
source("REF-4-pisa.R") # has REF output in it

if(!exists("edsurveyHome")) {
  # if (Sys.info()[['sysname']] == "Windows") {
  #   edsurveyHome <- "C:/EdSurveyData/"
  # } else {
    edsurveyHome <- "~/EdSurveyData/"
  # }
}

if (!dir.exists(edsurveyHome)) {
  dir.create(edsurveyHome)
}

# Test =================
 
# 1. Check data read-in
test_that("PISA data reads in correctly", {
  expect_silent(downloadPISA(root=edsurveyHome, year=c(2000, 2003, 2006, 2009, 2012, 2015, 2018), cache=FALSE, verbose = FALSE))
  expect_silent(downloadPISA(root=edsurveyHome, year=2012, database="CBA", cache=FALSE, verbose = FALSE))
  expect_silent(usaINT2018 <<- readPISA(paste0(edsurveyHome, "PISA/2018"), countries = "usa", verbose = FALSE))
  expect_silent(usaINT2015 <<- readPISA(paste0(edsurveyHome, "PISA/2015"), countries = "usa", verbose = FALSE))
  expect_silent(usaINT2012 <<- readPISA(paste0(edsurveyHome, "PISA/2012"), countries = "usa", verbose = FALSE))
  expect_silent(qcnCBA2012 <<- readPISA(paste0(edsurveyHome, "PISA/2012"), database = "CBA", countries = "qcn", verbose = FALSE)) # Shanghai
  expect_silent(jpn2009 <<- readPISA(paste0(edsurveyHome, "PISA/2009"), countries = "jpn", verbose = FALSE))
  expect_silent(aus2006 <<- readPISA(paste0(edsurveyHome, "PISA/2006"), countries = "aus", verbose = FALSE))
  expect_silent(aus2003 <<- readPISA(paste0(edsurveyHome, "PISA/2003"), countries = "aus", verbose = FALSE))
  expect_warning(usa2000 <<- readPISA(paste0(edsurveyHome, "PISA/2000"), countries = "usa", verbose = FALSE),
                  "Cannot find both PSU and Stratum variables on data.")
  # 2000 complains about the PSU variable not being present

  expect_is(usaINT2015, "edsurvey.data.frame")
  expect_is(usaINT2012, "edsurvey.data.frame")
  expect_is(qcnCBA2012, "edsurvey.data.frame")
  expect_is(jpn2009, "edsurvey.data.frame")
  expect_is(aus2006, "edsurvey.data.frame")
  expect_is(usa2000, "edsurvey.data.frame")
  expect_equal(dim(usaINT2015), c(5712, 3714))
  expect_equal(dim(usaINT2012), c(4978, 1262))
  expect_equal(dim(qcnCBA2012), c(5177, 1345))
  expect_equal(dim(jpn2009), c(6088, 981))
  expect_equal(dim(aus2006), c(14170, 1021))
})

# Check multiple-path read-in
test_that("PISA multiple path read-in", {
  multiESDFL <- readPISA(paste0(edsurveyHome, c("PISA/2009", "PISA/2012", "PISA/2015")), countries="usa", verbose=FALSE)
  expect_is(multiESDFL, "edsurvey.data.frame.list")
  expect_equal(colnames(multiESDFL$covs), c("subject", "year", "country"))
  expect_equal(length(multiESDFL$datalist), 3)
})

context("PISA showPlausibleValues and showWeights verbose output agrees")
test_that("PISA showPlausibleValues and showWeights verbose output agrees", {
  co <- capture.output(showPlausibleValues(usaINT2012, verbose = TRUE))
  expect_equal(co,pvREF)
  
  co <- capture.output(showWeights(usaINT2012, verbose = TRUE))
  expect_equal(co, swREF)
})

context("PISA getData")
test_that("PISA getData", {
  expect_known_value(gd0 <- getData(usaINT2015,c("st004d01t", "st001d01t")), file = "PISAgd0.rds", update = FALSE)
  expect_known_value(gd1 <- getData(usaINT2012,c("st04q01","st20q01")), file = "PISAgd1.rds", update = FALSE)
  expect_known_value(gd2 <- getData(qcnCBA2012,c("st04q01","st20q01")), file = "PISAgd2.rds", update = FALSE)
  expect_known_value(gd3 <- getData(jpn2009,c("s514q03","bookid")), file = "PISAgd3.rds", update = FALSE)
  expect_known_value(gd4 <- getData(aus2006,c("isi")), file = "PISAgd4.rds", update = FALSE)
})

context("PISA subset data")
test_that("PISA subset data", {
  usa12_female <- subset(usaINT2012, st04q01 == "Female", verbose = FALSE)
  expect_equal(dim(usa12_female),c(2453,1262))
})

context("PISA showCutPoints")
test_that("PISA showCutPoints", {
  co <- capture.output(showCutPoints(qcnCBA2012))
  expect_equal(co,scREF)
})

# 2. Check analytical functions
context("PISA edsurveyTable")
test_that("PISA edsurveyTable", {
  edTable1 <- edsurveyTable(math ~ st04q01 + st20q01, data = usaINT2012)
  withr::with_options(list(digits=7),
                      edTable1c <- capture.output(edTable1)
                     )
  expect_equal(edTable1c, pisaedTable1REF)
})


context("PISA lm.sdf")
test_that("PISA lm.sdf", {
  plm1 <- lm.sdf(macq ~ st29q06 + sc01q01, data = usaINT2012)
  withr::with_options(list(digits=7),
                      plm1c <- capture.output(plm1)
                      )
  expect_equal(plm1c,plm1REF)
})

context("PISA gap")
test_that("PISA gap", {
  pgap1 <- gap(variable = "math", data = usaINT2012,
      groupA = st04q01 == "Male" , groupB = st04q01 == "Female", weightVar = 'w_fstuwt')
  withr::with_options(list(digits=7),
                      pgap1c <- capture.output(pgap1)
                      )
  expect_equal(pgap1c,pgap1REF)
  
  pgap2 <- gap(variable = "math", data = usaINT2012,
               groupA = st04q01 == "Male" , groupB = st04q01 == "Female", weightVar = 'w_fstuwt', percentiles = c(50,90), pctMethod = 'symmetric')
  withr::with_options(list(digits=7), pgap2c <- capture.output(pgap2))
  expect_equal(pgap2c,pgap2REF)
})

context("PISA achievementLevels")
test_that("PISA achievementLevels", {
  al1 <- achievementLevels(achievementVars = "cpro", aggregateBy = "st04q01", data = qcnCBA2012)
  withr::with_options(list(digits=7), al1c <- capture.output(al1))
  expect_equal(al1c,al1REF)
})

context("PISA glm")
test_that("PISA glm", {
  logit1 <- logit.sdf(I(st04q01=="Male") ~ st20q01 + st48q01 + st87q01, data=qcnCBA2012)
  coefREF <- c("(Intercept)"         =  0.898510256928794,
            "st20q01Other country"= -0.637039679070128,
            "st48q01Courses after school Test Language" =  0.0570384010137383,
            "st87q01Agree"        = -1.02420699129252,
            "st87q01Disagree"     = -0.984759761693152,
            "st87q01Strongly disagree" = -0.896205635267306)
  expect_equal(coef(logit1), coefREF)
  se <- c("(Intercept)"         = 0.383392563610423,
          "st20q01Other country"= 0.481575743890542,
          "st48q01Courses after school Test Language" = 0.1112482333064,
          "st87q01Agree"        = 0.418721001158844,
          "st87q01Disagree"     = 0.38157294795411,
          "st87q01Strongly disagree" = 0.400877207648506)
  expect_equal(logit1$se, se)
})

context("PISA cor")
test_that("PISA cor", {
  cor1 <- cor.sdf("read", "st21q02", usa2000, method = "Pearson", weightVar = "w_fstuwt_read", jrrIMax = Inf, omittedLevels = c("Mis"))
  withr::with_options(list(digits=7), cor1c <- capture.output(cor1))
  cor1REF <- c("Method: Pearson",
               "full data n: 3846",
               "n used: 3634",
               "", 
               "Correlation: -0.1345117",
               "Standard Error: 0.01830193",
               "Confidence Interval: [-0.170717, -0.09794362]", 
               "",
               "Correlation Levels:",
               "  Levels for Variable 'st21q02' (Lowest level first):", 
               "    1. Yes",
               "    2. No")
  expect_equal(cor1c,cor1REF)
})
