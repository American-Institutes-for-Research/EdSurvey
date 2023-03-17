skip_on_cran()
require(testthat)

sppFld <- "I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/HSTS/HSTS 2019/HSTS 2019_Rev 4/"
onSPP <- file.exists(file.path(sppFld, "SPSS_programs/student.sps"))
skip_if_not(onSPP, message = "Not on SPP, skipping HSTS tests")

context("HSTS data reads in correctly")
require(EdSurvey)
require(dplyr)
options(width = 500)
options(useFancyQuotes=FALSE)



test_that("HSTS 2019 data reads in correctly",{
  #must be on the SPP for analysis
  expect_warning(hsts2019 <<- readHSTS(dataFilePath = sppFld, year = 2019, verbose = FALSE),
                 "PSU or Stratum variable.*Taylor series")
  
  expect_equal(dim(hsts2019), c(47274, 1045))

  #test data cannot be merged with transcript data, expect error
  expect_error(getData(hsts2019, c("t_code", "carncred"), omittedLevels = FALSE),
               "merge conflict")
  
  
  
  cb <- showCodebook(hsts2019)
  
  #pull student data
  stuVar <- cb$variableName[cb$fileFormat=="Student"][1:25]
  d1 <- getData(hsts2019, stuVar, omittedLevels = FALSE)
  expect_equal(dim(d1), c(47274, 76)) #includes finstuwt so includes all the replicates
  
  #sum of Carnegie Credits Earned - All STEM Courses (stfullc)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(hsts2019$stfullc), c(204036.32))
  })
  
  #pull student + test data
  testVar <- cb$variableName[cb$fileFormat=="Test"]
  d2 <- getData(hsts2019, c(stuVar, testVar), omittedLevels = FALSE)
  expect_equal(dim(d2), c(78971, 82))
  
  #subset to only students having a test record (dropping rows with NA test data)
  d2 <- subset(d2, !is.na(d2$t_seq))
  expect_equal(dim(d2), c(37142, 82))
  
  #sum of all test scores (t_score)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d2$t_score), c(7447880))
  })
  
  #pull student + transcript data
  trnsVar <- cb$variableName[cb$fileFormat=="Transcript"]
  d3 <- getData(hsts2019, c(stuVar, trnsVar), omittedLevels = FALSE)
  expect_equal(dim(d3), c(2327191, 88))
  
  #sum of all transcript carnagie credits (carncred)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d3$carncred), c(1332713.9028))
  })
  d3 <- NULL #clear memory
  
  #pull student + transcript + catalog data (catalog data will always be at the Transcript data level)
  ctlgVar <- cb$variableName[cb$fileFormat=="Transcript_Catalog"]
  d4 <- getData(hsts2019, c(stuVar, trnsVar, "crsename", "carncred", "tookonln", "transfer"), omittedLevels = FALSE)
  expect_equal(dim(d4), c(2327191, 88))
  
  #sum of all transcript carnagie credits (carncred)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d4$carncred[d4$gradlev=="Tenth grade"]), c(331679.1278))
  })
  d4 <- NULL
  
  #pull student + school
  schlVar <- cb$variableName[cb$fileFormat=="School"]
  d5 <- getData(hsts2019, c(stuVar, schlVar), omittedLevels = FALSE)
  expect_equal(dim(d5), c(47274, 370))
  
  #sum of all school English Credits for Graduation (mnengg)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d5$mnengg[!is.na(d5$mnengg)]), c(165959.5000))
  })
  
  #pull student + NAEP Math + NAEP Sci
  naepMVars <- cb$variableName[cb$fileFormat=="NAEP_Math"]
  naepSVars <- cb$variableName[cb$fileFormat=="NAEP_Science"]
  d6 <- getData(hsts2019, c(stuVar, naepMVars, naepSVars), omittedLevels = FALSE)
  expect_equal(dim(d6), c(47274, 338))
  
  #sum of all NAEP Math Final Link Weight (math_finlkwt)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d6$math_finlnkwt[!is.na(d6$math_finlnkwt)]), c(2777290.219667))
  })
  
  #sum of all NAEP Science Final Link Weight (sci_finlkwt)
  withr::with_options(list(digits = 7, scipen = 999),{
    expect_equal(sum(d6$sci_finlnkwt[!is.na(d6$sci_finlnkwt)]), c(2776990.707734))
  })
  
  #Recalculate the Math GPA score contained in the student record with the actual transcript data
  mathGPAref <- getData(hsts2019, varnames = c("studenti", "mthcred", "mthgpa"), omittedLevels = FALSE) #mthgpa is on the student file, what we want to match!

  #grab transcripts and catalog data, filter them by scedcode "02" (SCED reference here: https://nces.ed.gov/scedfinder)
  trnGPA <- getData(hsts2019, c("studenti", "honcred", "mthcred", "grreqflg", trnsVar, ctlgVar), omittedLevels = FALSE)
  mathGPA <- subset(trnGPA,  grepl("^02", trnGPA$scedcode)) #02 codes are for math
  
  #calculate the grade points, all other grades are dropped
  mathGPA$gradePoints <- NA #no points to start
  mathGPA$gradePoints[mathGPA$stdgrad=="A"] <- 4
  mathGPA$gradePoints[mathGPA$stdgrad=="B"] <- 3
  mathGPA$gradePoints[mathGPA$stdgrad=="C"] <- 2
  mathGPA$gradePoints[mathGPA$stdgrad=="D"] <- 1
  mathGPA$gradePoints[mathGPA$stdgrad=="F"] <- 0
  
  mathGPA$gradePointsCalc <- (mathGPA$gradePoints * mathGPA$carncred)
  mathGPA$credHrsCalc <- ifelse(!is.na(mathGPA$gradePoints), mathGPA$carncred, NA) #only give credit hours for courses counted
  
  #use dplyr here for ease of use
  mathGPAres <- mathGPA %>% 
                  group_by(studenti, schoolid) %>% 
                  summarize(gradePoints = sum(gradePointsCalc, na.rm = TRUE), creditHrs = sum(credHrsCalc, na.rm = TRUE))
  
  #merge calculated result base to the reference data.frame
  mathGPAres$Result <- round((mathGPAres$gradePoints / mathGPAres$creditHrs) + 0.00000001, digits = 2)
  
  mathGPAref2 <- merge(mathGPAref, mathGPAres, by="studenti", all.x = TRUE)
  mathGPAref2$Result[mathGPAref2$Result == Inf] <- NA #convert Inf to NA to indicate 
  
  #test raw values match!
  expect_equal(mathGPAref2$mthgpa, mathGPAref2$Result) #math GPA check
  
  mathGPAref2 <- mathGPAref2[ , c("studenti", "gradePoints", "creditHrs", "Result")]
  mathGPAref2$testMathGPA <- mathGPAref2$Result
  mathGPAref2$Result <- NULL
  
  #TEST WITH esdf MERGE function
  hsts2019merge <- merge(hsts2019, mathGPAref2, by = "studenti")
  
  expect_equal(hsts2019merge$mthgpa, hsts2019merge$testMathGPA)
})

context("HSTS merge cache testing")
test_that("HSTS merge cache testing",{

})

context("HSTS edsurveyTable")
test_that("HSTS edsurveyTable", {
  
  #confirm we can check ref values with RUD data?
  
  withr::with_options(list(digits = 7, scipen=999),{
    et1 <- edsurveyTable(math_composite ~ sex, hsts2019, weightVar = "math_finlnkwt", jrrIMax = Inf)
    et1 <- capture.output(et1)
  })
  
  #et1REF <- c()
  #expect_equal(et1, et1REF)
  
  #c102805 Schl offers: Programming classes (school level)
  withr::with_options(list(digits = 7, scipen=999),{
    et2 <- edsurveyTable(sci_univariate ~ c102805, hsts2019, weightVar = "sci_finlnkwt", jrrIMax = Inf)
    et2 <- capture.output(et2)
  })
  
  #et2REF <- c()
  #expect_equal(et2, et2REF)
})

context("HSTS mixed model")
test_that("HSTS mixed model", {
  
  #target mixed model: Math_composite ~ [AP math offered at the school] + (1|school)
  hstsMixed <- readHSTS(dataFilePath = sppFld, year = 2019, verbose = FALSE)
  cb <- showCodebook(hstsMixed)
  
  schoolCatalog <- getData(hstsMixed, cb$variableName[cb$fileFormat=="School_Catalog"], omittedLevels = FALSE)
  
  mathCatalog <- schoolCatalog[grepl("^02", schoolCatalog$schcat_scedcode), ]
  APMathCatalog <- mathCatalog[mathCatalog$schcat_crselvl %in% c("College credit", "Enhanced/advanced", "Honors") , ]
  
  schoolOffersHonorsMath <- data.frame(schoolid = unique(schoolCatalog$schoolid))
  schoolOffersHonorsMath$OffersHonorsMath <- factor(ifelse(schoolOffersHonorsMath$schoolid %in% APMathCatalog$schoolid,1,2), levels = c(1,2), labels = c("Yes", "No"))
  #schcat_crselvl to determine if advanced placement
  
  hsts2019 <- merge(hstsMixed, schoolOffersHonorsMath, by = "schoolid")
  
  #  Yes    No 
  #27004 20270
  #table(hsts2019$OffersHonorsMath)
  
  #NEEDS TO BE UPDATED FOR WEIGHTS AND RESULTS!
  expect_error(res <- mixed.sdf(math_composite ~ OffersHonorsMath + (1|schoolid), hstsMixed), "automated weights")
})


