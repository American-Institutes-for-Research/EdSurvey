options(width = 500)
options(useFancyQuotes = FALSE)
options(digits = 7)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
source("REF-helpers.R") # has REF output in it
# ideally this wouldn't trip up any of the scope fixes below
dsex <- "should not be used"

test_that("Primer reads in correctly", {
  expect_s3_class(sdf, "edsurvey.data.frame")
  expect_equal(dim(sdf), c(17606, 303))
  expect_equal(c(nrow(sdf), ncol(sdf)), c(17606, 303))
})

test_that("[, [[, [<-, [[<- edsurvey.data.frame", {
  sdf2 <- sdf
  testVal <- sdf[ , "iep"]
  #test index number
  expect_equal(sdf2[ , 6], testVal)
  expect_equal(sdf2[[6]], testVal)
  
  #when assigned here, the index shifts since it will be saved to the esdf 'cache' data.frame and that is first listed in 'colnames'
  #we should document or note this somewhere
  sdf2[ , 6] <- testVal
  expect_equal(sdf2[ , 2], sdf[[6]], ignore_attr = TRUE) #factor re-assignment won't exactly match attributes
  
  #note the origial index was 6, it was changed to 2 from first re-assignment
  sdf2[[2]] <- testVal
  expect_equal(sdf2[[2]], sdf[ , 6], ignore_attr = TRUE) #factor re-assignment won't exactly match attributes
  
  #test by column name
  lm1S <- lm.sdf(~ dsex + b017451, sdf, standardizeWithSamplingVar = TRUE)
  withr::with_options(
    list(digits = 4, width=500),
    slm1Scoef <- capture.output(summary(lm1S, src = TRUE)$coefmat)
  )
  expect_equal(sdf2[ , "iep"], testVal, ignore_attr = TRUE)
  expect_equal(sdf2[["iep"]], testVal, ignore_attr = TRUE)
  
  testVal <- sdf[["dsex"]]
  sdf2[ , "dsex"] <- testVal
  expect_equal(sdf2[ , "dsex"], sdf[["dsex"]], ignore_attr = TRUE)
  
  sdf2[["dsex"]] <- testVal
  expect_equal(sdf2[ , "dsex"], sdf[["dsex"]], ignore_attr = TRUE)
  
  expect_error(sdf2[ , 999999], "Column index out of range.*")
  expect_error(sdf2[ , "NOT A COLUMN ZZZZ"], "The following variable names are required for this call.*")
  
  expect_error(sdf2[[9999999]] <- testVal, "Column index out of range.*")
  expect_error(sdf2[["NOT A COLUMN ZZZ"]] <- testVal, "Cannot find.*as a column name or list item in this object")
  
  #new tests for vector assignment and retrieval
  sdf$testVar <- paste0(sdf$dsex, "-", sdf$iep)
  testDF <- sdf[ , c("dsex", "iep", "testVar")]
  
  for(i in seq_along(testDF)){
    sdf2[ , colnames(testDF[i])] <- testDF[i]
  }
  
  expect_error(sdf[ , c("dsex", "iep", "testVar")] <- testDF, "Try assigning a single column at a time")
  expect_equal(sdf[ , c("dsex", "iep", "testVar")], sdf2[ , c("dsex", "iep", "testVar")], ignore_attr = TRUE)
})

test_that("$ assign", {
  # subset then assign
  sdfM <- subset(sdf, dsex == "Male")
  sdfM$books <- ifelse(sdfM$b013801 %in% c("0-10", "11-25"), "0-25 books", "26+ books")
  sdf$books <- ifelse(sdf$b013801 %in% c("0-10", "11-25"), "0-25 books", "26+ books")
  sdfM2 <- subset(sdf, dsex == "Male")
  tab <- table(sdfM$books, sdfM2$books)
  diag(tab) <- 0
  expect_equal(as.vector(tab), rep(0, 4))
  # assign a new variable
  sdf$a <- sdf$dsex
  tab <- table(sdf$a, sdf$dsex)
  expect_equal(tab, assignTableREF)
  # overwrite a variable, common for recoding
  # note: dsex has no NAs on the reporting sample
  sdf$dsex <- ifelse(sdf$dsex %in% "Male", "boy", "girl")
  tab2 <- table(sdf$a, sdf$dsex)
  expect_equal(unname(tab2), unname(assignTableREF))

  expect_warning(sdf$a[1:5] <- "invalid", "factor level")
  # repeated to be sure this does not throw an error, which it used to
  expect_warning(sdf$a[1:5] <- "invalid", "factor level")
  # reset sdf
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
})

test_that("searchSDF", {
  search1 <- searchSDF(string = c("home", "book"), data = sdf)
  search2 <- searchSDF(string = c("home|book"), data = sdf)
  search3 <- searchSDF(string = "value", data = sdf, levels = TRUE)

  # drop 'fileFormat' var for quick comparison, update the RDS when able
  search1$fileFormat <- NULL
  search2$fileFormat <- NULL
  search3$fileFormat <- NULL

  search2_ref <- structure(list(variableName = c("b013801", "b017001", "b017101", 
"b018201", "b017451", "t088804", "t088805", "t091503"), Labels = c("Books in home", 
"Newspaper in home", "Computer at home", "Language other than English spoken in home", 
"Talk about studies at home", "Computer activities: Use a gradebook program", 
"Computer activities: Post homework,schedule info", "G8Math:How often use Geometry sketchbook"
)), row.names = c(NA, 8L), class = "data.frame")
  search3_ref <- structure(list(variableName = c("m086101", "m020001", "m143601", 
"m142301"), Labels = c("Read value from graph", "Apply place value                            (R1)", 
"Solve for x given value of n", "Identify place value"), Levels = c("1. A; 2. B; 3. C *; 4. D; 5. E; 8. Omitted; 9. Not Reached; 0. Multiple", 
"1. Incorrect; 2. Correct*; 5. Illegible; 6. Off Task; 7. Non-Rateable; 8. Omitted; 9. Not Reached", 
"1. A; 2. B; 3. C; 4. D *; 5. E; 8. Omitted; 9. Not Reached; 0. Multiple", 
"1. A; 2. B; 3. C; 4. D; 5. E *; 8. Omitted; 9. Not Reached; 0. Multiple"
)), row.names = c("Student.131", "Student.165", "Student.177", "Student.214"), class = c("searchSDF", "data.frame"))
  expect_equal(search1, structure(list(variableName = "b013801", Labels = "Books in home"), row.names = 1L, class = "data.frame"))
  expect_equal(search2, search2_ref)
  expect_equal(search3, search3_ref)
})

test_that("showCodebook", {
  cb <- showCodebook(sdf, "school")
  sdfRecode <- recode.sdf(sdf, recode = list(dsex = list(from = c("Male"), to = c("MALE"))))
  cb2 <- showCodebook(sdfRecode, c("student", "school"), labelLevels = FALSE, includeRecodes = TRUE)

  withr::with_options(
    list(digits = 4, width=500),
    sdfRecodeTab <- capture.output(table(sdfRecode$dsex, useNA="always"))
  )
  sdfRecodeTab_REF <- c("",
                        "Female   MALE   <NA> ",
                        "  8429   8486      0 ")

  expect_equal(sdfRecodeTab, sdfRecodeTab_REF)
  expect_snapshot_value(cb2, style="json2")
})


test_that("getData order of userConditions", {
  # subset first, then recode
  sdf_males <- subset(sdf, dsex %in% "Male")
  sdf_males <- recode.sdf(sdf_males, recode = list(dsex = list(from = "Male", to = "Boy")))
  gdat1 <- getData(sdf_males, c("dsex"))
  expect_equal(nrow(gdat1), 8486)
  expect_equal(unique(as.character(gdat1$dsex)), "Boy")

  # recode first then subset
  sdf_males <- recode.sdf(sdf, recode = list(dsex = list(from = "Male", to = "Boy")))
  sdf_males <- subset(sdf_males, dsex %in% "Boy")
  gdat2 <- getData(sdf_males, c("dsex"))
  expect_equal(gdat2, gdat1)
})

test_that("getData", {
  expect_snapshot_value(head(gd1 <- getData(sdf, c("dsex", "b017451"))), style="json2")
})


test_that("subset throws an error", {
  expect_error(sdf_error <- subset(sdf, dsex1 %in% "Male"))
  expect_error(sdf_error <- subset(sdf, dsex == "Male" & dsex1 == "Male"))
})

test_that("edsurvey with $ method", {
  # $ method work for existing attributes
  expect_equal(sdf$country, "USA")
  # $ method return a vector
  expect_equal(class(sdf$dsex), c("lfactor", "factor"))
  # that vector has data
  dsexTab <- structure(c(Male = 8486L, Female = 8429L),
    .Dim = 2L,
    .Dimnames = structure(list(c("Male", "Female")), .Names = ""),
    class = "table"
  )
  expect_equal(table(sdf$dsex), dsexTab)

  # $ method return a data.frame
  expect_equal(class(sdf$composite), "data.frame")
})

test_that("achievementLevel with result of zero", {
  sdfC <- subset(sdf, scrpsu %in% 100:200)
  expect_snapshot_value(test9 <- achievementLevels(data = subset(sdfC, pared == 1)), style="json2", tol=1e-5)
})

test_that("variable label stored as attributes", {
  est1 <- edsurveyTable(composite ~ dsex + b017451, sdf, jrrIMax = 1)
  expect_equal(attr(est1$data$dsex, "label"), "Gender")
  expect_equal(attr(est1$data$b017451, "label"), "Talk about studies at home")
})

test_that("sdf correlation", {
  expect_c1_pear <- cor.sdf("b017451", "b003501", sdf, method = "Pearson", weightVar = "origwt", verbose = FALSE)
  expect_equal(expect_c1_pear$correlation, 0.00920585972204715)
  expect_equal(expect_c1_pear$se, 0.0108269380691247)
})

test_that("achievementLevel basic", {
  expect_snapshot_value(test1 <- achievementLevels(returnCumulative = TRUE, data = sdf), style="json2", tol=1e-5)
})

test_that("gap", {
  # gap SD
  expect_snapshot_value(g0 <- gap("composite", sdf, dsex == "Male", dsex == "Female", returnSimpleDoF = TRUE, stDev = TRUE), style="json2", tol=1e-5)
  # gap means
  expect_snapshot_value(g1 <- gap("composite", sdf, dsex == "Male", dsex == "Female", returnSimpleDoF = TRUE), style="json2", tol=1e-5)
  g1q <- gap("composite", sdf, "dsex==\"Male\"", "dsex==\"Female\"", returnSimpleDoF = TRUE)
  g1q$call <- g1$call # the call is different, so fix that
  expect_snapshot_value(g1q, style="json2", tol=1e-5)
})