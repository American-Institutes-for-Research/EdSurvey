#' Format AM dct File for Use with DirectEstimation
#' @description
#' Takes an \code{AM dct} file and formats it for use with the \code{mml} method
#' as \code{paramTab}.
#'
#' @param dct a file location from which to read the \code{dct} file
#' @param mml a logical for if the paramTab is being used in \code{mml.sdf}
#'
#' @return a \code{data.frame} in a format suitable for use with \code{mml} as
#' a \code{paramTab}.
#'
#' @author Sun-Joo Lee
#' @export
parseNAEPdct <- function(dct, mml = TRUE) {
  # build
  dichotParamTab <- data.frame()
  polyParamTab <- data.frame()
  testDat <- data.frame()

  # correct factors
  subjects <- c("Mathematics", "Reading", "Science", "Geography", "History", "Writing", "Civics", "Economics", "Arts")
  CompositeSubjects <- c("Mathematics", "Reading", "Science", "Geography", "History", "Writing", "Civics", "Economics", "Arts")
  subtests <- c("algebra", "geometry", "measurement", "number", "data", "information", "literary", "earth", "life", "physical", "environment", "space", "spatial", "cultures", "democracy", "technology", "world", "task", "ltt", "ltt_bridge", "international", "market", "national", "vocabulary", "univariate", "one", "overall")

  dct_obj <- readLines(dct)
  for (line in dct_obj) {
    line <- trimws(tolower(line))
    line <- gsub(" = ", "=", line)
    line <- gsub(" =", "=", line)
    line <- gsub("= ", "=", line)
    if (grepl("irm=3pl", line)) {
      # parse dichot
      line_split <- unlist(strsplit(line, " "))
      naepid <- line_split[1]
      ipa <- as.numeric(unlist(strsplit(line_split[grepl("ipa", line_split)], "="))[2])
      ipb <- as.numeric(unlist(strsplit(line_split[grepl("ipb", line_split)], "="))[2])
      ipc <- as.numeric(unlist(strsplit(line_split[grepl("ipc", line_split)], "="))[2])
      subtest <- as.numeric(unlist(strsplit(line_split[grepl("onsubtest", line_split)], "="))[2])

      params_row <- as.data.frame(
        list(
          ItemID = tolower(naepid),
          subtest = subtest,
          slope = ipa,
          difficulty = ipb,
          guessing = ipc
        )
      )
      dichotParamTab <- rbind(dichotParamTab, params_row)
    } else if (grepl("irm=pcl", line)) {
      # parse poly
      line_split <- unlist(strsplit(line, " "))
      naepid <- line_split[1]
      ipa <- as.numeric(unlist(strsplit(line_split[grepl("ipa", line_split)], "="))[2])
      ipb <- as.numeric(unlist(strsplit(line_split[grepl("ipb", line_split)], "="))[2])

      ds <- c()
      for (dnum in 1:5) {
        d <- as.numeric(unlist(strsplit(line_split[grepl(paste0("ipd", dnum), line_split)], "="))[2])
        if (length(d) == 0) {
          d <- NA
        }
        ds <- c(ds, d)
      }
      subtest <- as.numeric(unlist(strsplit(line_split[grepl("onsubtest", line_split)], "="))[2])

      params_row <- as.data.frame(
        list(
          ItemID = naepid,
          subtest = subtest,
          slope = ipa,
          itemLocation = ipb,
          d1 = ds[1],
          d2 = ds[2],
          d3 = ds[3],
          d4 = ds[4],
          d5 = ds[5]
        )
      )
      polyParamTab <- rbind(polyParamTab, params_row)
    } else if (grepl("^subtest", line)) {
      # parse testDat
      line_split <- tolower(unlist(strsplit(line, " ")))
      line_split <- gsub("[[:punct:]]+$", "", line_split)
      line_split <- gsub("^[[:punct:]]+", "", line_split)
      subtest_name <- intersect(line_split, subtests)

      ### This part will need to be refined to capture all variations of subtest names ###
      if (length(subtest_name) != 1) {
        subtest_name <- unlist(strsplit(line_split[1], "="))[2]
        subtest_name <- tolower(gsub("[[:punct:]]+", "", subtest_name))
        if (subtest_name == "numbers") {
          subtest_name <- "number"
        }
      }
      if ((subtest_name == "space") & (grepl("earth", line))) {
        subtest_name <- "earth"
      }

      if ((subtest_name == "one") | (subtest_name == "overall")) {
        subtest_name <- "univariate"
      }
      ### Up to here ###

      subtest_num <- as.numeric(unlist(strsplit(line_split[grepl("id", line_split)], "="))[2])
      subtest_scale <- as.numeric(unlist(strsplit(line_split[grepl("scale", line_split)], "="))[2])
      subtest_loc <- as.numeric(unlist(strsplit(line_split[grepl("location", line_split)], "="))[2])
      subtest_wgt <- as.numeric(unlist(strsplit(line_split[grepl("weight", line_split)], "="))[2])
      if (identical(subtest_wgt, numeric(0))) {
        subtest_wgt <- 0
      }

      subtest_row <- as.data.frame(
        list(
          subtest = subtest_name,
          subtestNumber = subtest_num,
          scale = subtest_scale,
          location = subtest_loc,
          subtestWeight = subtest_wgt
        )
      )
      testDat <- rbind(testDat, subtest_row)
    } else if (grepl("^test", line)) {
      line_split <- trimws(gsub("[[:punct:] ]+", " ", (unlist(strsplit(unlist(strsplit(line, " ")), "=")))))
      nums <- grep("[[:digit:]]+", line_split, value = TRUE)
      theyear <- as.numeric(nums[1])
      thelevel <- as.numeric(nums[2])
      if (("age" %in% line_split) & ("grade" %in% line_split)) {
        warning("Unclear whether grade or age of students.")
        theleveltype <- ""
      } else if ("age" %in% line_split) {
        theleveltype <- "age"
      } else if ("grade" %in% line_split) {
        theleveltype <- "grade"
      } else {
        theleveltype <- ""
      }
      if (("national" %in% line_split) & ("state" %in% line_split)) {
        warning("Unclear whether National or State test.")
        theregion <- ""
      } else if ("national" %in% line_split) {
        theregion <- "national"
      } else if ("state" %in% line_split) {
        theregion <- "state"
      } else {
        theregion <- ""
      }
      thesubject <- intersect(line_split, tolower(subjects))
      if (length(thesubject) == 1) {
        thesubject <- thesubject
      } else {
        for (part in line_split) {
          subject <- grep(part, tolower(subjects), value = TRUE)
          if (length(subject) == 1) {
            thesubject <- subject
            break
          } else {
            thesubject <- ""
          }
        }
      }
    }
  }

  # check that the DCT had IRT attributes, if not return an error
  if (nrow(dichotParamTab) == 0 && nrow(polyParamTab) == 0 && nrow(testDat) == 0) {
    stop(paste0(
      "Unable to locate dichotomous or polytomous IRT parameters in DCT file: ", dQuote(dct),
      ".  Ensure you specified the correct file and retry."
    ))
  }
  if (nrow(testDat) == 0) {
    stop(paste0(
      "Unable to locate any test/subtest data IRT parameters in DCT file: ", dQuote(dct),
      ".  Ensure you specified the correct file and retry."
    ))
  }

  # change subtest from number to label
  dichotParamTab$subtest <- unlist(lapply(dichotParamTab$subtest, function(x) testDat[testDat$subtestNumber == x, "subtest"]))
  polyParamTab$subtest <- unlist(lapply(polyParamTab$subtest, function(x) testDat[testDat$subtestNumber == x, "subtest"]))
  testDat$subtestNumber <- NULL
  if (tolower(thesubject) %in% tolower(CompositeSubjects)) {
    testDat$test <- "composite"
  } else {
    testDat$test <- "overall"
  }
  # for feeding into mml.sdf
  if (mml) {
    if (nrow(dichotParamTab) > 0) {
      dichotParamTab$D <- 1.7
      dichotParamTab$missingValue <- "c"
      dichotParamTab$missingCode <- 8
      if (tolower(thesubject) %in% tolower(CompositeSubjects)) {
        dichotParamTab$test <- "composite"
      } else {
        dichotParamTab$test <- "overall"
      }
    }
    if (nrow(polyParamTab) > 0) {
      polyParamTab$D <- 1.7
      polyParamTab$missingValue <- "c"
      polyParamTab$missingCode <- 8
      if (tolower(thesubject) %in% tolower(CompositeSubjects)) {
        polyParamTab$test <- "composite"
      } else {
        polyParamTab$test <- "overall"
      }
    }
  } else {
    # for making NAEPirtparams
    if (nrow(dichotParamTab) > 0) {
      dichotParamTab$level <- thelevel
      dichotParamTab$levelType <- theleveltype
      dichotParamTab$assessmentCode <- paste0(toupper(substr(theregion, 1, 1)), substr(theregion, 2, nchar(theregion)))
      dichotParamTab$subject <- paste0(toupper(substr(thesubject, 1, 1)), substr(thesubject, 2, nchar(thesubject)))
      dichotParamTab$year <- theyear
    }

    if (nrow(polyParamTab) > 0) {
      polyParamTab$level <- thelevel
      polyParamTab$levelType <- theleveltype
      polyParamTab$assessmentCode <- paste0(toupper(substr(theregion, 1, 1)), substr(theregion, 2, nchar(theregion)))
      polyParamTab$subject <- paste0(toupper(substr(thesubject, 1, 1)), substr(thesubject, 2, nchar(thesubject)))
      polyParamTab$year <- theyear
    }

    if (nrow(testDat) > 0) {
      testDat$level <- thelevel
      testDat$assessmentCode <- paste0(toupper(substr(theregion, 1, 1)), substr(theregion, 2, nchar(theregion)))
      testDat$subject <- paste0(toupper(substr(thesubject, 1, 1)), substr(thesubject, 2, nchar(thesubject)))
      testDat$year <- theyear
    }
  }

  # return list
  res <- list(
    dichotParamTab = dichotParamTab,
    polyParamTab = polyParamTab,
    testDat = testDat
  )

  return(res)
}

naepParamTabs <- function(params_use) {
  paramTabs <- list()

  # polyParamTab
  poly <- params_use[!is.na(params_use$d1), ]
  polyParamTab <- poly[ , colnames(poly)[!colnames(poly) %in% c("c")]]
  polyParamTab <- polyParamTab[ , c("NAEPid", "subtest", "a", "b", paste0("d", 1:5))]
  colnames(polyParamTab) <- c("ItemID", "subtest", "slope", "itemLocation", paste0("d", 1:5))
  polyParamTab$ItemID <- tolower(polyParamTab$ItemID)
  if (nrow(polyParamTab) > 0) {
    polyParamTab$D <- 1.7
    polyParamTab$missingValue <- "c"
    polyParamTab$missingCode <- 8
  }

  # dichotomous
  bi <- params_use[!params_use$NAEPid %in% poly$NAEPid, ]
  dichotParamTab <- bi[ , colnames(bi)[!colnames(bi) %in% paste0("d", 1:5)]]
  dichotParamTab <- dichotParamTab[ , c("NAEPid", "subtest", "a", "b", "c")]
  colnames(dichotParamTab) <- c("ItemID", "subtest", "slope", "difficulty", "guessing")
  dichotParamTab$ItemID <- tolower(dichotParamTab$ItemID)
  if (nrow(dichotParamTab) > 0) {
    dichotParamTab[is.na(dichotParamTab$guessing), "guessing"] <- 0
    dichotParamTab$D <- 1.7
    dichotParamTab$missingValue <- "c"
    dichotParamTab$missingCode <- 8
  }
  paramTabs$polyParamTab <- polyParamTab
  paramTabs$dichotParamTab <- dichotParamTab

  return(paramTabs)
}


timssParamTabs <- function(params_use) {
  paramTabs <- list()
  # polyParamTab
  poly <- params_use[!is.na(params_use$d1), ]
  polyParamTab <- poly[ , colnames(poly)[!colnames(poly) %in% c("c")]]
  ds <- grep("^d[0-9]$", colnames(polyParamTab), value = TRUE)
  polyParamTab <- polyParamTab[ , c("TIMSSid", "content_subtest", "cognitive_subtest", "a", "b", ds, "subject", "scorePoints")]
  colnames(polyParamTab) <- c("ItemID", "content_subtest", "cognitive_subtest", "slope", "itemLocation", ds, "test", "scorePoints")
  ss <- is.na(polyParamTab$scorePoints)
  for (i in seq_along(ds)) {
    polyParamTab$scorePoints[ss] <- ifelse(!is.na(polyParamTab[ss, ds[i]]), i, polyParamTab$scorePoints[ss])
  }
  polyParamTab$ItemID <- tolower(polyParamTab$ItemID)
  if (nrow(polyParamTab) > 0) {
    polyParamTab$D <- 1.7
  }

  # dichotomous
  bi <- params_use[!params_use$TIMSSid %in% poly$TIMSSid, ]
  dichotParamTab <- bi[ , colnames(bi)[!colnames(bi) %in% paste0("d", 1:2)]]
  dichotParamTab <- dichotParamTab[ , c("TIMSSid", "content_subtest", "cognitive_subtest", "a", "b", "c", "subject")]
  colnames(dichotParamTab) <- c("ItemID", "content_subtest", "cognitive_subtest", "slope", "difficulty", "guessing", "test")
  dichotParamTab$ItemID <- tolower(dichotParamTab$ItemID)
  if (nrow(dichotParamTab) > 0) {
    dichotParamTab[is.na(dichotParamTab$guessing), "guessing"] <- 0
    dichotParamTab$D <- 1.7
  }
  paramTabs$polyParamTab <- polyParamTab
  paramTabs$dichotParamTab <- dichotParamTab
  return(paramTabs)
}

timssParam <- function(timssDir, theYear, theLevel) {
  theYear <- as.numeric(theYear)
  theLevel <- as.numeric(unlist(regmatches(theLevel, gregexpr("[[:digit:]]+", theLevel))))

  if (theYear == 2011) {
    df1Name <- paste0("^T11_G", theLevel, "_ItemParameters[.]xlsx$")
    df2Name <- paste0("^T11_G", theLevel, "_ItemInformation[.]xlsx$")
    df3Name <- paste0("^T11_G", theLevel, "_Transform[.]xlsx$")
    df1File <- list.files(timssDir, df1Name, full.names = TRUE, ignore.case = TRUE) # if multiple files found, assume they are the same file but in different dirs
    df2File <- list.files(timssDir, df2Name, full.names = TRUE, ignore.case = TRUE)
    df3File <- list.files(timssDir, df3Name, full.names = TRUE, ignore.case = TRUE)
    if (length(df1File) == 0 || length(df2File) == 0 || length(df3File) == 0) {
      zips <- paste0(
        ". Download them here: ",
        "https://timssandpirls.bc.edu/timss2011/downloads/T11_ItemParameters.zip", " and ",
        "https://timssandpirls.bc.edu/timss2011/downloads/T11_ItemInformation.zip"
      )
      timssDirout <- timssDir
      if ("2011" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      if ("TIMSS" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      stop(paste0("Make sure ", df1Name, ", ", df2Name, ", and ", df3Name, " are downloaded in ", timssDir, zips, ". ", "Try running: downloadTIMSS('", timssDirout, "', '", theYear, "')"))
    }
    df1m <- suppressMessages(read_excel(df1File[1], sheet = "MAT", progress = FALSE))
    df2m <- suppressMessages(read_excel(df2File[1], sheet = "MAT", progress = FALSE))
    df3m <- suppressMessages(read_excel(df3File[1], sheet = "T11_Transform", progress = FALSE))
    df1s <- suppressMessages(read_excel(df1File[1], sheet = "SCI", progress = FALSE))
    df2s <- suppressMessages(read_excel(df2File[1], sheet = "SCI", progress = FALSE))
    df3s <- suppressMessages(read_excel(df3File[1], sheet = "T11_Transform", progress = FALSE))
  } else if (theYear == 2015) {
    df1Name <- paste0("^T15_G", theLevel, "_ItemParameters[.]xlsx$")
    df2Name <- paste0("^T15_G", theLevel, "_ItemInformation[.]xlsx$")
    df3Name <- paste0("^T15_G", theLevel, "_TransformConstants[.]xlsx$")
    df1File <- list.files(timssDir, df1Name, full.names = TRUE, ignore.case = TRUE) # if multiple files found, assume they are the same file but in different dirs
    df2File <- list.files(timssDir, df2Name, full.names = TRUE, ignore.case = TRUE)
    df3File <- list.files(timssDir, df3Name, full.names = TRUE, ignore.case = TRUE)
    if (length(df1File) == 0 || length(df2File) == 0 || length(df3File) == 0) {
      if (theLevel == 4) {
        zips <- paste0(
          ". Download them here: ",
          "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_IRTItemParameters.zip", " and ",
          "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_ItemInformation.zip"
        )
      } else {
        zips <- paste0(
          ". Download them here: ",
          "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_IRTItemParameters.zip", " and ",
          "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_ItemInformation.zip"
        )
      }
      timssDirout <- timssDir
      if ("2015" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      if ("TIMSS" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      stop(paste0("Make sure ", df1Name, ", ", df2Name, ", and ", df3Name, " are downloaded in ", timssDir, zips, ". ", "Try running: downloadTIMSS('", timssDirout, "', '", theYear, "')"))
    }
    df1m <- suppressMessages(read_excel(df1File[1], sheet = "MAT", progress = FALSE))
    df2m <- suppressMessages(read_excel(df2File[1], sheet = "MAT", progress = FALSE))
    df3m <- suppressMessages(read_excel(df3File[1], sheet = "MAT", progress = FALSE))
    df1s <- suppressMessages(read_excel(df1File[1], sheet = "SCI", progress = FALSE))
    df2s <- suppressMessages(read_excel(df2File[1], sheet = "SCI", progress = FALSE))
    df3s <- suppressMessages(read_excel(df3File[1], sheet = "SCI", progress = FALSE))
  } else if (theYear == 2019) {
    df1Name <- paste0("^eT19_G", theLevel, "_Item Parameters Adj[.]xlsx$|^T19_G", theLevel, "_Item Parameters[.]xlsx$")
    df2Name <- paste0("^e?T19_G", theLevel, "_Item Information[.]xlsx$")
    df3Name <- paste0("^T19_G", theLevel, "_Transform Constants[.]xlsx$")
    df1File <- unique(list.files(timssDir, df1Name, full.names = TRUE, ignore.case = TRUE)) # if multiple files found, assume they are the same file but in different dirs
    df2File <- unique(list.files(timssDir, df2Name, full.names = TRUE, ignore.case = TRUE))
    df3File <- unique(list.files(timssDir, df3Name, full.names = TRUE, ignore.case = TRUE))
    if (length(df1File) == 0 || length(df2File) == 0 || length(df3File) == 0) {
      if (theLevel == 4) {
        zips <- paste0(
          ". Download them here: ",
          "https://timss2019.org/international-database/downloads/T19_G4_Item%20Information.zip", " and ",
          "https://timss2019.org/international-database/downloads/T19_G4_IRT%20Item%20Parameters.zip"
        )
      } else {
        zips <- paste0(
          ". Download them here: ",
          "https://timss2019.org/international-database/downloads/T19_G8_Item%20Information.zip", " and ",
          "https://timss2019.org/international-database/downloads/T19_G8_IRT%20Item%20Parameters.zip"
        )
      }
      timssDirout <- timssDir
      if ("2019" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      if ("TIMSS" == basename(timssDirout)) {
        timssDirout <- dirname(timssDirout)
      }
      stop(paste0("Make sure ", df1Name, ", ", df2Name, ", and ", df3Name, " are downloaded in ", timssDir, zips, ". ", "Try running: downloadTIMSS('", timssDirout, "', '", theYear, "')"))
    }
    fixDF1 <- function(z, subject) {
      res <- suppressMessages(read_excel(z, sheet = subject, progress = FALSE))
      if (theYear == 2019 & "2015" %in% res[2, 4] & "RMSD" %in% res[1, 4]) {
        res <- res[ , -4]
      }
      colnames(res) <- NA
      return(res)
    }
    df1m <- do.call(rbind, lapply(df1File, fixDF1, subject = "MAT"))
    df2m <- do.call(rbind, lapply(df2File, function(z) {
      (suppressMessages(read_excel(z, sheet = "MAT", progress = FALSE)))
    }))
    df3m <- suppressMessages(read_excel(df3File[1], sheet = "MAT", progress = FALSE))
    df1s <- do.call(rbind, lapply(df1File, fixDF1, subject = "SCI"))
    df2s <- do.call(rbind, lapply(df2File, function(z) {
      (suppressMessages(read_excel(z, sheet = "SCI", progress = FALSE)))
    }))
    df3s <- suppressMessages(read_excel(df3File[1], sheet = "SCI", progress = FALSE))
  } else {
    stop(paste0("Item parameters are currently not available for ", theYear, "."))
  }
  allParamsM <- timssParamForm(df1m, df2m, df3m, theYear, theLevel, "MAT")
  allParamsS <- timssParamForm(df1s, df2s, df3s, theYear, theLevel, "SCI")
  allParams <- list(
    params = rbind(allParamsM$params, allParamsS$params),
    transformations = rbind(allParamsM$transformations, allParamsS$transformations)
  )
  return(allParams)
}


timssParamForm <- function(itemParamDf, itemInfoDf, transConst, theYear, theLevel, subjectFilter) {
  allParams <- list()
  # clean
  if (theYear %in% 2019) {
    itemParamDf <- itemParamDf[3:nrow(itemParamDf), 3:ncol(itemParamDf)]
    colnames(itemParamDf) <- c("Item ID", "", "a", "", "b", "", "c", "", "d1", "", "d2", "")
  } else {
    itemParamDf <- itemParamDf[3:nrow(itemParamDf), ]
    colnames(itemParamDf) <- c("", "Item ID", "a", "", "b", "", "c", "", "d1", "", "d2", "")
  }
  itemParamDf <- itemParamDf[ , !colnames(itemParamDf) %in% ""]
  itemParamDf <- itemParamDf[!is.na(itemParamDf$`Item ID`), ]
  itemInfoDf <- itemInfoDf[ , c("Item ID", "Grade", "Subject", "Content Domain", "Cognitive Domain", "Maximum Points")]
  # these did not merge, they will not be on this assessment
  params <- merge(itemParamDf, itemInfoDf, by = "Item ID", all.x = TRUE)
  # merge param and info together
  params <- params[!params$"Cognitive Domain" %in% c("", NA), ]
  params[is.na(params$Grade), "Grade"] <- unique(na.omit(params$Grade))
  params[is.na(params$`Content Domain`), "Content Domain"] <- ""
  params$Subject <- ifelse(subjectFilter == "MAT", "mmat", ifelse(subjectFilter == "SCI", "ssci", ""))
  params$year <- as.numeric(theYear)
  colnames(params) <- c(
    "TIMSSid", "a", "b", "c", "d1", "d2", "level",
    "subject", "content_subtest", "cognitive_subtest",
    "scorePoints", "year"
  )

  # clean
  params$TIMSSid <- tolower(params$TIMSSid)
  colsNum <- c("a", "b", "c", "d1", "d2")
  params[colsNum] <- sapply(params[colsNum], as.numeric)
  params$content_subtest <- ifelse(params$content_subtest %in% c("Data Display","Data","Data and Probability","Data and Chance"), "mdat", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest %in% c("Geometric Shapes and Measures","Measurement and Geometry","Geometry"), "mgeo", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Number", "mnum", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Algebra", "malg", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Earth Science", "sear", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Life Science", "slif", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Biology", "sbio", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest == "Chemistry", "sche", params$content_subtest)
  params$content_subtest <- ifelse(params$content_subtest %in% c("Physical Science","Physics"), "sphy", params$content_subtest)
  prefix <- ifelse(subjectFilter == "MAT", "m", "s")
  params$cognitive_subtest <- ifelse(params$cognitive_subtest == "Applying", paste0(prefix, "app"), params$cognitive_subtest)
  params$cognitive_subtest <- ifelse(params$cognitive_subtest == "Knowing", paste0(prefix, "kno"), params$cognitive_subtest)
  params$cognitive_subtest <- ifelse(params$cognitive_subtest == "Reasoning", paste0(prefix, "rea"), params$cognitive_subtest)
  allParams$params <- params

  # transformations
  if (theYear == 2011) {
    if (subjectFilter == "MAT") {
      transformations <- transConst[3:7, 6:7]
      transformations$subject <- "Mathematics"
    } else { # SCI
      transformations <- transConst[11:15, 6:7]
      transformations$subject <- "Science"
    }
  } else { # 2015
    transformations <- transConst[3:nrow(transConst), 6:7]
    transformations$subject <- ifelse(subjectFilter == "MAT", "Mathematics", ifelse(subjectFilter == "SCI", "Science", ""))
  }
  colnames(transformations) <- c("location", "scale", "subject")
  transformations$location <- as.numeric(transformations$location)
  transformations$scale <- as.numeric(transformations$scale)

  # year, grade
  transformations$year <- theYear
  transformations$level <- theLevel
  params$subject <- ifelse(subjectFilter == "MAT", "mmat", ifelse(subjectFilter == "SCI", "ssci", ""))
  allParams$transformations <- transformations

  return(allParams)
}
