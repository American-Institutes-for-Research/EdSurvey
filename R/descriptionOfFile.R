# @author Tom Fink and Ahmad Emad
#returns the file information of a NAEP filename, as the filename has specific meanings for it's contents
descriptionOfFile <- function(filename) {
  
  filename <- toupper(filename)
  filename0 <- filename #original
  
  if(!is.character(filename) || nchar(filename) != 8){
    stop(paste0("The specified ", dQuote("filename"), " is invalid and must be 8 characters in length: ", dQuote(filename), "."))
  }
    
  #extract the meaningful characters in the filename
  subjC <- substr(filename, 1, 1)
  yrC <- substr(filename, 2, 3)
  assessCodeC <- substr(filename, 4, 4)
  dataTypeC <- substr(filename, 5, 5)
  gradeLvlC <- substr(filename, 6, 6)
  assessSampC <- substr(filename, 7, 8)
  
  #==== Subject ====
  validSubj <- c(Civics = "C",
                 Economics = "E",
                 Geography = "G",
                 History = "H",
                 Mathematics = "M",
                 Reading = "R",
                 Science = "S",
                 Music = "U",
                 'Visual Arts' = "V",
                 Writing = "W")
  if(!any(subjC %in% validSubj)) {
    stop(paste0("The specified ", dQuote("filename"), " has an invalid subject character at index 1. ", dQuote(subjC), " must match one of the following: ",
                paste(sQuote(validSubj), "-", names(validSubj), sep = "", collapse = ", ")))
  }
  subjName <- names(validSubj[subjC == validSubj])
    
  #==== Year ====
  if(!grepl("^\\d{2}$", yrC)){
    stop(paste0("The specified ", dQuote("filename"), " has an invalid year code at index 2 and 3. ", dQuote(yrC), " must be an integer value indicating the number of years from 1969."))
  }
  year <- as.numeric(yrC) + 1969
  
  #==== Assessment Code ====
  validAssessCodes <- c('Long-Term Trend' = "L",
                        National = "N",
                        State = "S")
  if(!any(assessCodeC %in% validAssessCodes)) {
    stop(paste0("The specified ", dQuote("filename"), " has an invalid assessment code character at index 4. ", dQuote(assessCodeC), " must match one of the following: ",
                paste(sQuote(validAssessCodes), "-", names(validAssessCodes), sep = "", collapse = ", ")))
  }
  assessmentCode <- names(validAssessCodes[assessCodeC == validAssessCodes])
  
  #==== Data Type ====
  validDataType <- c('Student Data' = "T",
                     'School Data' = "C")
  if(!any(dataTypeC %in% validDataType)) {
    stop(paste0("The specified ", dQuote("filename"), " has an invalid data type code character at index 5. ", dQuote(dataTypeC), " must match one of the following: ",
                paste(sQuote(validDataType), "-", names(validDataType), sep = "", collapse = ", ")))
  }
  dataType <- names(validDataType[dataTypeC == validDataType])
  
  #==== Grade Level ====
  if(!grepl("^[1-3]$", gradeLvlC)){
    stop(paste0("The specified ", dQuote("filename"), " has an invalid year code at index 6. ", dQuote(gradeLvlC), " must be an integer value between 1 and 3."))
  }
  
  if(assessCodeC == "L") { #special case for long-term trend
    gradeLvlDesc <- switch(gradeLvlC,
                           "1" = "Age 9",
                           "2" = "Age 13",
                           "3" = "Age 17")
  }else {
    gradeLvlDesc <- switch(gradeLvlC,
                           "1" = "Grade 4",
                           "2" = "Grade 8",
                           "3" = "Grade 12")
  }
  
  #==== Assessment Sample ====
  #don't throw error if match not found
  validAssessSample <- c('Total Sample' = "AT",
                         'Modified Sample' = "RT")
  assessSample <- names(validAssessSample[assessSampC == validAssessSample])
  if(length(assessSample)==0){ #match not found, set to NULL for return status.
    assessSample <- NULL
  }
  
  #output list object
  res <- list(Subject = subjName,
              Year = year,
              Assessment_Code = assessmentCode,
              Data_Type = dataType,
              Grade_Level = gradeLvlDesc,
              Assessment_Sample = assessSample,
              filename = filename0)
  return(res)
}

# @author Tom Fink and Ahmad Emad
achievementLevelsHelp <- function(grade, year, subject, assessmentCode) {

  alDF <- readRDS(system.file("extdata", "NAEP_AL.rds", package="EdSurvey"))
  alDF[,"Subject"] <- gsub(" ","",alDF$Subject)
  
  #filter down the data.frame to our specific rows of interest
  t1 <- grepl(subject, alDF$Subject, ignore.case = TRUE)
  t2 <- grepl(grade, alDF$Grade, ignore.case = TRUE)
  t3 <- grepl(assessmentCode, alDF$AssessmentCode, ignore.case = TRUE)
  
  alDF <- subset(alDF, t1 & t2 & t3)
  
  if(nrow(alDF) > 1) {
    for(i in seq(1,nrow(alDF))) {
      y <- as.character(alDF$Year[i])
      # if there is only one year (not a range)
      if(length(strsplit(y, "-")[[1]]) == 1) {
        if(y == year) {
          retVals <- as.numeric(c(alDF$Level1[i], alDF$Level2[i], alDF$Level3[i]))
          retNames <- unlist(strsplit(alDF$LevelNames[i],"||", fixed = TRUE))
          names(retVals) <- retNames
          return (retVals)
        }
        next
      }
      
      # there is a range, so find out if we are in it
      lower <- as.integer(strsplit(y, "-")[[1]][1])
      upper <- strsplit(y, "-")[[1]][2]
      
      if(upper == "present") {
        upper <- substr(Sys.Date(),1,4)
      }
      upper <- as.integer(upper)
      if(year >= lower & year <= upper) {
        retVals <- as.numeric(c(alDF$Level1[i], alDF$Level2[i], alDF$Level3[i]))
        retNames <- unlist(strsplit(alDF$LevelNames[i],"||", fixed = TRUE))
        names(retVals) <- retNames
        return (retVals)
      }
    } # end for(i in seq(1, nrow(alDF)))
  } # end if(nrow(alDF) > 1) {
  if(nrow(alDF) == 1) {
    retVals <- as.numeric(c(alDF$Level1, alDF$Level2, alDF$Level3))
    retNames <- unlist(strsplit(alDF$LevelNames,"||", fixed = TRUE))
    names(retVals) <- retNames
    return (retVals)
  }
  
  warning(paste0("Unable to determine appropriate achievement levels for this file.\n"))
  return(NULL)
}
