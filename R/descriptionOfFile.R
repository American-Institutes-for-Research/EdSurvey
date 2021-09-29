# @author Ahmad Emad
descriptionOfFile <- function(filename) {
  # parse NAEP file names
  filename <- casefold(filename, upper = TRUE)
  file <- filename
  filename <- substring(filename, 1:nchar(filename), 1:nchar(filename))
  if(length(filename) != 8) 
    return("Sorry, no information available about this file")
  meaning_of_digits <- vector(mode = 'list', length = 6)
  # There are 8 digits in a NAEP Filename. Defining each 
  # digit here.
  
  names(meaning_of_digits) <- c('1','23','4','5','6','78')
  meaning_of_digits[['1']] <- "Subject"
  meaning_of_digits[['23']] <- "Year"
  meaning_of_digits[['4']] <- "Assessment_Code"
  meaning_of_digits[['5']] <- "Data_Type"
  meaning_of_digits[['6']] <- "Grade_Level"
  meaning_of_digits[['78']] <- "Assessment_Sample"
  
  Subject <- vector(mode='list', length = 0)
  Subject[['C']] <- 'Civics'
  Subject[['E']] <- 'Economics'
  Subject[['G']] <- 'Geography'
  Subject[['H']] <- 'History'
  Subject[['M']] <- 'Mathematics'
  Subject[['R']] <- 'Reading'
  Subject[['S']] <- 'Science'
  Subject[['U']] <- 'Music'
  Subject[['V']] <- 'Visual Arts'
  Subject[['W']] <- 'Writing'
  
  Assessment_Code <- vector(mode = 'list', length = 0)
  Assessment_Code[['L']] <- "Long-Term Trend"
  Assessment_Code[['N']] <- 'National'
  Assessment_Code[['S']] <- 'State'
  
  Data_Type <- vector(mode = 'list', length = 0)
  Data_Type[['T']] <- 'Student Data'
  Data_Type[['C']] <- 'School Data'
  
  Assessment_Sample <- vector(mode = 'list', length = 0)
  Assessment_Sample[['AT']] <- 'Total Sample'
  Assessment_Sample[['RT']] <- 'Modified Sample'
  
  Grade_Level <- vector(mode = 'list', length = 0)
  Grade_Level[['L_1']] <- 'Age 9'
  Grade_Level[['L_2']] <- 'Age 13'
  Grade_Level[['L_3']] <- 'Age 17'
  Grade_Level[['N_1']] <- 'Grade 4'
  Grade_Level[['N_2']] <- 'Grade 8'
  Grade_Level[['N_3']] <- 'Grade 12'
  Grade_Level[['S_1']] <- 'Grade 4'
  Grade_Level[['S_2']] <- 'Grade 8'
  Grade_Level[['S_3']] <- 'Grade 12'
  
  year <- as.numeric(paste(filename[2],filename[3],sep='')) + 1969
  codes <- c(filename[1],
             paste(filename[2],
                   filename[3],
                   sep=''),
             filename[4],
             filename[5],
             paste(filename[4],
                   filename[6],
                   sep='_'),
             paste(filename[7],
                   filename[8],
                   sep=''))
  
  attributes <- vector(mode = 'list', length = 0)
  for (i in 1:6) {
    code <- codes[i]
    area <- meaning_of_digits[[names(meaning_of_digits)[i]]]
    
    if(area!='Year') {
      temp <- get(area)[[code]]
      attributes[[area]] <- temp 
    }
    else attributes[[area]] <- year
  }
  attributes[['filename']] <- file
  return(attributes)  
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
