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

# @author Ahmad Emad
achievementLevelsHelp <- function(grade, year, subject) {
  # return achievement levels
  temp <- "Mathematics\t1990-present\tGrade 4\t214\t249\t282
Mathematics\t1990-present\tGrade 8\t262\t299\t333
Mathematics\t1990-2003\tGrade 12\t288\t336\t367
Mathematics\t2005-present\tGrade 12\t141\t176\t216
Reading\t1992-2007\tGrade 4\t208\t238\t268
Reading\t2009-present\tGrade 4\t208\t238\t268
Reading\t1992-2007\tGrade 8\t243\t281\t323
Reading\t2009-present\tGrade 8\t243\t281\t323
Reading\t1992-2007\tGrade 12\t265\t302\t346
Reading\t2009-present\tGrade 12\t265\t302\t346
Science\t1990-2005\tGrade 4\t138\t170\t205
Science\t2009-present\tGrade 4\t131\t167\t224
Science\t1990-2005\tGrade 8\t143\t170\t208
Science\t2009-present\tGrade 8\t141\t170\t215
Science\t1990-2005\tGrade 12\t146\t178\t210
Science\t2009-present\tGrade 12\t142\t179\t222
Writing\t1998-2007\tGrade 4\t115\t176\t225
Writing\t2011\tGrade 4\t115\t176\t225
Writing\t1998-2007\tGrade 8\t114\t173\t224
Writing\t2011\tGrade 8\t120\t173\t211
Writing\t1998-2007\tGrade 12\t122\t178\t230
Writing\t2011\tGrade 12\t122\t173\t210
Civics\tall\tGrade 4\t136\t177\t215
Civics\tall\tGrade 8\t134\t178\t213
Civics\tall\tGrade 12\t139\t174\t204
Geography\tall\tGrade 4\t187\t240\t276
Geography\tall\tGrade 8\t242\t282\t315
Geography\tall\tGrade 12\t270\t305\t339
History\tall\tGrade 4\t195\t243\t276
History\tall\tGrade 8\t252\t294\t327
History\tall\tGrade 12\t294\t325\t355
Economics\tall\tGrade 12\t123\t160\t208
"
  temp2 <- unlist(strsplit(temp,"\n"))
  temp2 <- temp2[1:32]
  temp2 <- sapply(temp2, function(x) unlist(strsplit(x,"\t")),
                  USE.NAMES = FALSE)
  temp2 <- data.frame(matrix(unlist(temp2), nrow = 32, byrow = TRUE))
  names(temp2) <- c("Subject",  "Year",  "Grade",  "Basic",  "Proficient",  "Advanced")
  temp2[,"Subject"] <- gsub(" ","",temp2$Subject)
  
  # filter to just this grade and subject
  levels <- temp2[temp2$Subject == subject & temp2$Grade == grade,]
  if(nrow(levels) > 1) {
    for(i in seq(1,nrow(levels))) {
      y <- as.character(levels$Year[i])
      # if there is only one year (not a range)
      if(length(strsplit(y, "-")[[1]]) == 1) {
        if(y == year) {
          return(c(as.character(levels$Basic)[i], 
                   as.character(levels$Proficient)[i], as.character(levels$Advanced)[i]))
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
        return(c(as.character(levels$Basic)[i], 
                 as.character(levels$Proficient)[i], as.character(levels$Advanced)[i]))
      }
    } # end for(i in seq(1, nrow(levels)))
  } # end if(nrow(levels) > 1) {
  if(nrow(levels) == 1) {
    return (c(as.character(levels$Basic), 
              as.character(levels$Proficient), as.character(levels$Advanced)))
  }
  return (rep("Not found", 3))
}
