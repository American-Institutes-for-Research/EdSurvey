\dontrun{
require(dplyr)
PISA2012 <- readPISA(path = paste0(edsurveyHome, "PISA/2012"),
                     database = "INT",
                     countries = "ALB", verbose=TRUE)
ledf <- getData(data = PISA2012, varnames = c("cnt", "oecd", "w_fstuwt",
                                              "st62q04", "st62q11",
                                              "st62q13", "math"),
                dropOmittedLevels = FALSE, addAttributes = TRUE)

omittedLevels <- c('Invalid', 'N/A', 'Missing', 'Miss', 'NA', '(Missing)')
for (i in c("st62q04", "st62q11", "st62q13")) {
  ledf[,i] <- factor(ledf[,i], exclude=omittedLevels)
  ledf[,i] <- as.numeric(ledf[,i])
}

# after applying some dplyr functions, the "light.edsurvey.data.frame" becomes just "data.frame"
PISA2012_ledf <- ledf %>%        
  rowwise() %>% 
  mutate(avg_3 = mean(c(st62q04, st62q11, st62q13), na.rm = TRUE)) %>% 
  ungroup() %>%
  rebindAttributes(data=PISA2012) # could also be called with ledf
class(PISA2012_ledf) 
# again, a light.edsurvey.data.frame
lma <- lm.sdf(formula=math ~ avg_3, data=PISA2012_ledf)
summary(lma)

PISA2012_ledf <- ledf %>%        
  rowwise() %>% 
  mutate(avg_3 = mean(c(st62q04, st62q11, st62q13), na.rm = TRUE)) %>% 
  ungroup() %>%
  rebindAttributes(data=ledf) # return attributes and make a light.edsurvey.data.frame 
# again a light.edsurvey.data.frame
lma <- lm.sdf(formula=math ~ avg_3, data=PISA2012_ledf)
summary(lma)
}
