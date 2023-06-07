# identify if two edsurvey.data.frames (or lesdf's) are the same survey
# that includes year and the name of the survey.
sameSurvey <- function(x, y) {
  checkDataClass(x, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  checkDataClass(y, c("edsurvey.data.frame", "light.edsurvey.data.frame"))

  if (inherits(x, "light.edsurvey.data.frame")) {
    x <- attributes(x)
  } else {
    class(x) <- "list"
  }
  if (inherits(y, "light.edsurvey.data.frame")) {
    y <- attributes(y)
  } else {
    class(y) <- "list"
  }
  noncomp <- c(
    "userConditions", "data", "pvvars", "weights", "dataSch", "dataTch",
    "achievementLevels", "omittedLevels", "fileFormat",
    "fileFormatSchool", "fileFormatTeacher", "defaultConditions", "class", "names",
    "row.names", "recodes", "cache"
  )
  compare <- names(x)[!names(x) %in% noncomp]
  ae <- all.equal(x[compare], y[compare])
  if (inherits(ae, "logical") && length(ae) == 1 && ae == TRUE) {
    return(TRUE)
  }
  return(FALSE)
}
