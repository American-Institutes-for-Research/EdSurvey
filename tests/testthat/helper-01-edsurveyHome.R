on_spp <- ifelse(dir.exists("I:/NCES"),TRUE,FALSE)

if (!exists("edsurveyHome")) {
  if (Sys.info()[["sysname"]] == "Windows") {
    if(on_spp) {
      edsurveyHome <- "P:/EdSurveyData/"
    } else {
      edsurveyHome <- "C:/EdSurveyData/"
    }
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}
