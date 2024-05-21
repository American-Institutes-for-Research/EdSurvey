# @author Paul Bailey
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("EdSurvey v", utils::packageDescription("EdSurvey")$Version, "\n"))
  setOptionIfNotNull("EdSurvey_round_output", FALSE)
  setOptionIfNotNull("EdSurvey_round_n_function", roundNCES)
  setOptionIfNotNull("EdSurvey_round_pop_n_function", roundNCES)
  setOptionIfNotNull("EdSurvey_round_est_function", roundn(3))
  setOptionIfNotNull("EdSurvey_round_est_se_function", roundn(4))
  setOptionIfNotNull("EdSurvey_round_pct_function", roundn(0))
  setOptionIfNotNull("EdSurvey_round_pct_se_function", roundn(1))
}

setOptionIfNotNull <- function(option, value) {
  if(is.null(getOption(option))) {
    cl <- list(value)
    names(cl) <- option
    do.call(options, cl)
  }
}

globalVariables(c(
  "variable", "w", "c2", "dofNum", "dofDenom", "lengthY",
  "sumY", "Percent", "wtdN", "N", "pcti", "value", "Vjrr",
  "Vimp", "Level", "JKreplicate", "PV", "V1", "cnt_index",
  "w_lmi", "StandardError", "flag1", "wtdN2", "Denom",
  "DenomGroup", ".GRP", "answer", "Year", "..by.x", "..by.y",
  "component", "maxRound", "variableName", "content_subtest", "cognitive_subtest"
))
