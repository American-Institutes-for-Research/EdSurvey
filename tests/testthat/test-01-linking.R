context("Linking mean")
fe <- file.exists("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 G12 MR/G12 2019 MR_Review 4/Y50MR3/Y50MR3/Data/M50NT3AT.dat")
skip_if_not(fe, message = "Not on SPP, skipping linking error tests")
options(useFancyQuotes=FALSE)

civ <- "I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2018/NAEP 2018 SS_ Review/NAEP 2018 SS_Review 8/Y49CGH/Data/C49NT2AT.dat"
skip_if_not(file.exists(civ), message = "Could not find Civics")
geo <- "I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2018/NAEP 2018 SS_ Review/NAEP 2018 SS_Review 8/Y49CGH/Data/G49NT2AT.dat"
skip_if_not(file.exists(geo), message = "Could not find Geology")
hst <- "I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2018/NAEP 2018 SS_ Review/NAEP 2018 SS_Review 8/Y49CGH/Data/H49NT2AT.dat"
skip_if_not(file.exists(hst), message = "Could not find History")

skip_on_cran()
# math g12
g12 <- readNAEP("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/NAEP 2019 G12 MR/G12 2019 MR_Review 4/Y50MR3/Y50MR3/Data/M50NT3AT.dat")
# make SS version
g1 <- getData(g12, c("composite", "num_oper_theta", "da_stat_prob_theta", "algebra_theta", "measurementgeom_theta", "num_oper", "da_stat_prob", "algebra", "measurementgeom", "dsex", "origwt", "dbapba", "repgrp1", "jkunit", "sdracem"), omittedLevels=FALSE)
# subscaleWeights <- c(0.1, 0.25, 0.35, 0.3)
# subscales <- c("num_oper", "da_stat_prob", "algebra", "measurementgeom")
pvs <- list(getPlausibleValue(var="num_oper", data=g12),
            getPlausibleValue(var="da_stat_prob", data=g12),
            getPlausibleValue(var="algebra", data=g12),
            getPlausibleValue(var="measurementgeom", data=g12))
pvst <- list(getPlausibleValue(var="num_oper_theta", data=g12),
             getPlausibleValue(var="da_stat_prob_theta", data=g12),
             getPlausibleValue(var="algebra_theta", data=g12),
             getPlausibleValue(var="measurementgeom_theta", data=g12))
wjk <- getWeightJkReplicates(data=g12, "origwt")
g1a <- augment(data=g1, subscaleWeights=c(0.1, 0.25, 0.35, 0.3),
               DBA_PVs=pvst, PBA_PVs=pvs,  DBAW="origwt", PBAW="origwt", PBArepWs=wjk, DBArepWs=wjk,
               PVs= getPlausibleValue(var="composite", data=g12), StratumVar="repgrp1", PSUvar="jkunit")
cuts <- c(141, 176, 216)
# check SS v normal functions
# mean
mean_getA <- getA(data=g1a, repW=wjk, stat="MEAN", getPlausibleValue(var="composite", data=g12))
mean_ed <- summary(lm.sdf(composite_linking ~ 1, data=g12))
colnames(mean_ed$coefmat)[c(1,4)] <- c("est","df") 
expect_equal(mean_ed$coefmat[ , c(1,2,4), drop=TRUE], mean_getA[1:3], tolerance=1e-8)

# achievement levels
context("Linking achievement level")
achLvl_getdA <- getA(data=g1a, repW=wjk, stat="AT BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)
achLvl_getcA <- getA(data=g1a, repW=wjk, stat=">= BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)

achLvl_ed <- achievementLevels(achievementVars=c("composite_linking"), data=g12, returnCumulative=TRUE)
unstar <- function(x) {
  unname(unlist(x))
}
# compare "at basic"
expect_equal(unstar(achLvl_ed$discrete[2, 4:5]), unstar(achLvl_getdA[1:2]), tolerance=1e-8)
# compare "at or above basic"
expect_equal(unstar(achLvl_ed$cumulative[2, 4:5]), unstar(achLvl_getcA[1:2]), tolerance=1e-8)
# percentile
context("Linking percentiles")
pct_getA <- getA(data=g1a, repW=wjk, stat="75TH", getPlausibleValue(var="composite", data=g12))
pctLvl_ed <- percentile("composite_linking", data=g12, percentile=75, pctMethod="simple")
colnames(pctLvl_ed)[2] <- c("est") 
expect_equal(c(pctLvl_ed[c(2, 3, 4)]), pct_getA[c(1, 2, 3)], tolerance=1e-8)
# gap
context("Linking gap")
gap_getA <- getAB(data=g1a, repW=wjk, stat="MEAN", PVs = getPlausibleValue(var="composite", data=g12),
      groupA=g1a$sdracem == "Black", groupB=g1a$sdracem=="White")
gap_ed <- gap("composite_linking", data=g12, groupA=sdracem=="Black", groupB=sdracem=="White", noCov=TRUE)
expect_equal(gap_ed$results[c(1, 2, 3, 4)], gap_getA$results[c(1, 2, 5, 6)], tolerance=1e-8)

# AL gaps
gAL <- gap("composite_linking", data=g12, groupA=sdracem=="Black", groupB=sdracem=="White", achievementLevel="Basic", noCov=TRUE)
gAL1 <- getA(data=subset(g1a, sdracem=="Black"), repW=wjk, stat=">= BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)
gAL2 <- getA(data=subset(g1a, sdracem=="White"), repW=wjk, stat=">= BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)
expect_equal(c(gAL$results$estimateA, gAL$results$estimateAse, gAL$results$estimateB, gAL$results$estimateBse),
             c(gAL1$est, gAL1$se, gAL2$est, gAL2$se), tolerance=1E-8)

gALd <- gap("composite_linking", data=g12, groupA=sdracem=="Black", groupB=sdracem=="White", achievementLevel="Basic", noCov=TRUE, achievementDiscrete=TRUE)
gALd1 <- getA(data=subset(g1a, sdracem=="Black"), repW=wjk, stat="AT BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)
gALd2 <- getA(data=subset(g1a, sdracem=="White"), repW=wjk, stat="AT BASIC", getPlausibleValue(var="composite", data=g12), cuts=cuts)
expect_equal(c(gALd$results$estimateA, gALd$results$estimateAse, gALd$results$estimateB, gALd$results$estimateBse),
             c(gALd1$est, gALd1$se, gALd2$est, gALd2$se), tolerance=1E-8)

gpct <- gap("composite_linking", data=g12, groupA=sdracem=="Black", groupB=sdracem=="White", percentiles = 25, noCov=TRUE, pctMethod="simple")
gpct1 <- getA(data=subset(g1a, sdracem=="Black"), repW=wjk, stat="25TH", getPlausibleValue(var="composite", data=g12), cuts=cuts)
gpct2 <- getA(data=subset(g1a, sdracem=="White"), repW=wjk, stat="25TH", getPlausibleValue(var="composite", data=g12), cuts=cuts)
expect_equal(c(gpct$results$estimateA, gpct$results$estimateAse, gpct$results$estimateB, gpct$results$estimateBse),
             c(gpct1$est, gpct1$se, gpct2$est, gpct2$se), tolerance=1E-8)

# expect an error noting Taylor is not supported
context("Linking no Taylor")
expect_error(lm.sdf(composite_linking ~ sdracem, data=g12, varMethod="Taylor"), "Taylor series variance estimation not supported with NAEP linking error.")
expect_warning(lm.sdf(composite_linking ~ sdracem, data=g12, jrrIMax=2), "jrrIMax")
expect_error(glm.sdf(I(composite_linking>220) ~ sdracem, data=g12, varMethod="Taylor", family=binomial()), "Taylor series variance estimation not supported with NAEP linking error.")
expect_error(logit.sdf(I(composite_linking>220) ~ sdracem, data=g12, varMethod="Taylor"), "Taylor series variance estimation not supported with NAEP linking error.")
expect_warning(glm.sdf(I(composite_linking>220) ~ sdracem, data=g12, jrrIMax=2, family=binomial()), "jrrIMax")
expect_warning(rq.sdf(composite_linking ~ sdracem, data=g12, jrrIMax=2), "jrrIMax")

### I think we need an error message here: 
#lm.sdf(composite_linking ~ sdracem, data=g12, varMethod="jackknife", standardizeWithSamplingVar = TRUE)
# should work summary 
context("Linking summary functions return")

linkinglmREF <- c("",
                  "Formula: composite_linking ~ sdracem",
                  "",
                  "Weight variable: 'origwt'",
                  "Variance method: jackknife",
                  "JK replicates: 62",
                  "Plausible values: 182", 
                  "jrrIMax: 1",
                  "full data n: 26201",
                  "n used: 25390",
                  "",
                  "Coefficients:", 
                  "                               coef      se      t  dof Pr(>|t|)    ", 
                  "(Intercept)                 159.444   0.537 296.79 69.5  < 2e-16 ***", 
                  "sdracemBlack                -31.091   0.855 -36.38 44.0  < 2e-16 ***", 
                  "sdracemHispanic             -21.909   0.762 -28.75 42.1  < 2e-16 ***", 
                  "sdracemAsian Amer/Pacif Isl  13.521   1.505   8.98 34.2  1.6e-10 ***", 
                  "sdracemAmer Ind/Alaska Nat  -23.440   1.918 -12.22 29.9  3.6e-13 ***", 
                  "sdracemUnclassified          -2.670   1.863  -1.43 46.7     0.16    ", 
                  "---",
                  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", 
                  "",
                  "Multiple R-squared: 0.1433",
                  "")
withr::with_options(list(digits=3), co <- capture.output(summary(lm.sdf(composite_linking ~ sdracem, data=g12))))
expect_equal(co, linkinglmREF)


l1 <- logit.sdf(I(composite_linking > 220) ~ sdracem, data=g12)
l2 <- logit.sdf(I(composite > 220) ~ sdracem, data=g12)
expect_equal(coef(l1), coef(l2))
linkingglmREF <- c("",
                   "Formula: I(composite_linking > 220) ~ sdracem",
                   "Family: binomial (logit)",
                   "",
                   "jrrIMax: 1",
                   "Weight variable: 'origwt'",
                   "Variance method: jackknife",
                   "JK replicates: 62",
                   "full data n: 26201",
                   "n used: 25390",
                   "",
                   "Coefficients:",
                   "                                coef       se        t   dof Pr(>|t|)    ", 
                   "(Intercept)                  -3.5498   0.0941 -37.7433 40.63  < 2e-16 ***", 
                   "sdracemBlack                 -2.9118   0.5831  -4.9939 26.29  3.3e-05 ***", 
                   "sdracemHispanic              -2.1758   0.2928  -7.4316 26.95  5.5e-08 ***", 
                   "sdracemAsian Amer/Pacif Isl   1.5172   0.1806   8.3996 11.99  2.3e-06 ***", 
                   "sdracemAmer Ind/Alaska Nat   -3.1591   5.8081  -0.5439  6.25     0.61    ", 
                   "sdracemUnclassified           0.0649   0.4225   0.1536 57.11     0.88    ", 
                   "---",
                   "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
withr::with_options(list(digits=3), co <- capture.output(summary(l1)))
expect_equal(co, linkingglmREF)
r1 <- rq.sdf(composite_linking ~ sdracem, data=g12)
r2 <- rq.sdf(composite ~ sdracem, data=g12)
expect_equal(coef(r1), coef(r2))
linkinggrqREF <- c("",
                   "Formula: composite_linking ~ sdracem",
                   "",
                   "tau: 0.5", 
                   "jrrIMax: 1",
                   "Weight variable: 'origwt'",
                   "Variance method: jackknife", 
                   "JK replicates: 62",
                   "full data n: 26201",
                   "n used: 25390",
                   "", 
                   "Coefficients:",
                   "                               coef      se       t  dof Pr(>|t|)    ", 
                   "(Intercept)                 160.317   0.671 239.000 73.5  < 2e-16 ***", 
                   "sdracemBlack                -33.479   1.063 -31.498 38.6  < 2e-16 ***", 
                   "sdracemHispanic             -22.734   0.947 -24.004 55.0  < 2e-16 ***", 
                   "sdracemAsian Amer/Pacif Isl  16.074   2.211   7.271 23.3  2.0e-07 ***", 
                   "sdracemAmer Ind/Alaska Nat  -23.975   3.154  -7.601 64.6  1.5e-10 ***", 
                   "sdracemUnclassified          -3.862   3.738  -1.033 66.4     0.31    ", 
                   "---",
                   "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

withr::with_options(list(digits=3), co <- capture.output(summary(r1)))
expect_equal(co, linkinggrqREF)

# should give an error corr 
context("Linking cor must not mix")
expect_error(cor.sdf(algebra, num_oper_linking, data=g12, method="Pearson"), "When correlating two assessment scores, cannot mix _linking variables with variables not including linking error")
expect_error(cor.sdf(algebra_linking, num_oper, data=g12, method="Pearson"), "When correlating two assessment scores, cannot mix _linking variables with variables not including linking error")
# should work corr 
context("Linking cor")
c1 <- cor.sdf(algebra, num_oper, data=g12, method="Pearson")
c2 <- cor.sdf(algebra_linking, num_oper_linking, data=g12, method="Pearson")
c3 <- suppressMessages(cor.sdf(algebra_linking, dsex, data=g12, method="Pearson"))
c4 <- suppressMessages(cor.sdf(dsex, num_oper_linking, data=g12, method="Pearson"))
expect_s3_class(c1, "edsurveyCor")
expect_s3_class(c2, "edsurveyCor")
expect_s3_class(c3, "edsurveyCor")
expect_s3_class(c4, "edsurveyCor")
context("Linking summary2")
# summary2 does not work with linking
expect_error(s1 <- summary2("composite_linking", data=g12), "linking")
context("Linking mixed.sdf")
# mixed does not work with linking
expect_error(m1 <- mixed.sdf(composite_linking ~ 1 + (1|schid), data=g12, weightVars=c("origwt", "schwt")), "linking")
context("Linking mvrlm.sdf")
expect_error(mv1 <- mvrlm.sdf(algebra_linking | measurementgeom_linking ~ dsex, data = g12), "linking")
context("Linking rename")
expect_error(rename.sdf(g12, "composite_linking", "composite_other"), "linking")
expect_error(rename.sdf(g12, "composite", "other_linking"), "linking")
