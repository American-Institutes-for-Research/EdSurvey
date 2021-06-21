if(FALSE) {
skip_on_cran()
# set-up 
require(testthat)
options(width = 500)
options(useFancyQuotes=FALSE)
options(digits=7)


# comparison data source: \\dc3fs1\dc4work\ESSIN Task 14\Reporting and Dissemination\01_Technical Review\_NRC DataCheck\2019 G12 M R\2019 G12 M
#mat <- readRDS("I:/NCES/NCES_Dev/Ting/Data companion/G12_2019MR_SDT/Y50MR3/Data/M50NT3AT.rds")
# other potential compares 
#civ <- readNAEP("I:/NCES/NCES_Dev/Ting/Data companion/NAEP 2018 SS_Review 5/Y49CGH/Data/C49NT2AT.dat")
#his <- readNAEP("I:/NCES/NCES_Dev/Ting/Data companion/NAEP 2018 SS_Review 5/Y49CGH/Data/H49NT2AT.dat")
#red <- readNAEP("I:/NCES/NCES_Dev/Ting/Data companion/G12_2019MR_SDT/Y50MR3/Data/R50NT3AT.dat")
sci4 <- readNAEP("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/NAEP2019SCI/NAEP2019 Sci_SDT Check/Y50SCI/Data/S50NT1AT.dat")
sci8 <- readNAEP("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/NAEP2019SCI/NAEP2019 Sci_SDT Check/Y50SCI/Data/S50NT2AT.dat")
sci12 <- readNAEP("I:/NCES/NCES_Dev/NAEP DATA REVIEW_DATA/NAEP DATA REVIEW/NAEP 2019/NAEP2019SCI/NAEP2019 Sci_SDT Check/Y50SCI/Data/S50NT3AT.dat")

# build sci4 for SS
uni_thetapv <- c("sthuv1", "sthuv2", "sthuv3", "sthuv4", "sthuv5", "sthuv6", 
                 "sthuv7", "sthuv8", "sthuv9", "sthuv10", "sthuv11", "sthuv12", 
                 "sthuv13", "sthuv14", "sthuv15", "sthuv16", "sthuv17", "sthuv18", 
                 "sthuv19", "sthuv20")

uni_pv <- c("srpuv1", "srpuv2", "srpuv3", "srpuv4", "srpuv5", "srpuv6", 
            "srpuv7", "srpuv8", "srpuv9", "srpuv10", "srpuv11", "srpuv12", 
            "srpuv13", "srpuv14", "srpuv15", "srpuv16", "srpuv17", "srpuv18", 
            "srpuv19", "srpuv20")

repw <- c("srwt1", "srwt2", "srwt3", "srwt4", "srwt5", "srwt6", "srwt7", 
          "srwt8", "srwt9", "srwt10", "srwt11", "srwt12", "srwt13", "srwt14", 
          "srwt15", "srwt16", "srwt17", "srwt18", "srwt19", "srwt20", "srwt21", 
          "srwt22", "srwt23", "srwt24", "srwt25", "srwt26", "srwt27", "srwt28", 
          "srwt29", "srwt30", "srwt31", "srwt32", "srwt33", "srwt34", "srwt35", 
          "srwt36", "srwt37", "srwt38", "srwt39", "srwt40", "srwt41", "srwt42", 
          "srwt43", "srwt44", "srwt45", "srwt46", "srwt47", "srwt48", "srwt49", 
          "srwt50", "srwt51", "srwt52", "srwt53", "srwt54", "srwt55", "srwt56", 
          "srwt57", "srwt58", "srwt59", "srwt60", "srwt61", "srwt62")

scompPV <-  c('srpuv1', 'srpuv2', 'srpuv3', 'srpuv4', 'srpuv5', 'srpuv6', 'srpuv7', 'srpuv8', 'srpuv9', 'srpuv10', 'srpuv11', 'srpuv12', 'srpuv13', 'srpuv14', 'srpuv15', 'srpuv16', 'srpuv17', 'srpuv18', 'srpuv19', 'srpuv20')

msci4 <- augment(data=getData(data=sci4, varnames=c("dbapba","origwt", repw, scompPV, uni_thetapv, "repgrp1", "jkunit", "dsex", uni_pv), omittedLevels =FALSE) ,
                 subscaleWeights=c(1), 
                 DBA_PVs= list(uni_thetapv),# thetas
                 PBA_PVs= list(uni_pv),# scaled
                 DBAW="origwt",
                 PBAW="origwt",
                 PBArepWs=repw,
                 DBArepWs=repw,
                 PVs=scompPV,
                 StratumVar="repgrp1",
                 PSUvar="jkunit") # 

# compare
m1 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="MEAN", PVs=scompPV, cuts=NULL)
m2 <- getA(data=subset(msci4, dsex %in% "Female"), repW=repw, stat="MEAN", PVs=scompPV, cuts=NULL)
edsurveyTable(univariate_scale_linking ~ dsex, data=sci4, jrrIMax=1)
g1 <- gap("univariate_scale_linking", groupA= dsex=="Male", groupB= dsex=="Female", data=sci4, returnSimpleDoF=TRUE)
cbind(m1, m2)


meanGetA <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="MEAN", PVs=scompPV, cuts=NULL)
stdevGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="ST DEV", PVs=scompPV, cuts=NULL)
cuts <- getAttributes(sci4, "pvvars")[["univariate_scale_linking"]]$achievementLevel
bscGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="AT BASIC", PVs=scompPV, cuts=cuts)
profGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="AT PRFNT", PVs=scompPV, cuts=cuts)
bBscGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="< BASIC", PVs=scompPV, cuts=cuts)
aBscGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat=">= BASIC", PVs=scompPV, cuts=cuts)
aProfGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat=">= PRFNT", PVs=scompPV, cuts=cuts)
advGeta <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="ADVANCED", PVs=scompPV, cuts=cuts)
# calculate stats using other edsurvey methods
sciTable <- edsurveyTable(univariate_scale_linking ~ dsex, data=sci4, jrrIMax=1)
sciSum <- summary2("univariate_scale_linking", data=subset(sci4, dsex=="Male"))
sciLvls <- achievementLevels(achievementVars = c("univariate_scale_linking","dsex"), 
                             data = sci4, 
                             weightVar = "origwt", 
                             aggregateBy="dsex",
                             returnCumulative=TRUE)

sciLvlsD <- sciLvls$discrete
sciLvlsC <- sciLvls$cumulative
# comparison between getA and other edsurvey methods 
# check mean
expect_equal(meanGetA$est, sciSum$summary$Mean)
expect_equal(stdevGeta$est, sciSum$summary$SD)
expect_equal(bscGeta$est, sciLvlsD[sciLvlsD$Level == "At Basic" & sciLvlsD$dsex == "Male",]$Percent)
expect_equal(profGeta$est, sciLvlsD[sciLvlsD$Level == "At Proficient" & sciLvlsD$dsex == "Male",]$Percent)
# large disagreement on achievement lvls starting here 
expect_equal(bBscGeta$est, sciLvlsD[sciLvlsD$Level == "Below Basic" & sciLvlsD$dsex == "Male",]$Percent)
expect_equal(aBscGeta$est, sciLvlsC[sciLvlsC$Level == "At or Above Basic" & sciLvlsC$dsex == "Male",]$Percent)
expect_equal(aProfGeta$est, sciLvlsC[sciLvlsC$Level == "At or Above Proficient" & sciLvlsC$dsex == "Male",]$Percent)
expect_equal(advGeta$est, sciLvlsC[sciLvlsC$Level == "At Advanced" & sciLvlsC$dsex == "Male",]$Percent)

p1l <- percentile(variable="univariate_scale_linking",
                  data=subset(sci4, dsex=="Male"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")


v1 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="10TH", PVs=scompPV, cuts=NULL)
v2 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="25TH", PVs=scompPV, cuts=NULL)
v3 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="50TH", PVs=scompPV, cuts=NULL)
v4 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="75TH", PVs=scompPV, cuts=NULL)
v5 <- getA(data=subset(msci4, dsex %in% "Male"), repW=repw, stat="90TH", PVs=scompPV, cuts=NULL)
pctm <- as.data.frame(rbind(v1, v2, v3, v4, v5))


# end science tests, setup math
mat <- readNAEP("I:/NCES/NCES_Dev/Ting/Data companion/G12_2019MR_SDT/Y50MR3/Data/M50NT3AT.dat")


num_oper_thetapv <- c("mths11", "mths12", "mths13", "mths14", "mths15", "mths16", 
                      "mths17", "mths18", "mths19", "mths110", "mths111", "mths112", 
                      "mths113", "mths114", "mths115", "mths116", "mths117", "mths118", 
                      "mths119", "mths120")
da_stat_prob_thetapv <- c("mths41", "mths42", "mths43", "mths44", "mths45", "mths46", 
                          "mths47", "mths48", "mths49", "mths410", "mths411", "mths412", 
                          "mths413", "mths414", "mths415", "mths416", "mths417", "mths418", 
                          "mths419", "mths420")
algebra_thetapv <- c("mths51", "mths52", "mths53", "mths54", "mths55", "mths56", 
                     "mths57", "mths58", "mths59", "mths510", "mths511", "mths512", 
                     "mths513", "mths514", "mths515", "mths516", "mths517", "mths518", 
                     "mths519", "mths520")
measurementgeom_thetapv <- c("mths61", "mths62", "mths63", "mths64", "mths65", "mths66", 
                             "mths67", "mths68", "mths69", "mths610", "mths611", "mths612", 
                             "mths613", "mths614", "mths615", "mths616", "mths617", "mths618", 
                             "mths619", "mths620")

num_oper_pv <- c("mrps11", "mrps12", "mrps13", "mrps14", "mrps15", "mrps16", 
                 "mrps17", "mrps18", "mrps19", "mrps110", "mrps111", "mrps112", 
                 "mrps113", "mrps114", "mrps115", "mrps116", "mrps117", "mrps118", 
                 "mrps119", "mrps120")
da_stat_prob_pv <- c("mrps41", "mrps42", "mrps43", "mrps44", "mrps45", "mrps46", 
                     "mrps47", "mrps48", "mrps49", "mrps410", "mrps411", "mrps412", 
                     "mrps413", "mrps414", "mrps415", "mrps416", "mrps417", "mrps418", 
                     "mrps419", "mrps420")
algebra_pv <- c("mrps51", "mrps52", "mrps53", "mrps54", "mrps55", "mrps56", 
                "mrps57", "mrps58", "mrps59", "mrps510", "mrps511", "mrps512", 
                "mrps513", "mrps514", "mrps515", "mrps516", "mrps517", "mrps518", 
                "mrps519", "mrps520")
measurementgeom_pv <- c("mrps61", "mrps62", "mrps63", "mrps64", "mrps65", "mrps66", 
                        "mrps67", "mrps68", "mrps69", "mrps610", "mrps611", "mrps612", 
                        "mrps613", "mrps614", "mrps615", "mrps616", "mrps617", "mrps618", 
                        "mrps619", "mrps620")
repw <- c("srwt1", "srwt2", "srwt3", "srwt4", "srwt5", "srwt6", "srwt7", 
          "srwt8", "srwt9", "srwt10", "srwt11", "srwt12", "srwt13", "srwt14", 
          "srwt15", "srwt16", "srwt17", "srwt18", "srwt19", "srwt20", "srwt21", 
          "srwt22", "srwt23", "srwt24", "srwt25", "srwt26", "srwt27", "srwt28", 
          "srwt29", "srwt30", "srwt31", "srwt32", "srwt33", "srwt34", "srwt35", 
          "srwt36", "srwt37", "srwt38", "srwt39", "srwt40", "srwt41", "srwt42", 
          "srwt43", "srwt44", "srwt45", "srwt46", "srwt47", "srwt48", "srwt49", 
          "srwt50", "srwt51", "srwt52", "srwt53", "srwt54", "srwt55", "srwt56", 
          "srwt57", "srwt58", "srwt59", "srwt60", "srwt61", "srwt62")

compPV <-  c('mrpcm1', 'mrpcm2', 'mrpcm3', 'mrpcm4', 'mrpcm5', 'mrpcm6', 'mrpcm7', 'mrpcm8', 'mrpcm9', 'mrpcm10', 'mrpcm11', 'mrpcm12', 'mrpcm13', 'mrpcm14', 'mrpcm15', 'mrpcm16', 'mrpcm17', 'mrpcm18', 'mrpcm19', 'mrpcm20')

# comparing math getA to edsurvey functions
# calculate stats using getA method 
mmat <- augment(data=getData(data=mat, varnames=c("dbapba",num_oper_thetapv,da_stat_prob_thetapv,algebra_thetapv,measurementgeom_thetapv,num_oper_pv,da_stat_prob_pv,algebra_pv,measurementgeom_pv,"origwt", repw, compPV, "repgrp1", "jkunit", "dsex"), omittedLevels =FALSE) ,
                subscaleWeights=c(0.1, 0.25, 0.35, 0.3), 
                DBA_PVs= list(num_oper_thetapv, da_stat_prob_thetapv,algebra_thetapv,measurementgeom_thetapv),# thetas
                PBA_PVs= list(num_oper_pv,da_stat_prob_pv,algebra_pv,measurementgeom_pv),# scaled
                DBAW="origwt",
                PBAW="origwt",
                PBArepWs=repw,
                DBArepWs=repw,
                PVs=compPV,
                StratumVar="repgrp1",
                PSUvar="jkunit") # 

meanGetA <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="MEAN", PVs=compPV, cuts=NULL)
stdevGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="ST DEV", PVs=compPV, cuts=NULL)

cuts <- getAttributes(mat, "pvvars")[["composite"]]$achievementLevel
bscGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="AT BASIC", PVs=compPV, cuts=cuts)
profGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="AT PRFNT", PVs=compPV, cuts=cuts)
bBscGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="< BASIC", PVs=compPV, cuts=cuts)
aBscGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat=">= BASIC", PVs=compPV, cuts=cuts)
aProfGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat=">= PRFNT", PVs=compPV, cuts=cuts)
advGeta <- getA(data=subset(mmat, dsex %in% "Male"), repW=repw, stat="ADVANCED", PVs=compPV, cuts=cuts)
# calculate stats using other edsurvey methods
mathTable <- edsurveyTable(composite_linking ~ dsex, data=mat, jrrIMax=1)
mathSum <- summary2("composite_linking", data=subset(mat, dsex=="Male"))
mathLvls <- achievementLevels(achievementVars = c("composite_linking","dsex"), 
                  data = mat, 
                  weightVar = "origwt", 
                  aggregateBy="dsex",
                  returnCumulative=TRUE)
mathLvlsD <- mathLvls$discrete
mathLvlsC <- mathLvls$cumulative
# comparison between getA and other edsurvey methods 
# check mean
expect_equal(meanGetA$est, mathSum$summary$Mean)
expect_equal(stdevGeta$est, mathSum$summary$SD)
expect_equal(bscGeta$est, mathLvlsD[mathLvlsD$Level == "At Basic" & mathLvlsD$dsex == "Male",]$Percent)
expect_equal(profGeta$est, mathLvlsD[mathLvlsD$Level == "At Proficient" & mathLvlsD$dsex == "Male",]$Percent)
# large disagreement on achievement lvls starting here 
expect_equal(bBscGeta$est, mathLvlsD[mathLvlsD$Level == "Below Basic" & mathLvlsD$dsex == "Male",]$Percent)
expect_equal(aBscGeta$est, mathLvlsC[mathLvlsC$Level == "At or Above Basic" & mathLvlsC$dsex == "Male",]$Percent)
expect_equal(aProfGeta$est, mathLvlsC[mathLvlsC$Level == "At or Above Proficient" & mathLvlsC$dsex == "Male",]$Percent)
expect_equal(advGeta$est, mathLvlsC[mathLvlsC$Level == "At Advanced" & mathLvlsC$dsex == "Male",]$Percent)

# Comparison to STD 
# 1) percentile
context("Checking Percentile")
# 1) percentile
g10 <- gap(variable="composite_linking",
           data=mat,
           groupA= dsex == "Male",
           groupB= dsex == "Female",
           percentiles=c(10, 25, 50, 75, 90),
           jrrIMax=1,
           pctMethod="simple",
           returnSimpleDoF=TRUE,
           noCov = TRUE, # SDT's always use noCov
           includeLinkingError=FALSE) # this is now baked in, regardless

a <- c(104.3505000000, 104.3600000000, 0.7158713750, 0.6855054258, 28.5044382210, 44.4653876479, -0.0095000000, 0.9911556459, 68.0000000000,
       125.9370000000, 124.5530000000, 0.7582515154, 0.6721591391, 50.3958117434, 52.7979592412, 1.3840000000, 1.0132834099, 101.0000000000,
       151.7155000000, 148.8955000000, 0.8182247296, 0.5817374773, 36.6975023642, 42.1271005045, 2.8200000000, 1.0039473097, 68.0000000000,
       177.5095000000, 173.1665000000, 0.6884144348, 0.6414928419, 43.2136984164, 60.3604904221, 4.3430000000, 0.9409715725, 97.0000000000,
       199.4320000000, 193.1755000000, 1.0440354115, 0.8558724512, 25.4986370159, 62.0000000000, 6.2565000000, 1.3500102197, 60.0000000000)
sdt <- data.frame(matrix(a, nrow=5, byrow=TRUE))
# use gap column names to facilitate comparison
colnames(sdt) <- c("estimateA", "estimateB", "estimateAse", "estimateBse", "dofA", "dofB", "diffAB", "diffABse", "dofAB")
# compare relevant columns
g10 <- g10$results[,colnames(sdt)]
expect_equal(g10, sdt)

# with a gap we do estimate (columns G&H), SE (columns I&J), DF (columns K&L), estimate I-J (column M), pooled SE (column N), and pooled DF (column O)

# check on row directly with function
# check on row directly with function
p1 <- percentile(variable="composite",
                 data=subset(mat, dsex=="Male"),
                 percentiles=c(10, 25, 50, 75, 90),
                 jrrIMax=1,
                 pctMethod="simple")

p10 <- percentile(variable="composite_linking",
                  data=subset(mat, dsex=="Male"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")

v1 <- getA(data=subset(mata, dsex %in% "Male"), repW=repw, stat="10TH", PVs=compPV, cuts=NULL)
v2 <- getA(data=subset(mata, dsex %in% "Male"), repW=repw, stat="25TH", PVs=compPV, cuts=NULL)
v3 <- getA(data=subset(mata, dsex %in% "Male"), repW=repw, stat="50TH", PVs=compPV, cuts=NULL)
v4 <- getA(data=subset(mata, dsex %in% "Male"), repW=repw, stat="75TH", PVs=compPV, cuts=NULL)
v5 <- getA(data=subset(mata, dsex %in% "Male"), repW=repw, stat="90TH", PVs=compPV, cuts=NULL)
pctm <- as.data.frame(rbind(v1, v2, v3, v4, v5))
colnames(pctm) <- c("estimateA", "estimateAse", "dofA", "n", "reportable")
pctm <- pctm[, colnames(sdt)[colnames(sdt) %in% colnames(pctm)]]
sdtm <- sdt[, colnames(sdt)[colnames(sdt) %in% colnames(pctm)]]
rownames(stdm) <- NULL
pctm[,1] <- unlist(pctm[,1])
pctm[,2] <- unlist(pctm[,2])
pctm[,3] <- unlist(pctm[,3])
expect_equal(sdtm, pctm)


# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)

comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(104.35050, 125.93700, 151.71550, 177.50950, 199.43200),
                   se = c(0.71587138, 0.75825152, 0.81822473, 0.68841443, 1.04403541),
                   df = c(28.504438, 50.395812, 36.697502, 43.213698, 25.498637))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# percentiles, srace10 
sraceLabs <- c("White, not Hispanic", "Afric Amer, not Hisp", 
               "Hispanic of any race","Asian, not Hispanic", 
               "Amer Ind/Alsk Nat", "Native Ha/Pac Island",
               ">1 race,not Hispanic")
# check on row directly with function, white 
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="White, not Hispanic"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL
p10 <- as.data.frame(p10)

# compare data 
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(116.07900000, 136.82950000, 160.31700000, 182.82350000,200.99250000),
                   se = c(0.61279612, 0.72314093, 0.67171000, 0.80595655,0.84931013),
                   df = c(47.21928948, 49.68055523, 62.00000000, 62.00000000,62.00000000))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# check on row directly with function, black 
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="Afric Amer, not Hisp"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(88.86800000, 106.27200000,126.83800000,149.81400000,170.83700000),
                   se = c(1.42198099, 0.78177443,0.80489616,1.78898532,0.97646704),
                   df = c(53.47948060, 36.32970297,62.00000000,62.00000000,51.37900468))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# check on row directly with function, hispanic 
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="Hispanic of any race"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(96.75550000, 116.03200000,137.58300000,159.28800000,178.23500000),
                   se = c(0.88112410, 0.95596238,0.62729842,0.76930412,0.62725587),
                   df = c(50.30703152, 44.19249411,62.00000000,62.00000000,36.38681629))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# check on row directly with function, asian 
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="Asian, not Hispanic"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(121.58150000, 148.15350000,177.90150000,202.30450000,223.32550000),
                   se = c(1.97011535, 2.32046122,1.63773210,2.67513732,3.81218563),
                   df = c(36.58964763, 44.47219573,22.78129788,5.74589964,19.20448730))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# check on row directly with function, Amer Ind/Alsk Nats 
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="Amer Ind/Alsk Nat"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(96.01000000,115.20550000,136.34150000,157.14600000,174.11950000),
                   se = c(4.04966577,2.36975908,3.16025416,4.59621998,6.10997376),
                   df = c(55.50078235,62.00000000,62.00000000,40.89736359,26.33780175))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)
# check on row directly with function, Native Ha/Pac Island
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10=="Native Ha/Pac Island"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(89.73500000,104.77300000,128.65900000,160.00300000,193.00300000),
                   se = c(31.95858065,5.33026461,12.25377823,23.71872774,6.22013770),
                   df = c(62.00000000,128.65900000,62.00000000,43.24293804,6.82343042))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)

# check on row directly with function, 2 or more
p10 <- percentile(variable="composite_linking",
                  data=subset(mat, srace10==">1 race,not Hispanic"),
                  percentiles=c(10, 25, 50, 75, 90),
                  jrrIMax=1,
                  pctMethod="simple")
# not compared
p10$confInt.ci_lower <- NULL
p10$confInt.ci_upper <- NULL
attributes(p10)$n0 <- NULL
attributes(p10)$nUsed <- NULL
attributes(p10)$call <- NULL

p10 <- as.data.frame(p10)
# compare data
comp <- data.frame(percentile = c(10, 25, 50, 75, 90),
                   estimate =  c(110.52050000,131.92400000,156.45500000,183.09350000,201.92000000),
                   se = c(6.73099889,4.42098548,3.71514097,1.47322403,2.46058369),
                   df = c(62.00000000,62.00000000,62.00000000,44.41233465,52.99337678))
rownames(comp) <- NULL
rownames(p10) <- NULL
expect_equal(p10, comp)




context("Checking Means")
# 2) mean 
# DSEX 
# comparison data 
comp <- data.frame(dsex = c("Male", "Female"),
                   MEAN = c(151.82866405, 148.70816882),
                   SE =  c(0.57375303, 0.41099954))
colnames(comp) <- c("dsex", "MEAN", "SE(MEAN)")
# EdSurvey call 
meanDsex <- edsurveyTable(formula = composite_linking ~ dsex,
                          data = mat,
                          weightVar = 'origwt')
meanDsex <- meanDsex$data[,colnames(comp)]
meanDsex$dsex <- as.character(meanDsex$dsex)
# compare 
expect_equal(comp, meanDsex)
# SRACE10  
# Comparison data 
comp <- data.frame(srace10 = sraceLabs,
                   MEAN = c(159.44413204, 128.35290986, 137.53513926,
                            174.75035641, 136.00398411, 135.35989352,
                            156.77424706),
                   SE =  c(0.53724971, 0.68285158, 0.51643838,
                           1.48943421, 1.92484599, 5.90213217,
                           1.88290755))
colnames(comp) <- c("srace10", "MEAN", "SE(MEAN)")
# EdSurvey call and 
meanSrace <- EdSurvey::edsurveyTable(formula = composite_linking ~ srace10,
                                    data = mat,
                                    weightVar = 'origwt')
meanSrace <- meanSrace$data[,colnames(comp)]
meanSrace$srace10 <- as.character(meanSrace$srace10)
# compare 
expect_equal(comp, meanSrace)




context("Checking Achievement Levels")
# 3) achievment level, discrete (Advanced, at basic, below basic, proficient) 
# 4) achievment level, cumulative (>= prfnt, >= basic)
# DSEX
# make comparison data, Discrete
comp <- data.frame(Level = c("Below Basic","At Basic","At Proficient","At Advanced",
                             "Below Basic","At Basic","At Proficient","At Advanced"),
                   dsex = c(rep("Male",4), rep("Female",4)),
                   Percent = c(39.11648962,34.62545874,22.26956547,3.98848616,
                               41.37338610,36.10456462,20.43262691,2.08942236),
                   StandardError =  c(0.70958815, 0.63305401, 0.58937488,0.34456970, 
                                      0.64292116,0.70868578,0.50783277,0.20375593)
                   )
# Make EdSurvey Data for Discrete and Cumulative 
aLvlsex <- achievementLevels(achievementVars = c("composite_linking","dsex"), 
                              data = mat, 
                              weightVar = "origwt", aggregateBy="dsex",returnCumulative=TRUE)
# Get EdSurvey data, dsex Discrete 
aLvlDsex <- aLvlsex$discrete[,colnames(comp)]
aLvlDsex[,"Level"] <- as.character(aLvlDsex[,"Level"])
aLvlDsex[,"dsex"] <- as.character(aLvlDsex[,"dsex"])
# compare, dsex Discrete 
expect_equal(comp, aLvlDsex)
# make comparison data, dsex Cumulative 
comp <- data.frame(Level = c("At or Above Basic", "At or Above Proficient",
                             "At or Above Basic", "At or Above Proficient"),
                   dsex = c(rep("Male",2), rep("Female",2)),
                   Percent = c(60.88351038,26.25805164,58.62661390,22.52204928),
                   StandardError =  c(0.70958815,0.70673520,0.64292116,0.53744458)
)

# Get EdSurvey Data, dsex Cumulative  
aLvlCsex <- aLvlsex$cumulative[,colnames(comp)]
aLvlCsex[,"Level"] <- as.character(aLvlCsex[,"Level"])
aLvlCsex[,"dsex"] <- as.character(aLvlCsex[,"dsex"])
aLvlCsex <- aLvlCsex[aLvlCsex$Level %in% c("At or Above Proficient","At or Above Basic"),]
attributes(aLvlCsex)$row.names <- c(1:4)
# compare, dsex Cumulative 
expect_equal(comp, aLvlCsex)
# SRACE 
# make comparison data, srace cumulative 
comp <- c(32.06668070, 0.70886246,
          7.62320696, 0.62667357,	
          11.34498639, 0.54400732,
          51.97355051, 1.85711323,
          9.01692312, 1.98158643,	
          16.29755254, 4.96751699,	
          30.97891494, 2.61382705,	
          71.09398079, 0.72671478,	
          33.67076533,1.00684702,	
          45.84447788,0.78575033,	
          79.71184714,	1.32345659,
          44.30046866,	3.23785044,	
          38.73413422,	7.17722288,	
          66.52240173,	2.78179679) 
comp <- data.frame(matrix(comp, nrow=14, byrow=TRUE))
colnames(comp) <- c("Percent", "StandardError")
comp$Level <- c(rep("At or Above Proficient",7), rep("At or Above Basic",7))
comp$srace10 <- rep(c("White, not Hispanic", "Afric Amer, not Hisp", "Hispanic of any race", 
                      "Asian, not Hispanic", "Amer Ind/Alsk Nat", "Native Ha/Pac Island", 
                      ">1 race,not Hispanic"),2)
comp <- comp[order(comp$srace10,comp$Level,decreasing = TRUE),]
attributes(comp)$row.names <- c(1:14)
# Make EdSurvey Data for Discrete and Cumulative 
aLvlsrace <- achievementLevels(achievementVars = c("composite","srace10"), 
                               data = mat, 
                               weightVar = "origwt", aggregateBy="srace10",returnCumulative=TRUE)
# Get EdSurvey Data, srace cumulative 
aLvlCsrace <- aLvlsrace$cumulative[,colnames(comp)]
aLvlCsrace[,"Level"] <- as.character(aLvlCsrace[,"Level"])
aLvlCsrace[,"srace10"] <- as.character(aLvlCsrace[,"srace10"])
aLvlCsrace <- aLvlCsrace[aLvlCsrace$Level %in% c("At or Above Proficient","At or Above Basic"),]
aLvlCsrace <- aLvlCsrace[order(aLvlCsrace$srace10,aLvlCsrace$Level,decreasing = TRUE),]
attributes(aLvlCsrace)$row.names <- c(1:14)
# compare, srace cumulative
expect_equal(comp, aLvlCsrace)
# make comparison data, srace discrete
comp <- c(3.73381579, 0.29775426,
          0.24952029, 0.14008429,	
          0.5191319,  0.12518146,
          14.56118162, 1.83987254,
          0.52872149, NA,
          4.11994488, NA, 
          3.99937326, 1.12447038, 
          28.90601921, 0.72671478,
          66.32923467, 1.00684702,
          54.15552212, 0.78575033, 
          20.28815286, 1.32345659,
          55.69953134, 3.23785044, 
          61.26586578, 7.17722288, 
          33.47759827, 2.78179679,
          28.33286492, 0.66748565,
          7.37368666, 0.62074866, 
          10.82585445, 0.52483702, 
          37.41236889, 1.72495410,
          8.48820163, 1.75586252, 
          12.17760766, 4.24830698, 
          26.97954168, 2.43314195,
          39.02730009, 0.76430106,
          26.04755837, 1.01593312, 
          34.49949150, 0.75286590,
          27.73829663, 1.81401814,
          35.28354554, 3.41780272,
          22.43658167, 6.21384404,
          35.54348680, 2.83476335) 
comp <- data.frame(matrix(comp, nrow=28, byrow=TRUE))
colnames(comp) <- c("Percent", "StandardError")
comp$Level <- c(rep("At Advanced",7), rep("Below Basic",7), 
                rep("At Proficient", 7), rep("At Basic", 7))
comp$srace10 <- rep(c("White, not Hispanic", "Afric Amer, not Hisp", "Hispanic of any race", 
                      "Asian, not Hispanic", "Amer Ind/Alsk Nat", "Native Ha/Pac Island", 
                      ">1 race,not Hispanic"),4)
comp <- comp[order(comp$srace10,comp$Level,decreasing = TRUE),]
attributes(comp)$row.names <- c(1:28)
# Get EdSurvey data, srace discrete 
aLvlDsrace <- aLvlsrace$discrete[,colnames(comp)]
aLvlDsrace[,"Level"] <- as.character(aLvlDsrace[,"Level"])
aLvlDsrace[,"srace10"] <- as.character(aLvlDsrace[,"srace10"])
aLvlDsrace <- aLvlDsrace[order(aLvlDsrace$srace10,aLvlDsrace$Level,decreasing = TRUE),]
attributes(aLvlDsrace)$row.names <- c(1:28)
# compare, srace discrete 
expect_equal(comp, aLvlDsrace)




context("Checking Stdev")
# stdev (summary2)
# dsex 
SDs <- c()
dsexLabs <- c("Male", "Female")
# get standard deviation for each category 
for(i in 1:length(dsexLabs)){
  sex <- dsexLabs[i]
  summary <- summary2("composite_linking", data=subset(mat, dsex==sex))
  SD <- summary$summary$SD
  names(SD) <- sex
  SDs <- c(SDs, SD)  
}
#compare data 
compSDs <-   c(36.68390502,
               34.37200752)
names(compSDs) <- dsexLabs
# compare SDs 
expect_equal(SDs,compSDs)
# SRACE 
SDs <- c()
# get standard deviation for each race 
for(i in 1:length(sraceLabs)){
  race <- sraceLabs[i]
  summary <- summary2("composite_linking", data=subset(mat, srace10==race))
  SD <- summary$summary$SD
  names(SD) <- race
  SDs <- c(SDs, SD)  
}
#compare data 
compSDs <-   c(32.90915684,
               31.87362931,
               31.71137007,
               38.45057805,
               30.56878276,
               38.98224506,
               35.39615859)
names(compSDs) <- sraceLabs
# compare SDs 
expect_equal(SDs,compSDs)
}