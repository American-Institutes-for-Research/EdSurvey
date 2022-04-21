
ssocs_ref1 <- c("                             c0014_r         c0526             c0528      ", 
                " Principal                       :1619   Min.   :  0.000   Min.   : 0.00  ", 
                " Vice-principal or disciplinarian: 351   1st Qu.:  1.000   1st Qu.:10.00  ", 
                " Security staff                  :  20   Median :  4.000   Median :12.00  ", 
                " Other school-level staff        :  72   Mean   :  9.498   Mean   :13.86  ", 
                " Superintendent or district staff:   7   3rd Qu.: 12.000   3rd Qu.:17.00  ", 
                "                                         Max.   :100.000   Max.   :96.00  ")

ssocs_ref2 <- c(" c0134                 c0198         c0534       ", 
                " Yes: 441   0-25%         :372   Min.   :  0.00  ", 
                " No :2321   26-50%        :653   1st Qu.: 50.00  ", 
                "            51-75%        :710   Median : 68.00  ", 
                "            76-100%       :854   Mean   : 62.69  ", 
                "            Does not offer:173   3rd Qu.: 80.00  ", 
                "                                 Max.   :100.00  ")

ssocs_refTbl1 <- c("", "Formula: vioinc16 ~ c0600 ", 
                   "", 
                   "Weight variable: 'finalwgt'", 
                   "Variance method: jackknife", 
                   "JK replicates: 50", 
                   "full data n: 2092", 
                   "n used: 2092", 
                   "", 
                   "", 
                   "Summary Table:", 
                   " c0600    N    WTD_N      PCT  SE(PCT)      MEAN  SE(MEAN)", 
                   "   Yes  996 34762.08 41.58557 1.557152 11.508763 0.6838331", 
                   "    No 1096 48829.60 58.41443 1.557152  9.518731 0.7048793")

ssocs_refTbl2 <- c("", "Formula: ROWID ~ c0610 + c0134 ", 
                   "", 
                   "Weight variable: 'finalwgt'", 
                   "Variance method: jackknife", 
                   "JK replicates: 50", 
                   "full data n: 2092", 
                   "n used: 2092", 
                   "", 
                   "", 
                   "Summary Table:", 
                   " c0610 c0134    N     WTD_N      PCT  SE(PCT)", 
                   "   Yes   Yes  224  8216.959 20.58818 1.668273", 
                   "   Yes    No 1136 31694.097 79.41182 1.668273", 
                   "    No   Yes  132  9735.372 22.28762 2.021483", 
                   "    No    No  600 33945.256 77.71238 2.021483")

ssocs_refTbl3 <- c("", "Formula: incid18 ~ c0669 + c0560 ", 
                   "", 
                   "Weight variable: 'finalwgt'", 
                   "Variance method: jackknife", 
                   "JK replicates: 50", 
                   "full data n: 2762", 
                   "n used: 1131", 
                   "", 
                   "", 
                   "Summary Table:", 
                   " c0669                                                        c0560   N      WTD_N       PCT  SE(PCT)      MEAN SE(MEAN)", 
                   "   Yes                                          High level of crime 102  2977.7645 10.772062 1.520821 40.741138 3.735516", 
                   "   Yes                                      Moderate level of crime 231  6400.6924 23.154502 1.880035 31.575244 4.105128", 
                   "   Yes                                           Low level of crime 536 15036.3585 54.394021 2.261935 14.311370 1.147264", 
                   "   Yes Students come from areas with very different levels of crime 150  3228.5879 11.679415 1.143869 25.331130 2.914940", 
                   "    No                                          High level of crime   5   180.0330  4.634943 2.516350 26.108075 8.521599", 
                   "    No                                      Moderate level of crime  31   953.3728 24.544545 5.296237 18.111184 4.049076", 
                   "    No                                           Low level of crime  60  1928.6608 49.653296 6.109887 11.365244 1.800395", 
                   "    No Students come from areas with very different levels of crime  16   822.1887 21.167216 5.824951  6.252847 1.828095")


ssocs_refCorr1 <- c("Method: Pearson", 
                    "full data n: 2762", 
                    "n used: 2762", 
                    "", 
                    "Correlation: 0.4155861", 
                    "Standard Error: 0.0427424", 
                    "Confidence Interval: [0.3180694, 0.5044011]")

ssocs_refLM1 <- c("(Intercept)       c0532    vioinc16 ", 
                  "0.446830271 0.009463642 0.018524504 ")

ssocs_refLogit1 <- c(" (Intercept)        c0532 ", 
                     "-4.509334913 -0.002386365 ")


ssocs_refWald <- c("Wald test:", "----------",
                   "H0:",
                   "c0532 = 0",
                   "",
                   "Chi-square test:",
                   "X2 = 0.14, df = 1, P(> X2) = 0.71",
                   "",
                   "F test:",
                   "W = 0.14, df1 = 1, df2 = 60, P(> W) = 0.71")

