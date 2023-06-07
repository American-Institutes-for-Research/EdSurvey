# reference values for NHES tests (test-11-NHES.R)

dat1Summary.Ref <- c(
  "E ELEMENTARY SCHOOL      H HOMESCHOOLER     M MIDDLE SCHOOL       S SENIOR HIGH ",
  "               6797                 397                3951                6418 "
)

dat2Summary.Ref <- c(
  "                 cpdays          p1sex                 p1mrsta    ",
  " -1 VALID SKIP      :3313   1 MALE  :1699   1 NOW MARRIED  :4454  ",
  " 5 FIVE DAYS A WEEK :1539   2 FEMALE:4145   2 WIDOWED      :  31  ",
  " 3 THREE DAYS A WEEK: 374                   3 DIVORCED     : 259  ",
  " 4 FOUR DAYS A WEEK : 305                   4 SEPARATED    : 127  ",
  " 2 TWO DAYS A WEEK  : 270                   5 NEVER MARRIED: 973  ",
  " 1 ONE DAY A WEEK   :  39                                         ",
  " (Other)            :   4                                         "
)

dat3Summary.Ref <- c(
  "     eehrs               cenreg     ",
  " Min.   : 1.00   1 NORTHEAST: 6839  ",
  " 1st Qu.:35.00   2 SOUTH    :12898  ",
  " Median :40.00   3 MIDWEST  : 8764  ",
  " Mean   :39.01   4 WEST     : 8066  ",
  " 3rd Qu.:45.00                      ",
  " Max.   :80.00                      "
)

dat4Summary.Ref <- c(
  "                 p1rel           p1sex               cenreg    ",
  " 1 BIOLOGICAL PARENT:6739   1 MALE  :2417   1  NORTHEAST:1183  ",
  " 2 ADOPTIVE PARENT  :  83   2 FEMALE:4675   2  SOUTH    :2541  ",
  " 3 STEPPARENT       :  25                   3  MIDWEST  :1650  ",
  " 4 FOSTER PARENT    :  41                   4  WEST     :1718  ",
  " 5 GRANDPARENT      : 167                                      ",
  " 6 OTHER GUARDIAN   :  37                                      "
)


nhes_lm1 <- c(
  "                                (Intercept)                        segrades2 MOSTLY B'S                        segrades3 MOSTLY C'S               segrades4 MOSTLY D'S OR LOWER segrades5 SCHOOL DOES NOT GIVE THESE GRADES ",
  "                                  6.0924827                                  -0.6508287                                  -1.4458851                                  -1.1422074                                  -2.1719469 "
)

nhes_lm2 <- c(
  "                                                             (Intercept)                                      p1educ2 HIGH SCHOOL, BUT NO DIPLOMA                          p1educ3 HIGH SCHOOL DIPLOMA OR EQUIVALENT (GED)                             p1educ4 VOCATIONAL DIPLOMA AFTER HIGH SCHOOL                                      p1educ5 SOME COLLEGE, BUT NO DEGREE                                      p1educ6 ASSOCIATE'S DEGREE (AA, AS) ",
  "                                                               10.060648                                                                 4.471191                                                                 4.570294                                                                 8.767438                                                                 5.849648                                                                 8.703452 ",
  "                                      p1educ7 BACHELOR'S DEGREE (BA, BS)              p1educ8 SOME GRADUATE/PROFESSIONAL EDUCATION, BUT NO DEGREE                                         p1educ9 MASTER'S DEGREE (MA, MS)                                     p1educ10 DOCTORATE DEGREE (PHD, EDD) p1educ11 PROFESSIONAL DEGREE BEYOND BACHELOR'S DEGREE (MD, DDS, JD, LLB) ",
  "                                                               10.004294                                                                 9.484839                                                                14.816251                                                                17.773760                                                                14.381424 "
)

nhes_gap1 <- c(
  "Call: gap(variable = \"xxage\", data = ate2016rc, groupA = wkstatus == ",
  "    \"35 or More Hrs Per Week\", groupB = wkstatus == \"35 or Less Hrs Per Week\", ",
  "    weightVar = \"fawt\")", "", "Labels:", " group                            definition nFullData nUsed",
  "     A wkstatus == \"35 or More Hrs Per Week\"     47744 25925",
  "     B wkstatus == \"35 or Less Hrs Per Week\"     47744 21436",
  "", "Percentage:", "     pctA    pctAse     pctB    pctBse  diffAB       covAB  diffABse diffABpValue    dofAB",
  " 51.69118 0.2687489 47.45516 0.2660952 4.23602 -0.06900543 0.5301353 4.134559e-11 61.94562",
  "", "Results:", " estimateA estimateAse estimateB estimateBse    diffAB        covAB  diffABse diffABpValue    dofAB",
  "  41.94047   0.0890237  41.58566   0.1083014 0.3548094 -0.008560437 0.1917688   0.06781409 83.70246"
)
