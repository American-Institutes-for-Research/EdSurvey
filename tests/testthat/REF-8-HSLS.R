dat1Summary.Ref <- c(
  "     stu_id         x1sex                                        x1race     ", " Min.   :10001   MALE  :11455   WHITE, NON-HISPANIC                 :12082  ",
  " 1st Qu.:16317   FEMALE:11041   HISPANIC, RACE SPECIFIED            : 3375  ", " Median :22598                  BLACK/AFRICAN-AMERICAN, NON-HISPANIC: 2449  ",
  " Mean   :22604                  ASIAN, NON-HISPANIC                 : 1952  ", " 3rd Qu.:28890                  MORE THAN ONE RACE, NON-HISPANIC    : 1941  ",
  " Max.   :35206                  HISPANIC, NO RACE SPECIFIED         :  422  ", "                                (Other)                             :  275  "
)


dat2Summary.Ref <- c(
  "     sch_id          x1region     x1locale  ",
  " Min.   :1001   NORTHEAST:149   CITY  :272  ",
  " 1st Qu.:1237   MIDWEST  :251   SUBURB:335  ",
  " Median :1472   SOUTH    :380   TOWN  :117  ",
  " Mean   :1472   WEST     :164   RURAL :220  ",
  " 3rd Qu.:1708                               ",
  " Max.   :1944                               "
)

dat3Summary.Ref <- c(
  "     stu_id          x1ses              x2ses         ", " Min.   :10001   Min.   :-1.93020   Min.   :-1.75010  ", " 1st Qu.:16321   1st Qu.:-0.48278   1st Qu.:-0.46542  ",
  " Median :22586   Median : 0.01525   Median : 0.04565  ", " Mean   :22586   Mean   : 0.07923   Mean   : 0.09896  ", " 3rd Qu.:28863   3rd Qu.: 0.59380   3rd Qu.: 0.64345  ",
  " Max.   :35206   Max.   : 2.88070   Max.   : 2.28240  "
)


wgtNames.Ref <- c(
  "w1student", "w1parent", "w1mathtch", "w1scitch",
  "w2student", "w2w1stu", "w2parent", "w2w1par",
  "w3student", "w3w1stu", "w3w1w2stu", "w3w2stu", "w3hstrans", "w3studenttr", "w3w1stutr", "w3w1w2stutr", "w3w2stutr",
  "w4student", "w4w1stu", "w4w1stup1", "w4w1stup1p2", "w4w1w2w3stu", "w3w1mathtch", "w3w1scitch"
)
