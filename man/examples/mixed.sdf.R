\dontrun{
# save TIMSS 2015 data to ~/TIMSS/2015
downloadTIMSS(root="~/", years=2015)
fin <- readTIMSS(path="~/TIMSS/2015", countries="fin", gradeLvl=4)
# uses all plausible values
mix1 <- mixed.sdf(formula=mmat ~ itsex + (1|idschool), data = fin,
                  weightVar=c("totwgt","schwgt"), weightTransformation=FALSE)
summary(mix1)
# uses only one plausible value
mix2 <- mixed.sdf(formula=asmmat01 ~ itsex + (1|idschool), data = fin,
                  weightVar=c("totwgt","schwgt"), weightTransformation=FALSE)
summary(mix2)
}
