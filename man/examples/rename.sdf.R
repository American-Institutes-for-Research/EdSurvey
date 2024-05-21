\dontrun{
usaG4.15 <- readTIMSS(path="~/TIMSS/2015", "usa", 4)
usaG4.15.renamed <- rename.sdf(x=usaG4.15,
                               oldnames=c("itsex", "mmat"),
                               newnames=c("gender", "math_overall"))
lm1 <- lm.sdf(formula=math_overall ~ gender, data = usaG4.15.renamed)
summary(lm1)
}
