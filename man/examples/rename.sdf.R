\dontrun{
usaG4.15 <- readTIMSS("~/TIMSS/2015", "usa", 4)
usaG4.15.renamed <- rename.sdf(usaG4.15,
                               c("itsex", "mmat"),
                               c("gender", "math_overall"))
lm1 <- lm.sdf(math_overall ~ gender, data = usaG4.15.renamed)
summary(lm1)
}
