\dontrun{
# filepath argument will vary by operating system conventions
usaG4.15 <- readTIMSS("~/TIMSS/2015", "usa", 4)
d <- getData(usaG4.15, "itsex")
summary(d) #show details: MALE/FEMALE
usaG4.15 <- recode.sdf(usaG4.15,
                       recode = list(itsex=list(from=c("MALE"),
                                                to=c("BOY")),
                                     itsex=list(from=c("FEMALE"),
                                                to=c("GIRL"))))

d <- getData(usaG4.15, "itsex") #apply recode
summary(d) #show details: BOY/GIRL
}
