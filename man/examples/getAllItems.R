\dontrun{
#TIMSS Example
t15 <- readTIMSS(path="~/TIMSS/2015", "usa", 4)

showPlausibleValues(data=t15) #view constructs in console

#ensure we have all data needed for mml.sdf on light.edsurvey.data.frame
#must be specified ahead of time.  the 'getAllItems' function makes this easy
mathItems <- getAllItems(sdf=t15, construct="mmat") #get mathematics items
sciItems <- getAllItems(sdf=t15, construct="ssci") #get science items
allItems <- getAllItems(sdf=t15, construct=NULL)

wgtVar <- "totwgt"
psustr <- c(getPSUVar(t15, wgtVar), getStratumVar(t15, wgtVar))
lsdf <- getData(data=t15,
                varnames=c("ROWID", "mmat", mathItems, psustr, wgtVar),
                omittedLevels=FALSE,
                addAttributes=TRUE) #builds light.edsurvey.data.frame

#as a light.edsurvey.data.frame all elements must be present
mml.sdf(formula=mmat ~ 1, data=lsdf, weightVar="totwgt")

#as edsurvey.data.frame elements retrieved automatically for user
mml.sdf(formula=mmat ~ 1, data=t15, weightVar="totwgt") 

#NAEP example
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

allItems <- getAllItems(sdf=sdf, construct=NULL)
algebraItems <- getAllItems(sdf=sdf, construct="algebra")
}
