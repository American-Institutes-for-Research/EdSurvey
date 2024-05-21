\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# get two variables, without weights
df <- getData(data=sdf, varnames=c("dsex", "b017451"))
table(df)

# example of using recode
df2 <- getData(data=sdf, varnames=c("dsex", "t088301"),
               recode=list(t088301=list(from=c("Yes, available","Yes, I have access"),
                                        to=c("Yes")),
                           t088301=list(from=c("No, have no access"),
                                        to=c("No"))))
table(df2)

# when readNAEP is called on a data file, it appends a default 
# condition to the edsurvey.data.frame. You can see these conditions
# by printing the sdf
sdf

# As per the default condition specified, getData restricts the data to only
# Reporting Sample. This behavior can be changed as follows:
df2 <- getData(data=sdf, varnames=c("dsex", "b017451"), defaultConditions = FALSE)
table(df2)

# similarly, the default behavior of omitting certain levels specified
# in the edsurvey.data.frame can be changed as follows:
df2 <- getData(data=sdf, varnames=c("dsex", "b017451"), omittedLevels = FALSE)
table(df2)

# omittedLevels can also be edited with setAttributes()
# here, the omitted level "Multiple" is removed from the list
sdfIncludeMultiple <- setAttributes(data=sdf, attribute="omittedLevels", value=c(NA, "Omitted"))
# check that it was set
getAttributes(data=sdfIncludeMultiple, attribute="omittedLevels")
# notice that omittedLevels is TRUE, removing NA and "Omitted" still
dfIncludeMultiple <- getData(data=sdfIncludeMultiple, varnames=c("dsex", "b017451"))
table(dfIncludeMultiple)

# the variable "c052601" is from the school-level data file; merging is handled automatically.
# returns a light.edsurvey.data.frame using addAttributes=TRUE argument
gddat <- getData(data=sdf, 
                 varnames=c("composite", "dsex", "b017451","c052601"),
                 addAttributes = TRUE)
class(gddat)
# look at the first few lines
head(gddat)

# get a selection of variables, recode using ifelse, and reappend attributes
# with rebindAttributes so that it can be used with EdSurvey analysis functions
df0 <- getData(data=sdf, varnames=c("composite", "dsex", "b017451", "origwt"))
df0$sex <- ifelse(df0$dsex=="Male", "boy", "girl")
df0 <- rebindAttributes(data=df0, attributeData=sdf)

# getting all the data can use up all the memory and is generally a bad idea
df0 <- getData(data=sdf, varnames=colnames(sdf),
               omittedLevels=FALSE, defaultConditions=FALSE)
}
