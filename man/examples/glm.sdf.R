\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# by default uses the jackknife variance method using replicate weights
table(sdf$b013801)

# create a binary variable for 26 or more books
sdf$b013801_26more <- ifelse(sdf$b013801 %in% c("26-100", ">100"), yes = 1, no = 0)

# compare the multiple categorical and binary variable for accuracy
table(sdf$b013801, sdf$b013801_26more) 

logit1 <- logit.sdf(formula=b013801_26more ~ dsex + b017451, data=sdf)

# use summary to get detailed results
summary(logit1)

# Taylor series variance estimation
logit1t <- logit.sdf(formula=b013801_26more ~ dsex + b017451, data=sdf,
                     varMethod="Taylor")
summary(logit1t)

# when using ifelse for PVs, use the ifelse in the formula call. PVs contains multiple variables
logit2 <- logit.sdf(formula=ifelse(composite >= 300, yes = 1, no = 0) ~ dsex + b013801, data=sdf)

summary(logit2)

# note this recoding of composite must be done in the formula
logit3 <- glm.sdf(formula=I(composite >= 300) ~ dsex + b013801, data=sdf, 
                  family=quasibinomial(link="logit"))

# Wald test for joint hypothesis that all coefficients in b013801 are zero
waldTest(model=logit3, coefficients="b013801")

summary(logit3)

# use plausible values as predictors in a generalized linear regression model
# ifelse function converts the selected categories to 1 and all the others including
# Multiple and Omitted levels to 0
sdf$AlgebraClass <- ifelse(sdf$m815701 %in% c('Algebra I (1-yr crs)',
                                              '1st yr 2-yr Algeb I',
                                              '2nd yr 2-yr Algeb I',
                                              'Algebra II'), 1, 0)

table(sdf$m815701, sdf$AlgebraClass) 

logit4 <- logit.sdf(formula = AlgebraClass ~ algebra,
                    weightVar = 'origwt', data = sdf)

summary(logit4)

# alternatively, same analyses can be executed using the I() function with
# dropOmittedLevels = FALSE
logit5 <- logit.sdf(I(m815701 %in% c('Algebra I (1-yr crs)', 
                      '1st yr 2-yr Algeb I', '2nd yr 2-yr Algeb I', 
                      'Algebra II')) ~ algebra, 
                      weightVar = 'origwt', data = sdf, 
                      dropOmittedLevels = FALSE)
summary(logit5)
}
