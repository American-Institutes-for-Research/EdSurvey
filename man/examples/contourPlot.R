\dontrun{
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
lm1 <- lm.sdf(formula=composite ~ pared * dsex + sdracem, data=sdf)
# plot the results
contourPlot(x=lm1$fitted.values,
	          y=lm1$residuals[,1], # use only the first plausible value
	          m=30,
	          xlab="fitted values",
	          ylab="residuals",
	          main="Figure 1")
# add a line indicating where the residual is zero
abline(0,0)
}
