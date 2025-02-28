\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(path=system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# create a table with composite scores by dsex and b017451
est1 <- edsurveyTable(formula=composite ~ dsex + b017451, data=sdf)

# create a table with csv output
edsurveyTable2pdf(data=est1, 
                  formula=b017451~dsex, 
                  toCSV="C:/example table.csv",
                  filename="C:/example table.pdf",
                  returnMeans=FALSE)

# create a pdf file using the default subject scale or subscale
# and keep two digits for estimates and three digits for SE after decimal point
edsurveyTable2pdf(data=est1, formula=b017451~dsex, 
                  returnMeans=TRUE, estDigits=2, seDigits=3)

# create a pdf file using the percentage of students at the 
# aggregation level specified by \code{pctAggregationLevel}
# output will be saved as "C:/example table.pdf"
edsurveyTable2pdf(data=est1, 
                  formula=b017451~dsex, 
                  filename="C:/example table.pdf",
                  returnMeans=FALSE)
}
