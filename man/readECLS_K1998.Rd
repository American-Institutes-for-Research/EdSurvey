% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readECLS_K1998.R
\name{readECLS_K1998}
\alias{readECLS_K1998}
\title{Connect to ECLS--K 1998 Data}
\usage{
readECLS_K1998(
  path = getwd(),
  filename = "eclsk_98_99_k8_child_v1_0.dat",
  layoutFilename = "Layout_k8_child.txt",
  forceReread = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{path}{a character value to the full directory path(s) to the
ECLS--K-extracted fixed-width-format (.dat) set of data files}

\item{filename}{a character value of the name of the fixed-width (.dat)
data file in the specified \code{path} to be read}

\item{layoutFilename}{a character value of the filename of either the ASCII
(.txt) layout file of the \code{filename} within
the specified \code{path}
or a character value of the  filename of the SPSS syntax (.sps) layout file of the \code{filename} within the specified \code{path}}

\item{forceReread}{a logical value to force rereading of all processed data.
The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.}

\item{verbose}{a logical value that will determine if you want verbose output while the \code{readECLS-K1998} function is running to indicate processing progress.
The default value is \code{TRUE}.}
}
\value{
an \code{edsurvey.data.frame} for the ECLS--K 1998 longitudinal dataset
}
\description{
Opens a connection to an ECLS--K 1998 data file and
             returns an \code{edsurvey.data.frame} with
             information about the file and data.
}
\details{
Reads in the unzipped files downloaded from the ECLS--K 1998 longitudinal dataset(s) to an \code{edsurvey.data.frame}.  The ECLS--K 1998--99 study consisted of
         three distinct separate datasets that cannot be combined: (1) Child Grades K--8 Data, (2) School Base-Year Data, and (3) Teacher Base-Year Data.
         The \code{filename} and \code{layoutFilename} arguments default to the corresponding Child K--8 default filenames.
}
\examples{
\dontrun{
# read-in student file with defaults
eclsk_df <- readECLS_K1998(path="~/ECLS_K/1998") #using defaults
d <- getData(data=eclsk_df, varnames=c("childid", "gender", "race"))
summary(d)
}

\dontrun{
# read-in with parameters specified
eclsk_df <- readECLS_K1998(path = "~/ECLS_K/1998", 
                           filename = "eclsk_98_99_k8_child_v1_0.dat", 
                           layoutFilename = "Layout_k8_child.txt", 
                           verbose = TRUE, 
                           forceReread = FALSE)
}
}
\seealso{
\code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, \code{\link{getData}}, \code{\link{downloadECLS_K}}
}
\author{
Tom Fink
}
