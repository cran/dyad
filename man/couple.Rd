\name{couple}
\alias{couple}
\docType{data}
\title{Sample Couple Data}
\description{
This is an example of data from a 15 minute couple conflict discussion,
as used in studies of marriage by John Gottman and others. The data are
coded according to the Specific Affect Coding System (SPAFF) version
4.0. They are discretized by second, weighted according to the
positivity or negativity of the code, from +4 to -4, where neutral has a
value of .1 (just slightly positive). Then the sequence is 
aggregated by 6-second windows to produce 150 data points ranging from
-24 to 24 for both the husband and the wife. 
}
\usage{data(couple)}
\format{
  A data frame with 150 observations on the following 2 variables.
  \describe{
    \item{\code{wife}}{a numeric vector}
    \item{\code{husband}}{a numeric vector}
  }
}

\source{
   Gottman, J. M., Murray, J. D., Swanson, C., Tyson, R., & Swanson, K. R. (2003). \emph{The Mathematics of Marriage: Dynamic Nonlinear Models.} The MIT Press.
}

\examples{
data(couple)
fit <- bilinmodel(couple)
plot(fit$wife)
}
\keyword{datasets}
