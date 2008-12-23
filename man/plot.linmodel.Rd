\name{plot.linmodel}
\alias{plot.linmodel}

\title{ Plot Influence for Linear Model}
\description{
Plot influence and fitted function for an object returned by \code{\link{linmodel}}.

}
\usage{
plot.linmodel(x, xlim = NULL, ylim = NULL, xlab = "Score", ylab = "Influence", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Object of class \code{bilinmodel}}
  \item{xlim}{the x limits (x1, x2) of the plot (by default, calculated
    from the score data)}
  \item{ylim}{the y limits of the plot (by default, calculated from the
    influence data)}
  \item{xlab}{ a title for the x axis }
  \item{ylab}{ a title for the y axis }
  \item{\dots}{ additional arguments to \code{\link{plot.default}}.}
}
\author{Tara Madhyastha}
\keyword{hplot}
