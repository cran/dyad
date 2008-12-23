\name{plot.combimodel}
\alias{plot.combimodel}
\title{Plot Influence for Combi Model}
\description{
Plot influence and fitted function for an object returned by \code{\link{combimodel}}
}
\usage{
plot.combimodel(x, xlab = "Score", ylab = "Influence", xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Object of class \code{combimodel} }
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
