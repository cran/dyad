\name{plot.ojivemodel}
\alias{plot.ojivemodel}
\title{Plot Influence for Ojive Model}
\description{
 Plot influence and fitted function for an object returned by \code{\link{ojivemodel}}.
}
\usage{
plot.ojivemodel(x, xlab = "Score", ylab = "Influence", xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class \code{ojivemodel} }
  \item{xlab}{a title for the x axis}
  \item{ylab}{a title for the y axis}
  \item{xlim}{the x limits (x1, x2) of the plot (by default, calculated
    from the score data)}
  \item{ylim}{the y limits of the plot (by default, calculated from the
    influence data)}
  \item{\dots}{ additional arguments to \code{\link{plot.default}}}
}
\author{Tara Madhyastha }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
