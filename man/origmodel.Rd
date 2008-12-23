\name{origmodel}
\alias{origmodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Original Gottman-Murray Marriage Model}
\description{
  This is an R adaptation of the original FORTRAN marriage model
  described in ``The Mathematics of Marriage". The code uses a two-phased
  method of parameter estimation that has been superceded by
  simultaneous parameter estimation procedures implemented in
  \code{\link{bilinmodel}}, \code{\link{ojivemodel}} and \code{\link{combimodel}}. These
  functions extend and generalize the influence functions. 
}

\usage{
origmodel(observations, inf = "bilin")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{A data frame with two columns, the first for the wife (person
    1) and the second for the husband (person 2) scores for each unit of
    time. It is expected that these are in the range -24 to 24, and that
  the data frame has 150 observations.}
  \item{inf}{The influence is one of ``bilin" for bilinear influence,
    ``ojive" for ojive influence, or ``rd" for repair and damping.}
}
\details{
This model is designed to work with input from John
Gottman's conflict discussion protocol. In this protocol, a topic of
disagreement is identified and couples are instructed to discuss this
topic for 15 minutes. They are videotaped during this discussion. The
discussion is then coded according to the Specific Affect Coding System
(SPAFF), version 4.0. These codes are discretized so that each second is
described by a single code, the one that occurs for the longest time
within the second. The codes are then weighted, producing a range of
values ranging from -4 (very negative) to 4 (very positive) where
neutral has a value of .1 (just slightly positive). This produces a
sequence of 900 data points for both the husband and the wife.

After coding, the sequence is aggregated by 6-second intervals to
produce 150 data points ranging from -24 to 24 for both the husband and
the wife. This program expects a data frame consisting of these two
sequences (wife and husband, in that order).

This model uses a two phase algorithm to fit the data. First, the
weighted SPAFF 6-second sums are converted so that any value <= .6 is converted to zero. This means that any neutral window (which may have a
weight of up to .6) is converted to a weight of zero. Then the remaining
values are rounded to the nearest integer value. 

Then the uninfluenced parameters are estimated using the fraction of the
150 windows that have value zero. These parameters are factored out,
leaving the residuals that correspond to the influence, and these are
fit to an influence function. When the influence selected is a
repair/damping model, only those residuals in the upper right and lower
left quadrants are used to fit the bilinear influence function and then
the repair/damping terms. Therefore, slopes calculated with the bilinear
influence function alone and in the context of a repair/damping model
are different. 

This code was translated from the original FORTRAN version with minimal
changes to the program logic.  Graphing functions are not yet
implemented. There is little error checking. 
}

\value{
  \code{origmodel} returns a list consisting of the following
  parameters.
  
  \item{r1 }{Wife emotional inertia}
  \item{a}{Wife initial state}
  \item{natw}{Wife uninfluenced steady state}
  \item{wmin}{Minimum wife score}
  \item{wmax}{Maximum wife score}
  \item{r2 }{Husband emotional inertia}
  \item{b}{Husband initial state}
  \item{nath}{Husband uninfluenced steady state}
  \item{wmin}{Minimum husband score}
  \item{wmax}{Maximum husband score}
  \item{hsetpoints}{Husband setpoints}
  \item{wsetpoints}{Wife setpoints}
    \item{inf}{Influence function specified in the function call
      (``bilin", ``ojive" or ``rd")}
    
  For bilinear model (``bilin")
  \item{rw}{Wife influence on husband for positive affect (slope)}
  \item{lw}{Wife influence on husband for negative affect (slope)}
  \item{rh}{Husband influence on wife for positive affect (slope)}
  \item{lh}{Husband influence on wife for negative affect (slope)}
  
  For ojive model (``ojive")
  \item{ah}{Wife influence on husband for positive affect below threshold}
  \item{bh}{Wife influence on husband for positive affect above threshold}
  \item{ch}{Threshold of wife influence on husband for positive affect}
  \item{dh}{Wife influence on husband for negative affect below threshold}
  \item{eh}{Wife influence on husband for negative affect above threshold}
  \item{fh}{Threshold of wife influence on husband for negative affect}
  \item{aw}{Husband influence on wife for positive affect below threshold}
  \item{bw}{Husband influence on wife for positive affect above threshold}
  \item{cw}{Threshold of husband influence on wife for positive affect}
  \item{dw}{Husband influence on wife for negative affect below threshold}
  \item{ew}{Husband influence on wife for negative affect above threshold}
  \item{fw}{Threshold of husband influence on wife for negative affect}
  \item{hsetpoints}{Husband setpoints, indexed by quadrant}
  \item{wsetpoints}{Wife setpoints, indexed by quadrant}

  For repair/damping model (``rd")
  \item{kdw}{Husband damping  of wife's influence(w score trigger)}
  \item{cdh}{Husband damping of wife's influence (h effectiveness)}
  \item{krw}{Husband repair of wife's influence (w score trigger)}
  \item{crh}{Husband repair of wife's influence (h effectiveness)}
  \item{kdh}{Wife damping of husband's influence (h score trigger)}
  \item{cdw}{Wife damping of husband's influence (w effectiveness)}
  \item{krh}{Wife repair of husband's influence (h score trigger)}
  \item{crw}{Wife repair of husband's influence (w effectiveness)}
  \item{setpts}{Strongest stable setpoint in each quadrant}
  \item{allsetpts}{Stable and unstable steady states in each quadrant}

  Note that quadrants are numbered counterclockwise from upper left
  on a plot where husband score is on the x-axis and wife influence is
  on the y-axis: Quadrant 1 is H<0, W>0, Quadrant 2 is H<0, W>0,
  Quadrant 3 is H>0, W>0, and Quadrant 4 is H>0, W<0.
}

\references{
  For description of expected input, the marriage model and influence functions implemented by this function, see
   Gottman, J. M., Murray, J. D., Swanson, C., Tyson, R., & Swanson, K. R. (2003). \emph{The Mathematics of Marriage: Dynamic Nonlinear Models}. The MIT Press.
 }
\author{Tara Madhyastha (translated from the original FORTRAN version
  developed and written by Catherine C. Swanson)}

\seealso{\code{\link{noinfmodel}}, \code{\link{linmodel}}, \code{\link{bilinmodel}},\code{\link{ojivemodel}},\code{\link{combimodel}}}
\examples{
require(dyad)
data(couple)
## fit a bilinear model
fit <- origmodel(couple, inf="bilin")
}


\keyword{ models }
\keyword{ nonlinear }
