\name{combimodel}
\alias{combimodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Gottman-Murray Marriage Model with Combination of Bilinear and
  Ojive Influence}
\description{
  Fit the Gottman-Murray marriage model using a combination of ojive and
  bilinear influence, optimizing the threshold.
}
\usage{
combimodel(observations, mpr = NA, nregime = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{A data frame with two columns, one for the wife (person
    1) and one for the husband (person 2) scores for each unit of time}
  \item{nregime}{Number of regimes (either 2 or 3). If 2 regimes are
    specified, there is a negative and positive regime. If 3 regimes are
    specified, there is a negative, neutral, and positive
    regime. Default is 2.}
  \item{mpr}{Minimum number of observations per regime. If not specified, set to 10\% of observations. }
}
\details{
  This function fits the following Gottman-Murray model of marriage,
  where input is a series of discrete observations of the  wife (\eqn{W})
  and the husband (\eqn{H}). The same model can apply to single-sex couples,
  where the wife should be interpreted as partner 1 and the husband as
  partner 2. 

  \deqn{W_{t+1}= a_{0} + a_{1}W_{t} + I_{HW}(H_t)}{W(t+1) = a0 + a1*W(t)
    + IHW(H(t))}

  \deqn{H_{t+1}= b_{0} + b_{1}H_{t} + I_{WH}(W_t)}{H(t+1) = b0 + b1*H(t)
    + IWH(H(t))}

  In these equations, one partner exerts influence on the other as a
  function of the previous timestep, denoted by the influence functions
  \eqn{I_{HW}}{IHW} (influence of husband on wife) or \eqn{I_{WH}}{IWH} (influence
  of wife on husband). 

  The influence function here is a combination of an ojive influence
  function (see \code{\link{ojivemodel}} and a bilinear influence
  function \code{\link{bilinmodel}}. Therefore, the model allows for
  large changes between regimes, as with the ojive model, and gradual
  variation in influence within a regime, as in the bilinear influence model.

}
\value{
  \code{combimodel} returns a list consisting of the results (the
  parameters fit by the model) for the
  wife (person 1) and the husband (person 2). Each set of results is an
  object of class \code{combimodel}.  Although variable names (e.g., a0
  and a1) are the same for each object, their values correspond to the
  model fit to the husband or wife (and are generally different for each
  spouse). Therefore, a0 and a1 for the
  husband's results should be interpreted as b0 and b1 in the equation
  for the husband above. 
  The \code{plot} method for \code{combimodel} objects graphs the
  partner score against the influence and the interpolated influence
  function. For example, for the wife results, this would plot the
  husband score against the influence of the husband on the wife.
 
  An object of class \code{combimodel} contains the
  following parameters, depending on the number of regimes specified.

  \item{a0}{Initial state}
  \item{a1}{Inertia}
  \item{l1}{Difference in constant between regime 1 and regime 2}
  \item{l2}{Difference in constant between regime 2 and regime 3 (3
    regime only)}
  
  \item{s1}{Slope in regime 1}
  \item{s2}{Slope in regime 2}
  \item{s3}{Slope in regime 3 (3 regime only)}
  \item{th}{Threshold between regime 1 and regime 2 (2 regime only)}
  \item{nth}{Threshold between regime 1 and regime 2 (3 regime only)}
  \item{pth}{Threshold between regime 1 and regime 2 (3 regime only)}
  \item{ss}{Sum squared residuals}
  \item{loglik}{Log likelihood assuming equal variance of residuals
    across regimes}
  \item{nparams}{Number parameters, assuming unequal variance of
    residuals across regimes}
  \item{BICeq}{Bayesian Information Criterion calculated assuming equal
    variance of residuals across regimes}
  \item{BICneq}{Bayesian Information Criterion calculated assuming unequal
    variance of residuals across regimes}
  \item{AICeq}{Akaike's Information Criterion calculated assuming equal
    variance of residuals across regimes}
  \item{AICneq}{Akaike's Information Criterion calculated assuming unequal
    variance of residuals across regimes}
  \item{nt}{Number of observations}
  \item{nregime}{Number of regimes (2 or 3)}
  \item{score}{Vector of partner data (from 1 to nt-1)}
  \item{influence}{Vector of influence, calculated using a0 and a1 above}
}
\references{
For a general description of the marriage model and influence functions, see
   Gottman, J. M., Murray, J. D., Swanson, C., Tyson, R., & Swanson, K. R. (2003). \emph{The Mathematics of Marriage: Dynamic Nonlinear Models}. The MIT Press.

The method of parameter estimation used here is described in
   Hamaker, E., Zhang, Z., Van der Maas, H.L. Using threshold autoregressive models to study dyadic interactions. Psychometrika, in press.
}
\author{Tara Madhyastha and Ellen Hamaker}
\seealso{ \code{\link{bilinmodel}}, \code{\link{ojivemodel}}, \code{\link{origmodel}}}

\examples{
require(dyad)
data(couple)
## fit a combination model with 3 regimes
fit <- combimodel(couple, nregime=3);
## plot influence function for wife on husband
plot(fit$husband)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ nonlinear }
