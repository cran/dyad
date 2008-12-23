\name{bilinmodel}
\alias{bilinmodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gottman-Murray Marriage Model with Bilinear Influence}
\description{
Fit the Gottman-Murray marriage model with the bilinear influence function, optimizing the threshold.
}
\usage{
bilinmodel(observations, mpr = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{A data frame with two columns, one for the wife (person
    1) and one for the husband (person 2) scores for each unit of time}
  \item{mpr}{Minimum number of observations per regime. If not specified, set to 10\% of observations. }
}
\details{
  This function fits the following Gottman-Murray model of marriage,
  where input is a series of discrete observations of the  wife (\eqn{W})
  and the husband (\eqn{H}). The same model can apply to single-sex couples,
  in which case the wife should be interpreted as partner 1 and the husband as
  partner 2.

  The Gottman-Murray equations of marriage are written as follows.

  \deqn{W_{t+1}= a_{0} + a_{1}W_{t} + I_{HW}(H_t)}{W(t+1) = a0 + a1*W(t)
    + IHW(H(t))}

  \deqn{H_{t+1}= b_{0} + b_{1}H_{t} + I_{WH}(W_t)}{H(t+1) = b0 + b1*H(t)
    + IWH(H(t))}
  
  In these equations, one partner exerts influence on the other as a
  function of the previous timestep, denoted by the function
  \eqn{I_{HW}}{IHW} (influence of the husband on the wife) or \eqn{I_{WH}}{IWH}
  (influence of the wife on the husband). 

  The influence function here is a bilinear function. This function
  consists of two linear pieces, which we refer to as the negative and
  positive regime, while the influence is zero at the threshold between
  regimes. If we  plot \eqn{H_t}{H(t)} against \eqn{I_{HW}}{IHW}, we adjust the threshold of \eqn{H_t}{H(t)} to obtain the best least squares fit to both lines. 

  Parameters are estimated simultaneously using the method described by
  Hamaker, E., Zhang, Z., and Van der Maas, H.L. (See References).
}
\value{
  \code{bilinmodel} returns a list consisting of the results (the
  parameters fit by the model) for the
  wife (person 1) and the husband (person 2), corresponding to the
  solution to first and the second Gottman-Murray equations of marriage
  above. Each set of results is an
  object of class \code{bilinmodel}. Although variable names (e.g., a0
  and a1) are the same for each object, their values correspond to the
  model fit to the husband or wife (and are generally different for each
  spouse). Therefore, a0 and a1 for the
  husband's results should be interpreted as b0 and b1 in the equation
  for the husband above. 
  
  The \code{plot} method for \code{bilinmodel} objects graphs the partner
  score against the influenced component and the interpolated influence
  function. For example, for the wife results, this would plot the
  husband score against the influence of the husband on the wife.
  
  An object of class \code{bilinmodel} contains the
  following parameters.
  \item{a0}{Initial state}
  \item{a1}{Inertia}
  \item{ls}{Slope for influence function left of the threshold}
  \item{rs}{Slope for influence function right of the threshold}
  \item{th}{Threshold}
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
  \item{score}{Vector of partner data (from 1 to nt-1)}
  \item{influence}{Vector of influence, calculated using a0 and a1 above}
}
\references{ 
For a general description of the marriage model and influence functions, see
   Gottman, J. M., Murray, J. D., Swanson, C., Tyson, R., & Swanson, K. R. (2003). The Mathematics of Marriage: Dynamic Nonlinear Models. The MIT Press.

The method of parameter estimation used here is described in
   Hamaker, E., Zhang, Z., Van der Maas, H.L. Using threshold autoregressive models to study dyadic interactions. Psychometrika, in press.
}
\author{Tara Madhyastha and Ellen Hamaker}

\seealso{ \code{\link{noinfmodel}}, \code{\link{linmodel}}, \code{\link{ojivemodel}}, \code{\link{combimodel}}, \code{\link{origmodel}}}
\examples{
require(dyad)
data(couple)
## fit a bilinear model
fit <- bilinmodel(couple);
## plot influence function for husband on wife
plot(fit$wife)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ nonlinear }
