\name{linmodel}
\alias{linmodel}
\title{Gottman-Murray Marriage Model with Linear Influence}
\description{
Fit the Gottman-Murray marriage model with a linear influence function.
}
\usage{
linmodel(observations)
}

\arguments{
  \item{observations}{A data frame with two columns, one for the wife (person
    1) and one for the husband (person 2) scores for each unit of time}
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

  The influence function here is a linear function. 

  Parameters are estimated simultaneously using the method described by
  Hamaker, E., Zhang, Z., and Van der Maas, H.L. (See References).

}
\value{
  \code{linmodel} returns a list consisting of the results (the
  parameters fit by the model) for the
  wife (person 1) and the husband (person 2). Each set of results is an
  object of class \code{linmodel}. Although variable names (e.g., a0
  and a1) are the same for each object, their values correspond to the
  model fit to the husband or wife (and are generally different for each
  spouse). Therefore, a0 and a1 for the
  husband's results should be interpreted as b0 and b1 in the equation
  for the husband above. 
  The \code{plot} method for \code{linmodel} objects graphs the partner
  score against the influence and the interpolated influence
  function. For example, for the wife results, this would plot the
  husband score against the influence of the husband on the wife.

  An object of class \code{linmodel} contains the
  following parameters.
  \item{a0}{Initial state}
  \item{a1}{Inertia}
  \item{ls}{Slope for influence function}
  \item{ss}{Sum squared residuals}
  \item{loglik}{Log likelihood assuming equal variance of residuals
    across regimes}
  \item{nparams}{Number parameters, assuming unequal variance of
    residuals across regimes}
  \item{BIC}{Bayesian Information Criterion }
  \item{AIC}{Akaike's Information Criterion }
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
\author{ Tara Madhyastha and Ellen Hamaker}
\seealso{ \code{\link{noinfmodel}}, \code{\link{ojivemodel}}, \code{\link{combimodel}},\code{\link{bilinmodel}}, \code{\link{origmodel}}}

\examples{
require(dyad)
data(couple)
## fit a linear model
fit <- linmodel(couple);
## plot influence function for husband on wife
plot(fit$wife)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ regression}
