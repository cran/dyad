\name{noinfmodel}
\alias{noinfmodel}
\title{Gottman-Murray Marriage model with No Influence}
\description{
Fit an autoregressive marriage model to husband and wife, with no influence.
}
\usage{
noinfmodel(observations)
}
\arguments{
  \item{observations}{A data frame with two columns, one for the wife (person
    1) and one for the husband (person 2) scores for each unit of time}
}
\details{
   This function fits a model of marriage where input is a series of discrete observations of the  wife (\eqn{W}) and the husband (\eqn{H}). The same model can apply to single-sex couples,
  in which case the wife should be interpreted as partner 1 and the husband as
  partner 2.

  The model is as follows:

  \deqn{W_{t+1}= a_{0} + a_{1}W_{t}}{W(t+1) = a0 + a1*W(t)}

  \deqn{H_{t+1}= b_{0} + b_{1}H_{t}}{H(t+1) = b0 + b1*H(t)}  
}
\value{
  \code{noinfmodel} returns a list consisting of the results (the
  parameters fit by the model) for the wife
  (person 1) and the husband (person 2). Each set of results is an object of class
  \code{noinfmodel}.  Although variable names (e.g., a0
  and a1) are the same for each object, their values correspond to the
  model fit to the husband or wife (and are generally different for each
  spouse). Therefore, a0 and a1 for the
  husband's results should be interpreted as b0 and b1 in the equation
  for the husband above. 
An object of class \code{noinfmodel} contains the
  following parameters.
  \item{a0}{Initial state}
  \item{a1}{Inertia}
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

\author{Tara Madhyastha and Ellen Hamaker}

\seealso{ \code{\link{linmodel}}, \code{\link{ojivemodel}}, \code{\link{combimodel}}, \code{\link{origmodel}},\code{\link{bilinmodel}}}

\examples{

require(dyad)
data(couple)
## fit a model with no influence
fit <- noinfmodel(couple)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
