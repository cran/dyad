\name{ojivemodel}
\alias{ojivemodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gottman-Murray Marriage Model with Ojive Influence}
\description{
 Fit the Gottman-Murray marriage model with the ojive influence
 function, optimizing the threshold. 

}
\usage{
ojivemodel(observations, nregime = 3, mpr = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observations}{A data frame with two columns, one for the wife (person
    1) and one for the husband (person 2) scores for each unit of time.}
  \item{nregime}{Number of regimes (either 2 or 3). If 2 regimes are
    specified, there is a negative and positive regime. If 3 regimes are
    specified, there is a negative, neutral, and positive
    regime. Default is 3.}
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

  The influence function here is an ojive function. This is based on the
  assumption that the influence of one partner on the other is constant
  until his/her behavior passes some critical value or threshold. The
  region to each side of the threshold is referred to as a regime. The
  ojive function used here is proposed by Hamaker, E., Zhang, Z., and
  Van der Maas, H.L. (See References). 

  For values in each regime, a horizontal line is fit to the influence.
  By varying the
  threshold values (one threshold if 2 regimes are used, and two
  thresholds if 3 regimes are used) we can determine the fit with the
  minimum sum of squares residuals. We can write the ojive influence
  functions as \eqn{I_{HW}(H_t)=}{IHW(H(t))}

  \deqn{\delta^{-}_w\quad if \quad H_t\le\tau^{-}_w}{l1 if H_t <= nth}
  \deqn{0 \quad if \quad \tau^{-}_{w} < H_t \le \tau^{-}_w}{0
    if nth < H_t}
  \deqn{\delta^{-}_w  \quad if \quad \tau^{+}_{w} < H_t }{l1 if
    pth < H_t}

  Parameters are estimated simultaneously using the method described by
  Hamaker, E., Zhang, Z., and Van der Maas, H.L. (See References).
}
\value{
  \code{ojivemodel} returns a list consisting of the results (the
  parameters fit by the model) for the
  wife (person 1) and the husband (person 2). Each set of results is an
  object of class \code{ojivemodel}. Although variable names (e.g., a0
  and a1) are the same for each object, their values correspond to the
  model fit to the husband or wife (and are generally different for each
  spouse). Therefore, a0 and a1 for the
  husband's results should be interpreted as b0 and b1 in the equation
  for the husband above. 
  The \code{plot} method for \code{ojivemodel} objects graphs the
  partner score against the influence and the interpolated influence
  function. For example, for the wife results, this would plot the
  husband score against the influence of the husband on the wife.
 
  An object of class \code{ojivemodel} contains the
  following parameters, depending on the number of regimes specified.
  \item{a0}{Initial state}
  \item{a1}{Inertia}
  \item{l1}{Difference in constant between neutral regime and negative
    regime}
  \item{l2}{Difference in constant between positive regime and neutral
    regime (3 regime only)}
  \item{th}{Threshold (2 regime only)}
  \item{nth}{Threshold between negative and neutral regime (3 regime only)}
  \item{pth}{Threshold between positive and neutral regime (3 regime only)}
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
\seealso{ \code{\link{bilinmodel}}, \code{\link{combimodel}}, \code{\link{origmodel}}}

\examples{
require(dyad)
data(couple)
## fit an ojive model with 3 regimes
fit <- ojivemodel(couple, nregime=3);
## plot influence function for husband on wife
plot(fit$wife)

}
\keyword{ models }
\keyword{ nonlinear }