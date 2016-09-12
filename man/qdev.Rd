% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qdev.R, R/testfun.R
\name{qdev}
\alias{qdev}
\alias{qdev.GA}
\alias{qdev.LOGNO}
\alias{qdev.NBII}
\alias{qdev.PO}
\alias{qdev.TNO}
\alias{qdev.WEI}
\alias{qdev.ZEXP}
\alias{qdev.ZIP}
\title{Sum of squared differences between distribution parameters}
\usage{
qdev.TNO(par, obs, prob)

qdev.LOGNO(par, obs, prob)

qdev.WEI(par, obs, prob)

qdev.ZEXP(par, obs, prob)

qdev.GA(par, obs, prob)

qdev.PO(par, obs, prob)

qdev.NBII(par, obs, prob)

qdev.ZIP(par, obs, prob)

qdev.TNO(par, obs, prob)

qdev.LOGNO(par, obs, prob)

qdev.WEI(par, obs, prob)

qdev.ZEXP(par, obs, prob)

qdev.GA(par, obs, prob)

qdev.PO(par, obs, prob)

qdev.NBII(par, obs, prob)

qdev.ZIP(par, obs, prob)
}
\arguments{
\item{par}{double    length=2    parameter values for theoretical distribution}

\item{obs}{double    length=3    observed mean and quantiles}

\item{prob}{double    length=2    "vector of confidence", i.e. proba=(p(rand.obs < q1),p(rand.obs < q2))}

\item{par}{double    length=2    parameter values for theoretical distribution}

\item{obs}{double    length=3    observed mean and quantiles}

\item{prob}{double    length=2    "vector of confidence", i.e. proba=(p(rand.obs < q1),p(rand.obs < q2))}
}
\value{
All functions returns an unnamed object of length=2 with residual sums of squares

All functions returns an unnamed object of length=2 with residual sums of squares
}
\description{
Functions for calculating sum of squared differences between "observed" parameter values and
model distribution parameters

Functions for calculating sum of squared differences between "observed" parameter values and
model distribution parameters
}
\details{
Functions qdev.TNO, qdev.LOGNO, qdev.WEI, qdev.ZEXP, qdev.GA, qdev.PO, qdev.NBII, qdev.ZIP below
returns the sum of squared differences between "observed" values for the expected value (obs[2]) and two
quantiles (obs[1] and obs[3]) from an empirical distribution and ditto (m/q2, q1 and q3) predicted from
respectively a given truncated normal-, lognormal-, weibull-, zero-inflated exponential distribution
gamma-, poisson-, negative binomial-, and zero-inflated poisson distribution. The lower bound in the truncated
normal distribution is always zero, while the upper bound is infinity. With these restrictions all theoretical
distributions have two parameters, except the Poisson with only one parameter.

Functions qdev.TNO, qdev.LOGNO, qdev.WEI, qdev.ZEXP, qdev.GA, qdev.PO, qdev.NBII, qdev.ZIP below
returns the sum of squared differences between "observed" values for the expected value (obs[2]) and two
quantiles (obs[1] and obs[3]) from an empirical distribution and ditto (m/q2, q1 and q3) predicted from
respectively a given truncated normal-, lognormal-, weibull-, zero-inflated exponential distribution
gamma-, poisson-, negative binomial-, and zero-inflated poisson distribution. The lower bound in the truncated
normal distribution is always zero, while the upper bound is infinity. With these restrictions all theoretical
distributions have two parameters, except the Poisson with only one parameter.
}
\examples{
qdev.TNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.LOGNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.WEI(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.ZEXP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.GA(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.PO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.NBII(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.ZIP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.TNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.LOGNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.WEI(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.ZEXP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.GA(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.PO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.NBII(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
qdev.ZIP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
}
\author{
All functions programmed by Nigel Yoccoz except qdev.ZEXP programmed by Bård Pedersen

All functions programmed by Nigel Yoccoz except qdev.ZEXP programmed by Bård Pedersen
}
