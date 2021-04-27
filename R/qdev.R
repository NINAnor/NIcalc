#' Sum of squared Differences between Distribution Parameters
#'
#' Functions for calculating sum of squared differences between a set of "observed" parameter values and
#' corresponding model distribution parameters
#'
#' Functions qdev.TNO, qdev.LOGNO, qdev.WEI, qdev.ZEXP, qdev.GA, qdev.PO, qdev.NBII, qdev.ZIP
#' return the sum of squared differences between the expected value (\code{obs[2]}) and two
#' quantiles (\code{obs[1], obs[3]}) from an empirical distribution and ditto predicted from
#' respectively a given truncated normal-, lognormal-, Weibull-, zero-inflated exponential-,
#' gamma-, Poisson-, negative binomial-, and zero-inflated poisson distribution with parameters
#' \code{par}. The lower bound in the truncated normal distribution is always zero, while the
#' upper bound is infinity. With these restrictions all theoretical distributions have two parameters,
#' except the Poisson with only one parameter.
#'
#' @name qdev
#' @encoding UTF-8
#' @author All functions programmed by Nigel Yoccoz except qdev.ZEXP programmed by Bård Pedersen
#'
#' @import gamlss.dist
#' @importFrom truncnorm etruncnorm
#' @importFrom stats qexp
#' @importFrom msm qtnorm
#'
#' @param par	double	length=2	parameter values for theoretical distribution
#' @param obs	double	length=3	observed first quantile, expected value, and second quantile
#' @param prob	double	length=2	probabilities corresponding to the quantiles,
#' i.e. \eqn{prob = (p(x < obs[1]), p(x < obs[3]))} for a random variable \eqn{x}.
#' @return All functions return an unnamed object of length=1 with the sum of squared differences
#'
#' @examples
#' qdev.TNO(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.LOGNO(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.WEI(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.ZEXP(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.ZIP(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.NBII(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.PO(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#' qdev.GA(par = c(1,1), obs = c(0.1, 1.3, 2), prob = c(0.025, 0.975))
#'
NULL
#' @rdname qdev
#' @export
qdev.TNO <- function(par,obs,prob) {
  q1<-msm::qtnorm(p=prob[1], mean=par[1], sd=par[2], lower=0)
  q2<-truncnorm::etruncnorm(a=0, b=1e11, mean=par[1], sd=par[2])
  q3<-msm::qtnorm(p=prob[2], mean=par[1], sd=par[2], lower=0)
  qqq<-c(q1,q2,q3)
  sum((qqq-obs)^2)
}
#' @rdname qdev
#' @export
qdev.LOGNO <- function(par,obs,prob) {
  q1<-gamlss.dist::qLOGNO(p=prob[1],mu=par[1],sigma=par[2])
  m<-(exp(par[2]^2))^0.5*exp(par[1])
  q3<-gamlss.dist::qLOGNO(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
#'
qdev.WEI <- function(par,obs,prob) {
  op <- options("warn")
  options(warn = -1)
  q1<-gamlss.dist::qWEI(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]*gamma(1/par[2]+1)
  q3<-gamlss.dist::qWEI(p=prob[2],mu=par[1],sigma=par[2])
  options(op)
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
qdev.ZEXP <- function(par,obs,prob) {
  if (par[1] == 1) {
    qmq <- c(0.0,0.0,0.0)
  } else {
    q1<-ifelse(par[1] >= prob[1], 0, stats::qexp(p=(prob[1]-par[1])/(1-par[1]),rate=par[2]))
    m<-(1-par[1])/par[2]
    q3<-ifelse(par[1] >= prob[2], 0, stats::qexp(p=(prob[2]-par[1])/(1-par[1]),rate=par[2]))
    qmq<-c(q1,m,q3)
  }
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
#'
qdev.GA <- function(par,obs,prob) {
  q1<-gamlss.dist::qGA(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]
  q3<-gamlss.dist::qGA(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
#'
qdev.PO <- function(par,obs,prob) {
  q1<-gamlss.dist::qPO(p=prob[1],mu=par[1])
  m<-par[1]
  q3<-gamlss.dist::qPO(p=prob[2],mu=par[1])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
#'
qdev.NBII <- function(par,obs,prob) {
  q1<-gamlss.dist::qNBII(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]
  q3<-gamlss.dist::qNBII(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
#' @rdname qdev
#' @export
#'
# 	Zero-inflated Poisson distribution
qdev.ZIP <- function(par,obs,prob) {
  q1<-gamlss.dist::qZIP(p=prob[1],mu=par[1],sigma=par[2])
  m<-(1-par[2])*par[1]
  q3<-gamlss.dist::qZIP(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}
