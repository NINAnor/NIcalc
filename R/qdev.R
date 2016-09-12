#' Sum of squared differences between distribution parameters
#'
#' Functions for calculating sum of squared differences between "observed" parameter values and
#' model distribution parameters
#'
#' Functions qdev.TNO, qdev.LOGNO, qdev.WEI, qdev.ZEXP, qdev.GA, qdev.PO, qdev.NBII, qdev.ZIP below
#' returns the sum of squared differences between "observed" values for the expected value (obs[2]) and two
#' quantiles (obs[1] and obs[3]) from an empirical distribution and ditto (m/q2, q1 and q3) predicted from
#' respectively a given truncated normal-, lognormal-, weibull-, zero-inflated exponential distribution
#' gamma-, poisson-, negative binomial-, and zero-inflated poisson distribution. The lower bound in the truncated
#' normal distribution is always zero, while the upper bound is infinity. With these restrictions all theoretical
#' distributions have two parameters, except the Poisson with only one parameter.
#'
#'
#' @name qdev
#' @author All functions programmed by Nigel Yoccoz except qdev.ZEXP programmed by Bård Pedersen
#'
#' @import gamlss.dist
#' @importFrom truncnorm etruncnorm
#' @importFrom stats qexp quantile
#' @importFrom msm qtnorm
#'
#' @param par	double	length=2	parameter values for theoretical distribution
#' @param obs	double	length=3	observed mean and quantiles
#' @param prob	double	length=2	"vector of confidence", i.e. proba=(p(rand.obs < q1),p(rand.obs < q2))
#' @return All functions returns an unnamed object of length=2 with residual sums of squares
#'
#'
#' @rdname qdev
#' @examples
#' qdev.TNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
qdev.TNO <- function(par,obs,prob) {
  q1<-qtnorm(p=prob[1], mean=par[1], sd=par[2], lower=0)
  q2<-etruncnorm(a=0, b=1e11, mean=par[1], sd=par[2])
  q3<-qtnorm(p=prob[2], mean=par[1], sd=par[2], lower=0)
  qqq<-c(q1,q2,q3)
  sum((qqq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.LOGNO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
qdev.LOGNO <- function(par,obs,prob) {
  q1<-qLOGNO(p=prob[1],mu=par[1],sigma=par[2])
  m<-(exp(par[2]^2))^0.5*exp(par[1])
  q3<-qLOGNO(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}


#' @rdname qdev
#' @examples
#' qdev.WEI(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
qdev.WEI <- function(par,obs,prob) {
  op <- options("warn")
  options(warn = -1)
  q1<-qWEI(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]*gamma(1/par[2]+1)
  q3<-qWEI(p=prob[2],mu=par[1],sigma=par[2])
  options(op)
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.ZEXP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
qdev.ZEXP <- function(par,obs,prob) {
  if (par[1] == 1) {
    qmq <- c(0.0,0.0,0.0)
  } else {
    q1<-ifelse(par[1] >= prob[1], 0, qexp(p=(prob[1]-par[1])/(1-par[1]),rate=par[2]))
    m<-(1-par[1])/par[2]
    q3<-ifelse(par[1] >= prob[2], 0, qexp(p=(prob[2]-par[1])/(1-par[1]),rate=par[2]))
    qmq<-c(q1,m,q3)
  }
  sum((qmq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.GA(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
qdev.GA <- function(par,obs,prob) {
  q1<-qGA(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]
  q3<-qGA(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.PO(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
qdev.PO <- function(par,obs,prob) {
  q1<-qPO(p=prob[1],mu=par[1])
  m<-par[1]
  q3<-qPO(p=prob[2],mu=par[1])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.NBII(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
qdev.NBII <- function(par,obs,prob) {
  q1<-qNBII(p=prob[1],mu=par[1],sigma=par[2])
  m<-par[1]
  q3<-qNBII(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}

#'
#' @rdname qdev
#' @examples
#' qdev.ZIP(par = c(1,1), obs = c(1.3, 0.1, 2), prob = c(0.025, 0.975))
#' @export
#'
# 	Zero-inflated Poisson distribution
qdev.ZIP <- function(par,obs,prob) {
  q1<-qZIP(p=prob[1],mu=par[1],sigma=par[2])
  m<-(1-par[2])*par[1]
  q3<-qZIP(p=prob[2],mu=par[1],sigma=par[2])
  qmq<-c(q1,m,q3)
  sum((qmq-obs)^2)
}

