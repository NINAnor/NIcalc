#' plotDistribution
#'
#' Plot function for known distributions, defined by input parameters
#'
#'
#'
#'
#' @param distrib Type of distribution. Must match allowed values.
#' @param mu mu parameter of distribution (i.e. mean for normal distribution)
#' @param sig  sigma parameter of distribution
#' @param refValue reference value to be plotted along with the distribution
#' @param obsval
#'
#'
#' @importFrom msm dtnorm
#'
#'
#'
#'
#' @export
#'



plotDistribution <- function(distrib = c("Gamma",
                                         "ZIExponential",
                                         "LogNormal",
                                         "TruncNormal",
                                         "Normal",
                                         "Weibull",
                                         "Poisson",
                                         "NegBinom",
                                         "ZIP",
                                         "NoBoot"),
                             mu = NULL,
                             sig = NULL,
                             refValue = 1,
                             obsval = c("lower" = NULL,
                                        "mu" = NULL,
                                        "upper" = NULL),
                             type = "continuous") {

  #Det gjenstår ganske mye programmering på denne funksjonen, men den fungerer vel for formålet så langt.

  distrib <- match.arg(distrib, choices = c("Gamma",
                                  "ZIExponential",
                                  "LogNormal",
                                  "TruncNormal",
                                  "Normal",
                                  "Weibull",
                                  "Poisson",
                                  "NegBinom",
                                  "ZIP",
                                  "NoBoot"))

  #type.t <- "continuous"

  quantiles <- seq(0,1.25,0.00001)*refValue

  if (distrib=="Gamma"){
    vec<-dGA(quantiles,mu=mu,sigma=sig)
    q1<-qGA(p=0.25, mu=mu, sigma=sig)
    q2<-mu
    q3<-qGA(p=0.75, mu=mu, sigma=sig)
    d1<-dGA(q1, mu=mu, sigma=sig)
    d2<-dGA(q2, mu=mu, sigma=sig)
    d3<-dGA(q3, mu=mu, sigma=sig)
    o1<-dGA(obsval["lower"], mu=mu, sigma=sig)
    o2<-dGA(obsval["mu"], mu=mu, sigma=sig)
    o3<-dGA(obsval["upper"], mu=mu, sigma=sig)
    disttext <- "Gamma"
  }

  if (distrib=="ZIExponential"){
    vec<-c(mu,dexp(quantiles[2:length(quantiles)],rate=sig)*(1-mu))
    q1<-ifelse(mu >= 0.25, 0, qexp(p=(0.25-mu)/(1-mu),rate=sig))
    q2<-(1-mu)/sig
    q3<-ifelse(mu >= 0.75, 0, qexp(p=(0.75-mu)/(1-mu),rate=sig))
    d1<-ifelse(q1 == 0, mu, (1-mu)*dexp(q1,rate=sig))
    d2<-(1-mu)*dexp(q2,rate=sig)
    d3<-ifelse(q3 == 0, mu, (1-mu)*dexp(q3,rate=sig))
    o1<-ifelse(obsval["lower"] == 0, mu, (1-mu)*dexp(obsval["lower"],rate=sig))
    o2<-(1-mu)*dexp(obsval["mu"],rate=sig)
    o3<-ifelse(obsval["upper"] == 0, mu, (1-mu)*dexp(obsval["upper"],rate=sig))
    disttext <- "Zero-inflated exponential"
  }
  if (distrib=="LogNormal")    {
    vec<-dLOGNO(quantiles,mu=mu,sigma=sig)
    q1<-qLOGNO(p=0.25, mu=mu, sigma=sig)
    q2<-(exp(sig^2))^0.5*exp(mu)
    q3<-qLOGNO(p=0.75, mu=mu, sigma=sig)
    d1<-dLOGNO(q1, mu=mu, sigma=sig)
    d2<-dLOGNO(q2, mu=mu, sigma=sig)
    d3<-dLOGNO(q3, mu=mu, sigma=sig)
    o1<-dLOGNO(obsval["lower"], mu=mu, sigma=sig)
    o2<-dLOGNO(obsval["mu"], mu=mu, sigma=sig)
    o3<-dLOGNO(obsval["upper"], mu=mu, sigma=sig)
    disttext <- "Log-normal"
  }
  if (distrib=="TruncNormal")  {
    vec<-dtnorm(quantiles,mean=mu,sd=sig,lower=0)
    q1<-qtnorm(p=0.25, mean=mu, sd=sig, lower=0)
    q2<-etruncnorm(a=0, b=1e11, mean=mu, sd=sig)
    q3<-qtnorm(p=0.75, mean=mu, sd=sig, lower=0)
    d1<-dtnorm(q1, mean=mu, sd=sig, lower=0)
    d2<-dtnorm(q2, mean=mu, sd=sig, lower=0)
    d3<-dtnorm(q3, mean=mu, sd=sig, lower=0)
    o1<-dtnorm(obsval["lower"], mean=mu, sd=sig, lower=0)
    o2<-dtnorm(obsval["mu"], mean=mu, sd=sig, lower=0)
    o3<-dtnorm(obsval["upper"], mean=mu, sd=sig, lower=0)
    disttext <- "Truncated normal"
  }
  if (distrib=="Normal")       {vec<-dNO(quantiles,mu=mu,sigma=sig)}
  if (distrib=="Weibull")      {
    vec<-dWEI(quantiles,mu=mu,sigma=sig)
    q1<-qWEI(p=0.25, mu=mu, sigma=sig)
    q2<-mu*gamma(1/sig+1)
    q3<-qWEI(p=0.75, mu=mu, sigma=sig)
    d1<-dWEI(q1, mu=mu, sigma=sig)
    d2<-dWEI(q2, mu=mu, sigma=sig)
    d3<-dWEI(q3, mu=mu, sigma=sig)
    o1<-dWEI(obsval["lower"], mu=mu, sigma=sig)
    o2<-dWEI(obsval["mu"], mu=mu, sigma=sig)
    o3<-dWEI(obsval["upper"], mu=mu, sigma=sig)
    disttext <- "Weibull"
  }
  if (distrib=="Poisson")      {vec<-dPO(quantiles,mu=mu)}
  if (distrib=="NegBinom")     {vec<-dNBII(quantiles,mu=mu,sigma=sig)}
  if (distrib=="ZIP")          {vec<-dZIP(quantiles,mu=mu,sigma=sig)}
  if (distrib=="NoBoot")       {vec<-rep(quantiles,nsim)}

  if (distrib=="Weibull") {vec[is.na(vec)] <- 0}
  if (distrib=="Gamma") {vec[is.na(vec)] <- 0}

  quantiles <- quantiles/refValue

  if (distrib=="ZIExponential") {
    plot(quantiles[2:length(quantiles)],vec[2:length(quantiles)],type="l",
         #		ylim=c(0,max(vec)+0.1*max(vec)),lwd=4,axes=F,xlab="",ylab="")
         ylim=c(0,1),lwd=4,axes=F,xlab="",ylab="")
    points(0,vec[1],pch=19,cex=2)
  } else {
    if (distrib=="Gamma") {
      plot(quantiles[2:length(quantiles)],vec[2:length(quantiles)],type="l",
           ylim=c(0,max(vec[2:length(quantiles)])+0.1*max(vec)),lwd=4,axes=F,xlab="",ylab="")
    } else {
      plot(quantiles,vec,type="l",ylim=c(0,max(vec)+0.1*max(vec)),lwd=4,axes=F,xlab="",ylab="")
    }
  }

  axis(1, at = c(0.0,0.25,0.5,0.75,1.0,1.25), labels =c("","","","","ref",""),
       pos = 0.0, lwd = 2, lwd.ticks = 2)
  axis(2, pos = 0.0, lwd = 2, lwd.ticks = 2, cex=2)
  lines(c(q1,q1)/refValue,c(0,d1),lwd=3,col=2,lty=2)
  lines(c(q2,q2)/refValue,c(0,d2),lwd=3,col=2,lty=1)
  lines(c(q3,q3)/refValue,c(0,d3),lwd=3,col=2,lty=2)
  lines(c(obsval["lower"],obsval["lower"])/refValue,c(0,o1),lwd=3,col=4,lty=2)
  lines(c(obsval["mu"],obsval["mu"])/refValue,c(0,o2),lwd=3,col=4,lty=1)
  lines(c(obsval["upper"],obsval["upper"])/refValue,c(0,o3),lwd=3,col=4,lty=2)
  lines(c(1,1),c(0,0.93*max(vec)),lwd=3,col=1,lty=3)
  lines(c(1,1),c(0,0.85),lwd=3,col=1,lty=3)

  title(xlab = list("Quantiles", cex=1.5))
  title(ylab = list("Density",cex=1.5))
  #text(0.9,max(vec),disttext,cex=1.25)
  text(0.35,max(vec)+0.1*max(vec),disttext,cex=1.25)

}


