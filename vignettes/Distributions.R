## ----setup, include=FALSE------------------------------------------------
require(knitr)
require(distr)
require(NIcalc)
knitr::opts_chunk$set(echo = TRUE, tidy = T)
options(width = 60)


## ---- echo = F-----------------------------------------------------------

nMean <- 0.5
nSd <- 0.1


## ------------------------------------------------------------------------

nMean <- 0.5
nSd <- 0.1

N <- distr::Lnorm(mean = nMean, sd = nSd)



## ------------------------------------------------------------------------
N <- makeDistribution(input = "logNormal", distParams = list("mean" = 1, "sd" = 0.2))


## ------------------------------------------------------------------------
sampleDistribution


## ------------------------------------------------------------------------
sampleDistribution(N, 5)


## ------------------------------------------------------------------------
P <- makeDistribution("Poisson", distParams = list("lambda" = 5))


## ------------------------------------------------------------------------
sampleDistribution(P, 10)


## ------------------------------------------------------------------------
#Simulate an output from a MCMC run.
nSamples <- 100000
indicatorPosterior <- rnorm(nSamples, mean = 0.5, sd = 0.1)
EmpiricalPointEstimate <- mean(indicatorPosterior)
EmpiricalPointEstimate #mean of these particular draws
 
E <- distr::EmpiricalDistribution(indicatorPosterior)

sampleDistribution(E)


## ------------------------------------------------------------------------
E <- makeDistribution(indicatorPosterior)
sampleDistribution(E)
E@q(0.5) ##mean of the distribution, used internally to set the point estimate


## ------------------------------------------------------------------------
nSamples <- 1e5
sampleMethod <- indicatorPosterior[x = sample(1:length(indicatorPosterior), size = nSamples, replace = T)]

NImethod <- sampleDistribution(E, nSamples)
plot(density(sampleMethod), col = 1, main = "Comparison of distribution object \ndraws to default samples")
points(density(NImethod), col = 2, type = "l")
legend("topright", legend = c("Default", "NI smethod"), fill = 1:2)



## ------------------------------------------------------------------------
allowedIndicatorValues <- c(0.1, 0.2, 0.3, 0.4)
indicatorValueProbabilities <- c(0.1, 0.4, 0.4, 0.1)   

discretePointEstimate <- mean(allowedIndicatorValues)

D <- distr::DiscreteDistribution(allowedIndicatorValues, indicatorValueProbabilities)

sampleDistribution(D)



## ------------------------------------------------------------------------
allowedIndicatorValues <- c(0.1, 0.2, 0.3, 0.4)
indicatorValueProbabilities <- c(0.1, 0.4, 0.4, 0.1)   

myProbs <- cbind(allowedIndicatorValues, indicatorValueProbabilities)

D <- makeDistribution(myProbs)

sampleDistribution(D, 10)


## ---- eval = F-----------------------------------------------------------
## #display an example data set here
## ##Doesn't go well with Rmarkdown!
## str(indicatorData)


## ---- eval = F-----------------------------------------------------------
## exists("niToken")


## ---- echo = F-----------------------------------------------------------
load("../temp/credentials.Rdata")
getToken(username, password)
niToken


## ---- eval = F-----------------------------------------------------------
## getToken("your.username", "secretPassword")
## 
## niToken


## ------------------------------------------------------------------------
myIndicators <- getIndicators()
myIndicators


## ---- eval=F-------------------------------------------------------------
## indicatorData <- getIndicatorValues(indicatorID = 351)


## ------------------------------------------------------------------------
indicatorData <- getIndicatorValues(indicatorID = myIndicators$id[myIndicators$name == "Dagsommerfugler i skog"], year = 2018)
indicatorData


## ------------------------------------------------------------------------
#setIndicatorValues(indicatorData, areaId = 7040, year = 2018, est = 0.9, lower = 0.7, upper = 1)

updatedIndicatorData <- setIndicatorValues(indicatorData, areaId = 7040, year = 2018, est = 0.9, lower = 0.7, upper = 1)

updatedIndicatorData




## ------------------------------------------------------------------------
updatedIndicatorData <- setIndicatorValues(updatedIndicatorData, areaId = 7041, year = 2018, distribution = cbind(c(0.1, 0.3, 0.4), c(0.33, 0.33, 0.34)))

updatedIndicatorData <- setIndicatorValues(updatedIndicatorData, areaId = 7042, year = 2018, distribution = cbind(c(0.2, 0.4, 0.5), c(0.33, 0.33, 0.34)))


updatedIndicatorData


## ----eval = F------------------------------------------------------------
## updatedIndicatorData <- setIndicatorValues(updatedIndicatorData, areaId = 7041, year = 2018, distribution = "logNormal", distParams = list("mean" = 0.7, "sd" = 0.15))
## 


## ---- eval = F-----------------------------------------------------------
## myCodasamples <- rnorm(1000) ## toy example coda results
## 
## updatedIndicatorData <- setIndicatorValues(updatedIndicatorData, areaId = 7041, year = 2018, distribution = myCodasamples)
## 


## ------------------------------------------------------------------------
writeIndicatorValues(updatedIndicatorData)


## ------------------------------------------------------------------------
newValues <- getIndicatorValues(351, year = 2018)
newValues

