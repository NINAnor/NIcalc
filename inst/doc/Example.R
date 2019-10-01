## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy=T)
require(xtable)
options("xtable.comment"=F)

## ---- out.width='.49\\linewidth',echo=FALSE------------------------------
Uref <- 10

plot(	c(0,Uref,2*Uref,2.2*Uref),
	c(0,1,1,1),
	type="l",
	xlim=c(0,22),
	xaxp=c(0,40,1),
	ylim=c(0,1),
	ylab = expression(paste(italic("Scaled value (S)"))),
	xlab = expression(paste(italic("Unscaled value"))),
	lwd=3,
	col=2)
axis(1, at = c(0,10,20), labels = c("",expression(italic("Ref")),expression(paste("2*",italic("Ref")))))
lines(c(2*Uref,2*Uref), c(0,1),col=1,lty=2,lwd=1)
lines(c(Uref,Uref), c(0,1),col=1,lty=2,lwd=1)
text(2.5,0.75,"LOW",cex = 1.75)

plot(	c(0,Uref,2*Uref,2.2*Uref),
	c(1,1,0,0),
	type="l",
	xlim=c(0,22),
	xaxp=c(0,40,1),
	ylim=c(0,1),
	ylab = expression(paste(italic("Scaled value (S)"))),
	xlab = expression(paste(italic("Unscaled value"))),
	lwd=3,
	col=2)
axis(1, at = c(0,10,20), labels = c("",expression(italic("Ref")),expression(paste("2*",italic("Ref")))))
lines(c(2*Uref,2*Uref), c(0,1),col=1,lty=2,lwd=1)
lines(c(Uref,Uref), c(0,1),col=1,lty=2,lwd=1)
text(2.5,0.75,"MAX",cex = 1.75)

rm(Uref)


## ---- eval=F, include=F, comment=NA--------------------------------------
#  file.path.name <- "M:/My Documents/Naturindeks/Bulgaria/Scripts to Bulgarian experts/"
#  load(file=paste(file.path.name,"TestdataNI.RData",sep=""))
#  rm(file.path.name)
#  ls()

## ----load_data-----------------------------------------------------------
require(NIcalc)

data(BSunits, Indic, Indicator.area, Indicator.area.ind, Observations)
ls()

## ---- echo=TRUE, comment=NA, results='asis'------------------------------
print(xtable(rbind(head(Observations),tail(Observations))))

## ---- echo=TRUE, comment=NA, results='asis'------------------------------
print(xtable(rbind(head(Indic),tail(Indic))))

## ---- echo=TRUE, comment=NA, results='asis'------------------------------
print(xtable(BSunits))

## ---- results='asis'-----------------------------------------------------

print(xtable(rbind(head(Indicator.area.ind),tail(Indicator.area.ind))))


## ---- results='asis'-----------------------------------------------------

print(xtable(rbind(head(Indicator.area),tail(Indicator.area))))

## ---- echo=TRUE----------------------------------------------------------
library(MASS)
library(gamlss.dist)
library(truncnorm)
require(msm)

## ---- eval=F, include=F--------------------------------------------------
#  path <- "~/Prosjekter/NI R-pakke/Funksjoner sendt til Bulgaria/"
#  source(paste(path,"Simplified functions for calculating the Nature Index.R",sep=""), encoding = "Windows-1252")
#  rm(path)

## ---- echo=TRUE, prompt=FALSE--------------------------------------------
elicitate(
		expected.value=Observations$Expected.value[1:10],
		lower=Observations$Lower[1:10],
		upper = Observations$Upper[1:10])

## ----echo=TRUE, prompt=FALSE---------------------------------------------
Observations2 <- data.frame(
	Observations,
	elicitate(
		expected.value=Observations$Expected.value,
		lower=Observations$Lower,
		upper = Observations$Upper)[,1:3])
head(Observations2)

## ---- echo=TRUE, prompt=FALSE--------------------------------------------
samplebootmat(
	ValueID=Observations2$ValueID,
	Value=Observations2$Expected.value, 
	RefobsID = Observations2$ReferenceYearID,
	DistID=Observations2$FK_DistID,
	mu=Observations2$mu,
	sig=Observations2$sig,
	nsim=5)[1:10,]


## ----echo=TRUE, prompt=FALSE---------------------------------------------
Alt_DistID <- Observations2$FK_DistID
Alt_DistID[Observations2$ReferenceYearID==0] <- "NoBoot"

samplebootmat(
	ValueID=Observations2$ValueID,
	Value=Observations2$Expected.value, 
	RefobsID = Observations2$ReferenceYearID,
	DistID=Alt_DistID,
	mu=Observations2$mu,
	sig=Observations2$sig,
	nsim=5)[1:10,]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
Observationsyear2 <- Observations2[Observations2$ReferenceYearID %in% c(0,2),]
Alt_DistIDyear2 <- Alt_DistID[Observations2$ReferenceYearID %in% c(0,2)]

bootmatyear2 <- samplebootmat(
	ValueID=Observationsyear2$ValueID,
	Value=Observationsyear2$Expected.value, 
	RefobsID = Observationsyear2$ReferenceYearID,
	DistID=Alt_DistIDyear2,
	mu=Observationsyear2$mu,
	sig=Observationsyear2$sig,
	nsim=1000)
bootmatyear2[1:10,1:5]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
scaled.bootmatyear2 <- scaleobs(
	ValueID=Observationsyear2$ValueID,
	FK_OmraadeID=Observationsyear2$IndicatorareaID,
	FK_IndicatorID=Observationsyear2$IndicatorID,
	FK_RefAarID=Observationsyear2$ReferenceYearID,
	nsim=1000,
	bootmat=bootmatyear2,
	ref.value.code=0,
	IndicatorID=Indic$IndicatorID,
	FK_Scalingmodel=Indic$Scalingmodel)
scaled.bootmatyear2[1:9,1:5]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
scaled.bootmatyear2only <- scaled.bootmatyear2[Observationsyear2$ReferenceYearID != 0,]
scaled.bootmatyear2only[1:9,1:5]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
dim(scaled.bootmatyear2only)

## ----echo=TRUE, prompt=FALSE---------------------------------------------
Observationsyear2only <- Observationsyear2[Observationsyear2$ReferenceYearID != 0,]
dim(Observationsyear2only)

## ----echo=TRUE, prompt=FALSE---------------------------------------------
areaweights1 <- areaweights(
	Municipalities=BSunits$Basicunit,
	Regions=BSunits$NIarea1,
	Area.municipality=BSunits$Area,
	Indicators=Indic$Indicator_name)
areaweights1[,1:6,]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
areaweights2 <- areaweights(
	Municipalities=BSunits$Basicunit,
	Regions=BSunits$NIarea2,
	Area.municipality=BSunits$Area,
	Indicators=Indic$Indicator_name)
areaweights2[,1:6,]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
colSums(areaweights2) [1:6,]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
areaweights(
	Municipalities=BSunits$Basicunit,
	Regions=BSunits$NIarea2,
	Area.municipality=BSunits$Area/BSunits$Area,
	Indicators=Indic$Indicator_name) [,1:6,]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
munweights <- munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=Indic$TrophicgroupID,
	Key.indicators=Indic$Key.indicators,
	Fidelity=Indic$Fidelity,
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit)

munweights[,1:6]
rowSums(munweights)

## ----echo=TRUE, prompt=FALSE---------------------------------------------
munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=rep(4,length(Indic$TrophicgroupID)),
	Key.indicators=rep(F,length(Indic$Key.indicators)),
	Fidelity=rep(1.0,length(Indic$Fidelity)),
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit) [,1:6]

## ----echo=TRUE, prompt=FALSE---------------------------------------------
summary(NIcalculate(
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit,
	FK_OmraadeID=Observationsyear2$IndicatorareaID[Observationsyear2$ReferenceYearID != 0],
	nsim=1000,
	scaled.bootmat=scaled.bootmatyear2[Observationsyear2$ReferenceYearID != 0,],
	Weights.trof=munweights,
	Weights.reg.area=areaweights1))

## ----echo=TRUE, prompt=FALSE---------------------------------------------
summary(NIcalculate(
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit,
	FK_OmraadeID=Observationsyear2$IndicatorareaID[Observationsyear2$ReferenceYearID != 0],
	nsim=1000,
	scaled.bootmat=scaled.bootmatyear2[Observationsyear2$ReferenceYearID != 0,],
	Weights.trof=munweights,
	Weights.reg.area=areaweights2))

## ----echo=TRUE, prompt=FALSE---------------------------------------------
summary(NIcalculate(
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit,
	FK_OmraadeID=Observationsyear2$IndicatorareaID[Observationsyear2$ReferenceYearID != 0],
	nsim=1000,
	scaled.bootmat=scaled.bootmatyear2[Observationsyear2$ReferenceYearID != 0,],
	Weights.trof=munweights))

## ----echo=TRUE, prompt=FALSE---------------------------------------------
summary(NIcalculate(
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit,
	FK_OmraadeID=Observationsyear2$IndicatorareaID[Observationsyear2$ReferenceYearID != 0],
	nsim=1000,
	scaled.bootmat=scaled.bootmatyear2[Observationsyear2$ReferenceYearID != 0,],
	Weights.trof=munweights)
	)

## ----echo=TRUE, prompt=FALSE---------------------------------------------
Observations <- data.frame(
	Observations,
	elicitate(
		expected.value=Observations$Expected.value,
		lower=Observations$Lower,
		upper = Observations$Upper)[,1:3])

years <- unique(Observations$ReferenceYearID[Observations$ReferenceYearID!=0])
Alt_DistID <- Observations$FK_DistID
Alt_DistID[Observations$ReferenceYearID==0] <- "NoBoot"

bootmatall <- samplebootmat(
	ValueID=Observations$ValueID,
	Value=Observations$Expected.value,
	RefobsID = Observations$ReferenceYearID,
	DistID=Alt_DistID,
	mu=Observations$mu,
	sig=Observations$sig,
	nsim=1000)

scaled.bootmatall <- scaleobs(
	ValueID=Observations$ValueID,
	FK_OmraadeID=Observations$IndicatorareaID,
	FK_IndicatorID=Observations$IndicatorID,
	FK_RefAarID=Observations$ReferenceYearID,
	nsim=1000,
	bootmat=bootmatall,
	ref.value.code=0,
	IndicatorID=Indic$IndicatorID,
	FK_Scalingmodel=Indic$Scalingmodel)

munweightsforall <- munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=Indic$TrophicgroupID,
	Key.indicators=Indic$Key.indicators,
	Fidelity=Indic$Fidelity,
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit)

NIresult <- NULL

for (i in years) {

	scaled.bootmatyeari <- scaled.bootmatall[Observations$ReferenceYearID == i,]

	NIi <- summary(NIcalculate(
		Areaind.Name=Indicator.area.ind$IndicatorareaID,
		Areaind.Indicator=Indicator.area.ind$Indicator_name,
		Area.Name=Indicator.area$IndicatorareaID,
		Area.Municipality=Indicator.area$Basicunit,
		FK_OmraadeID=Observations$IndicatorareaID[Observations$ReferenceYearID == i],
		nsim=1000,
		scaled.bootmat=scaled.bootmatyeari,
		Weights.trof=munweightsforall))

	year <- rep(i,dim(NIi) [1])
	iresult <- cbind(year,NIi)
	NIresult <- rbind(NIresult,iresult)

}

NIresult

## ----echo=FALSE, prompt=FALSE--------------------------------------------
Alt_DistID <- Observations$FK_DistID

bootmatall <- samplebootmat(
	ValueID=Observations$ValueID,
	Value=Observations$Expected.value, 
	RefobsID = Observations2$ReferenceYearID,
	DistID=Alt_DistID,
	mu=Observations$mu,
	sig=Observations$sig,
	nsim=1000)

scaled.bootmatall <- scaleobs(
	ValueID=Observations$ValueID,
	FK_OmraadeID=Observations$IndicatorareaID,
	FK_IndicatorID=Observations$IndicatorID,
	FK_RefAarID=Observations$ReferenceYearID,
	nsim=1000,
	bootmat=bootmatall,
	ref.value.code=0,
	IndicatorID=Indic$IndicatorID,
	FK_Scalingmodel=Indic$Scalingmodel)

munweightsforall <- munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=Indic$TrophicgroupID,
	Key.indicators=Indic$Key.indicators,
	Fidelity=Indic$Fidelity,
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit)

NIresult1 <- NULL

for (i in years) {

	scaled.bootmatyeari <- scaled.bootmatall[Observations$ReferenceYearID == i,]

	NIi <- summary(NIcalculate(
		Areaind.Name=Indicator.area.ind$IndicatorareaID,
		Areaind.Indicator=Indicator.area.ind$Indicator_name,
		Area.Name=Indicator.area$IndicatorareaID,
		Area.Municipality=Indicator.area$Basicunit,
		FK_OmraadeID=Observations$IndicatorareaID[Observations$ReferenceYearID == i],
		nsim=1000,
		scaled.bootmat=scaled.bootmatyeari,
		Weights.trof=munweightsforall))

	year <- rep(i,dim(NIi) [1])
	iresult <- cbind(year,NIi)
	NIresult1 <- rbind(NIresult1,iresult)

}

NIresult1

## ----echo=FALSE, prompt=FALSE--------------------------------------------
Alt_DistID <- Observations$FK_DistID
Alt_DistID[Observations$ReferenceYearID!=0] <- "NoBoot"

bootmatall <- samplebootmat(
	ValueID=Observations$ValueID,
	Value=Observations$Expected.value,
	RefobsID = Observations$ReferenceYearID,
	DistID=Alt_DistID,
	mu=Observations$mu,
	sig=Observations$sig,
	nsim=1000)

scaled.bootmatall <- scaleobs(
	ValueID=Observations$ValueID,
	FK_OmraadeID=Observations$IndicatorareaID,
	FK_IndicatorID=Observations$IndicatorID,
	FK_RefAarID=Observations$ReferenceYearID,
	nsim=1000,
	bootmat=bootmatall,
	ref.value.code=0,
	IndicatorID=Indic$IndicatorID,
	FK_Scalingmodel=Indic$Scalingmodel)

munweightsforall <- munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=Indic$TrophicgroupID,
	Key.indicators=Indic$Key.indicators,
	Fidelity=Indic$Fidelity,
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit)

NIresult1 <- NULL

for (i in years) {

	scaled.bootmatyeari <- scaled.bootmatall[Observations$ReferenceYearID == i,]

	NIi <- summary(NIcalculate(
		Areaind.Name=Indicator.area.ind$IndicatorareaID,
		Areaind.Indicator=Indicator.area.ind$Indicator_name,
		Area.Name=Indicator.area$IndicatorareaID,
		Area.Municipality=Indicator.area$Basicunit,
		FK_OmraadeID=Observations$IndicatorareaID[Observations$ReferenceYearID == i],
		nsim=1000,
		scaled.bootmat=scaled.bootmatyeari,
		Weights.trof=munweightsforall))

	year <- rep(i,dim(NIi) [1])
	iresult <- cbind(year,NIi)
	NIresult1 <- rbind(NIresult1,iresult)

}

NIresult1

## ----echo=FALSE, prompt=FALSE--------------------------------------------
Alt_DistID <- Observations$FK_DistID
Alt_DistID[Observations$ReferenceYearID > -1] <- "NoBoot"

bootmatall <- samplebootmat(
	ValueID=Observations$ValueID,
	Value=Observations$Expected.value,
	RefobsID = Observations$ReferenceYearID,
	DistID=Alt_DistID,
	mu=Observations$mu,
	sig=Observations$sig,
	nsim=1000)

scaled.bootmatall <- scaleobs(
	ValueID=Observations$ValueID,
	FK_OmraadeID=Observations$IndicatorareaID,
	FK_IndicatorID=Observations$IndicatorID,
	FK_RefAarID=Observations$ReferenceYearID,
	nsim=1000,
	bootmat=bootmatall,
	ref.value.code=0,
	IndicatorID=Indic$IndicatorID,
	FK_Scalingmodel=Indic$Scalingmodel)

munweightsforall <- munweights(
	Municipalities=BSunits$Basicunit,
	Indicators=Indic$Indicator_name,
	FK_TrophicgroupID=Indic$TrophicgroupID,
	Key.indicators=Indic$Key.indicators,
	Fidelity=Indic$Fidelity,
	Areaind.Name=Indicator.area.ind$IndicatorareaID,
	Areaind.Indicator=Indicator.area.ind$Indicator_name,
	Area.Name=Indicator.area$IndicatorareaID,
	Area.Municipality=Indicator.area$Basicunit)

NIresult1 <- NULL

for (i in years) {

	scaled.bootmatyeari <- scaled.bootmatall[Observations$ReferenceYearID == i,]

	NIi <- summary(NIcalculate(
		Areaind.Name=Indicator.area.ind$IndicatorareaID,
		Areaind.Indicator=Indicator.area.ind$Indicator_name,
		Area.Name=Indicator.area$IndicatorareaID,
		Area.Municipality=Indicator.area$Basicunit,
		FK_OmraadeID=Observations$IndicatorareaID[Observations$ReferenceYearID == i],
		nsim=1000,
		scaled.bootmat=scaled.bootmatyeari,
		Weights.trof=munweightsforall))

	year <- rep(i,dim(NIi) [1])
	iresult <- cbind(year,NIi)
	NIresult1 <- rbind(NIresult1,iresult)

}

NIresult1

## ----echo=FALSE, prompt=FALSE--------------------------------------------
NIresult - NIresult1

