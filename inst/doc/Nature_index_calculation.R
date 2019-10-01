## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

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


