###################################################
### chunk number 1: ch02pic1
###################################################
graphics.off()
quartz(width = 11, height = 11)# we need larger displays. change this for other displays


###################################################
### chunk number 2: 
###################################################
x <- 1:100
err <- rnorm(100, mean = 0, sd = 10)
y <- 2.5*x + err


###################################################
### chunk number 3: 
###################################################
lm(y ~ x)


###################################################
### chunk number 4: 
###################################################
lmres <- lm(y ~ x)
plot(x, y)
abline(lmres)


###################################################
### chunk number 5: bsp02summary
###################################################
summary(lm( y ~ x))


###################################################
### chunk number 6: 
###################################################
hiddenplot <- function(){
opar <- par(mfrow = c(2, 2))
plot(lm(y ~ x))
par(opar)}


###################################################
### chunk number 7: expl02-plotlm
###################################################
plot(lm(y ~ x))


###################################################
### chunk number 8: 
###################################################
hiddenplot()


###################################################
### chunk number 9: 
###################################################
summary(aov(lmres))


###################################################
### chunk number 10: 
###################################################
n <- 100
sigma <- 1
x <- (1:n)/n-0.5
err <- rnorm(n) 
y <- 2.5 * x + sigma*err 
lmxy <- lm(y ~ x)


###################################################
### chunk number 11: S02plotlim
###################################################
plotlim <- function(x){
    xlim <- range(x)
    # check implementation of plot. is this needed?
    del <- xlim[2]-xlim[1]
    if (del>0) 
        xlim <- xlim+c(-0.1*del, 0.1*del) 
    else xlim <- xlim+c(-0.1, 0.1)
    return(xlim)
}
xlim <- plotlim(x)
ylim <- plotlim(y)

newx <- data.frame(x = seq(xlim[1], xlim[2], 1/(2*n)))


###################################################
### chunk number 12: Scheffe
###################################################
plot(x, y, xlim = xlim, ylim = ylim)
abline(lmxy)
pred.w.plim <- predict(lmxy, newdata = newx, interval = "prediction")
pred.w.clim <- predict(lmxy, newdata = newx, interval = "confidence")
matplot(newx$x, 
    cbind(pred.w.clim[, -1], pred.w.plim[, -1]), 
    lty = c(2, 2, 6, 6), 
    col = c(2, 2, 4, 4), 
    type = "l", add = TRUE)
title(main = "Simultaneous Confidence")
legend("topleft", 
    lty = c(2, 6), 
    legend = c("confidence", "prediction"), 
    col = c(2, 4), 
    inset = 0.05, bty = "n")


###################################################
### chunk number 13: 
###################################################
p35 <- read.delim("../data/p35.tab")


###################################################
### chunk number 14: 
###################################################
s35 <- stack(p35[,3:9])                                       # ignore column H
s35 <- data.frame(y=s35$values, 
    Tmt=s35$ind,
    Lane=rep(1:12, length.out=dim(s35)[1]))    # rename
lmres <- lm(y ~ 0+ Tmt, data= s35)                   # we do not want an overall mean


###################################################
### chunk number 15: 
###################################################
summary(lmres)


###################################################
### chunk number 16: 
###################################################
anova(lmres)


###################################################
### chunk number 17: 
###################################################
library(multcomp)

lhtres<-glht(lmres,linfct=mcp(Tmt="Tukey"))
summary(lhtres)	# muliple tests


###################################################
### chunk number 18: titre
###################################################
oldpar <- par(mfrow=c(2,2))
plot(lmres)
par(oldpar)


###################################################
### chunk number 19: 
###################################################
library(MASS)
s35$studres <- studres(lmres)
s35[s35$studres < -1,]


###################################################
### chunk number 20: blueyellow4
###################################################
blueyellow4.colors <-
function (n=100, rev=FALSE)
{
	if ((n <- as.integer(n[1])) > 0) {
		n<-n-1
		if (rev) {
		q<-((n:0)/n  -0.5) *2
		}
		else {
		q<-((0:n)/n  -0.5) *2
		}
		qq <- ((q*q*q*q*sign(q)+1)/2)
		q1<- 1- ((q*q*q*q+1)/2)
		rgb(qq+q1,qq+q1, 1-qq+q1)

    	}
    else character(0)
}

oldpar <- par(yaxt="n")
image(x=1:12, z=matrix(1:12,ncol=1), 
    col=blueyellow4.colors(12), 
    xlab="",  main="Colour Codes for Rank")
par(oldpar)


###################################################
### chunk number 21: 
###################################################
parasp<-
# set aspect ratio match data matrix or given aspect ratio
# usage: opar<-par(no.readonly=TRUE); on.exit(par(opar)); parasp(dat)
function(dat, aspr=dim(dat)[1]/dim(dat)[2], tol=0.01)
{	if (aspr<= 0) stop("parasp: aspr must be positive")
	pin<-par("pin")
	ar <- pin[1]/pin[2]
	if (abs(ar/aspr)>tol) {
		if (ar < aspr) 
			pin[2] <- pin[1]/aspr  else pin[1] <- pin[2]* aspr
		par(pin=pin)
	}
}


###################################################
### chunk number 22: 
###################################################
imagem <- 
# a variant of image.default keeping matrix orientation
function (z, zlim = range(z[is.finite(z)]), xlim = c(1,ncol(z)), 
    ylim = c(1,nrow(z)), col = heat.colors(12),
    add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,main,
    breaks, oldstyle=FALSE,
    names=TRUE, coloffs=-1, rowoffs=4,...)
{ textnames <-
    function (zi, coloffs=-1, rowoffs=NULL) {
    # note: image interchanges rows/colums
    for (x in (1:dim(zi)[1]) ) # column labels
        text(x, ncol(zi)+0.5, rownames(zi)[x], pos=3, 
        xpd=NA, offs= coloffs, srt=270)
		
    r <- par("usr")[2]
    for (y in (1:dim(zi)[2]))  # row labels
        text(r, y, colnames(zi)[y], pos=4, xpd=NA, offs=rowoffs,srt=0)
    } # textnames
	
    zi <- t(z)
    opin <- par("pin"); on.exit(par(pin=opin))
    parasp(zi)
    image(
    1:nrow(zi),1:ncol(zi), zlim=zlim,
    #xlim=xlim,
    ylim=c(ncol(zi)+0.5,0.5), 
    col=col, add=add, xaxs=xaxs, yaxs=yaxs,
    xlab="", ylab="",z=zi, 
    main=main,
    breaks=breaks, oldstyle=oldstyle,
#		frame.plot=FALSE,	
    ...)
    if (names) {
        textnames(zi,coloffs=-4,rowoffs=1)
    } # names
}#imagem


###################################################
### chunk number 23: titrekplot1
###################################################

a35 <- as.matrix( p35[3:10] )
a35rk <- apply(a35, 2, rank)

imagem(t(a35rk), col=blueyellow4.colors(10), main="p35")


###################################################
### chunk number 24: expl02loessexpl
###################################################
x <- runif(50) * pi
y <- sin(x)+rnorm(50)/10


###################################################
### chunk number 25: expl02loess
###################################################
plot(x, y)
abline(lm(y ~ x), lty = 3, col = "blue")
lines(loess.smooth(x, y), lty = 6, col = "red")
legend("topleft", 
    legend = c("linear", "loess"), 
    lty = c(3, 6), col = c("blue", "red"), bty = "n")


###################################################
### chunk number 26: 
###################################################
plot


###################################################
### chunk number 27: 
###################################################
cat("Generated by Sweave from:\\\\")
cat(chartr("$", " ", "\\verb+$Source: /u/math/j40/cvsroot/lectures/src/SIntro/Rintro/Rnw/S02reg.Rnw.tex,v $+\\\\"))
cat(chartr("$", " ", "\\verb+$Revision: 1.52 $+\\\\"))
cat(chartr("$", " ", "\\verb+$Date: 2008/08/07 08:42:15 $+\\\\"))
cat(chartr("$", " ", "\\verb+$name:  $+\\\\"))
cat(chartr("$", " ", "\\verb+$Author: j40 $+\\\\"))


