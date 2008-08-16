###################################################
### chunk number 1: ch04pic1
###################################################
#        1         2         3         4        5         6         7         8         9
#234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901
#   5   9   3   7  21   5   9  33   7  41   5
graphics.off()
quartz(width = 11, height = 11)# we need larger displays. change this for other displays


###################################################
### chunk number 2: Sframe02
###################################################
x <- runif(100)
plot(x)


###################################################
### chunk number 3: Rkernels
###################################################
kernels <- eval(formals(density.default)$kernel) #$

## show the kernels in the R parametrisation
plot (density(0, bw = 1), xlab = "", 
      main = "R's density() kernels with bandwidth h = 1")
for(i in 2:length(kernels))
   lines(density(0, bw = 1, kernel =  kernels[i]), col = i)
legend(1.5, .4, legend = kernels, col = seq(kernels), 
       lty = 1, cex = .8, y.intersp = 1)


###################################################
### chunk number 4: 
###################################################
x <- runif(100)
plot(sort(x), 1:length(x)/length(x))
abline(0, 1)


###################################################
### chunk number 5: edf1-3
###################################################
x <- runif(100)
plot(sort(x), (1:length(x))/length(x), 
    xlab = "x", ylab = expression(F[n]), 
    main = "Empirical distribution function\n (X uniform)"
)
abline(0, 1)


###################################################
### chunk number 6: a01ecdf
###################################################
plot(ecdf( runif(10) ), pch = '[', 
    main = "Empirical distribution function\n (X uniform)", 
    xlim = c(0, 1), ylab = expression(F[n](x)))


###################################################
### chunk number 7: whist
###################################################
x <- runif(100)
hist(x)
rug(x)


###################################################
### chunk number 8: histker
###################################################
hist(x, probability = TRUE)
rug(x)
lines(density(x))


###################################################
### chunk number 9: 
###################################################
x <- runif(100)
whist <- hist(x)
whist


###################################################
### chunk number 10: notenbar
###################################################
grades <- c(2, 1, 3, 4, 2, 2, 3, 5, 1, 3, 4, 3, 6)
barplot(table(grades), xlab = 'Grade', ylab = 'Count', 
   ylim = c(0, max(table(grades))), 
   main = 'Grading')


###################################################
### chunk number 11: MC06
###################################################
x <- (sin(1:100)+1)/2                   # demo example only
y <- (1:length(x))/length(x)
plot(sort(x), y)
nrsamples <- 19                         # nor of simulations
samples <- matrix(data = runif(length(x)* nrsamples), 
nrow = length(x), ncol = nrsamples)
samples <- apply(samples, 2, sort)
envelope <- t(apply(samples, 1, range))
lines(envelope[, 1], y, col = "red")
lines(envelope[, 2], y, col = "red")


###################################################
### chunk number 12: 
###################################################
x  <- (sin(1:100)+1)/2	                # demo example only
y <- (1:length(x))/length(x)
nrsamples <- 19                                 # nr of simulations


###################################################
### chunk number 13: plotMC06a
###################################################
plot(sort(x), y, 
    main = paste("Monte Carlo Band: ", 
        bquote( .(nrsamples)), " Monte Carlo Samples"), 
    xlab = 'x', ylab = expression(F[n]))
samples <- matrix(data = runif(length(x) * nrsamples), 
    nrow = length(x), ncol = nrsamples)
samples <- apply(samples, 2, sort)
envelope <- t(apply(samples, 1, range))
lines(envelope[, 1], y, col = "red")
lines(envelope[, 2], y, col = "red")


###################################################
### chunk number 14: boxplot
###################################################
oldpar <- par(mfrow = c(1, 4))
boxplot(runif(100), main = "uniform")
boxplot(rnorm(100), main = "normal")
boxplot(exp(rnorm(100)), main = "lognormal")
boxplot(rcauchy(100), main = "cauchy")
par(oldpar)


###################################################
### chunk number 15: 
###################################################
unif50 <- runif(50)
unif100 <- runif(100)
norm50 <- rnorm(50)
norm100 <- rnorm(100)
lognorm50 <- exp(rnorm(50))
lognorm100 <- exp( rnorm(100))


###################################################
### chunk number 16: DistFF
###################################################
oldpar <- par(mfrow = c(2, 3))

plot(ecdf(unif50), pch = "[")
plot(ecdf(norm50), pch = "[")
plot(ecdf(lognorm50), pch = "[")

plot(ecdf(unif100), pch = "[")
plot(ecdf(norm100), pch = "[")
plot(ecdf(lognorm100), pch = "[")
par(oldpar)


###################################################
### chunk number 17: DistFQQ
###################################################
oldpar <- par(mfrow = c(2, 3))
qqnorm(unif50, main ="Normal Q-Q Plot\n unif50")
qqnorm(norm50, main = "Normal Q-Q\n norm50")
qqnorm(lognorm50, main = "Normal Q-Q\n lognorm50")

qqnorm(unif100, main = "Normal Q-Q\n unif100")
qqnorm(norm100, main = "Normal Q-Q\n norm100")
qqnorm(lognorm100, main = "Normal Q-Q\n lognorm100")

par(oldpar)


###################################################
### chunk number 18: 
###################################################
qqnormx <- function(x, nrow = 5, ncol = 5, main = deparse(substitute(x))){
    oldpar <- par(mfrow = c(nrow, ncol))
    qqnorm(x, main = main)
    for (i in 1:(nrow*ncol-1)) 
     qqnorm(rnorm(length(x)), main = "N(0, 1)", xlab="", ylab="")
    par(oldpar)
}


###################################################
### chunk number 19: ch01qqnormx
###################################################
qqnormx(runif(100), nrow=4, ncol=4)


###################################################
### chunk number 20: 
###################################################
ppdemo <- function (x, samps  =  19) {     # samps: nr of simulations
    y <- (1:length(x))/length(x)
    plot(sort(x), y, xlab = substitute(x), ylab = expression(F[n]), 
        main = "Distribution Function with Monte Carlo Bands (unif.)", 
        type = "s")
    mtext(paste(samps, "Monte Carlo Samples"), side = 3)
    samples <- matrix(runif(length(x)* samps), 
        nrow = length(x), ncol = samps)
    samples <- apply(samples, 2, sort)
    envelope <- t(apply(samples, 1, range))
    lines(envelope[, 1], y, type = "s", col = "red");
    lines(envelope[, 2], y, type = "s", col = "red")
} 


###################################################
### chunk number 21: 
###################################################
z100 <- runif(100)
ppdemo(z100)


###################################################
### chunk number 22: 
###################################################
ppdemo


###################################################
### chunk number 23: 
###################################################
circlearea <- function( r) r^2 * pi
circlearea(1:4)


###################################################
### chunk number 24: AnnotPlot
###################################################
plot(1:10, xlab = "xlab", ylab = "ylab", main = "main", sub = "sub")
mtext("mtext 1", side = 1, col = "blue")
mtext("mtext 2", side  = 2, col = "blue")
mtext("mtext 3", side  = 3, col = "blue")
mtext("mtext 4", side  = 4, col = "blue")
legend("topleft", legend = "topleft legend")
legend("center", legend = c("lty =1", "lty =2", "lty =3"), 
    lty =  1:3, title = "center legend")


###################################################
### chunk number 25: needle
###################################################
NeedleInTheHayStack <- function(nn, p=0.1, col="black", ... ) {
    oldpar <- par(mfrow=c(1,length(nn)))
    on.exit(par(oldpar))
    for (n in nn){
        nhay <- n-round(p*n); xhay <- runif(nhay);  yhay <- runif(nhay)
        needle <- runif(round(p*n))
        plot( x = c(xhay, needle), y = c(yhay,needle), 
            main = paste("n = ", n, ",", " p = ", p), cex.main=3.0,
            axes=FALSE, frame.plot=TRUE,
            xlab="", ylab="",
            col= col, ...)
        }
}
NeedleInTheHayStack( c(40, 200,1000, 5000, 25000) )


###################################################
### chunk number 26: needle40
###################################################
NeedleInTheHayStack(40)


###################################################
### chunk number 27: needle200
###################################################
NeedleInTheHayStack(200)


###################################################
### chunk number 28: needle1000
###################################################
NeedleInTheHayStack(1000)


###################################################
### chunk number 29: needle5000
###################################################
NeedleInTheHayStack(5000)


###################################################
### chunk number 30: needle25000
###################################################
NeedleInTheHayStack(25000)


###################################################
### chunk number 31: needlealpha
###################################################
NeedleInTheHayStack( 25000, col =  rgb(red=0, blue=0, green=0, alpha=0.01))


###################################################
### chunk number 32: 
###################################################
cat("Generated by Sweave from:\\\\")
cat(chartr("$", " ", "\\verb+$Source: /u/math/j40/cvsroot/lectures/src/SIntro/Rintro/Rnw/S01base.Rnw.tex,v $+\\\\"))
cat(chartr("$", " ", "\\verb+$Revision: 1.52 $+\\\\"))
cat(chartr("$", " ", "\\verb+$Date: 2008/08/06 19:17:13 $+\\\\"))
cat(chartr("$", " ", "\\verb+$name:  $+\\\\"))
cat(chartr("$", " ", "\\verb+$Author: j40 $+\\\\"))


