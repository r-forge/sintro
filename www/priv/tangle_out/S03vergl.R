###################################################
### chunk number 1: ch03pic1
###################################################
graphics.off()
quartz(width = 11, height = 11)# we need larger displays. change this for other displays


###################################################
### chunk number 2: click1
###################################################
plot(x = runif(1), y = runif(1), 
    xlim = c(0, 1), ylim = c(0, 1), 
    main = "Please click on the circle", 
    xlab = '', ylab = '', 
    axes = FALSE, frame.plot = TRUE)
locator(1)


###################################################
### chunk number 3: 
###################################################
click1 <- function(){
    x <- runif(1);y <- runif(1)
    plot(x = x, y = y, xlim = c(0, 1), ylim = c(0, 1), 
        main = "Please click on the circle", 
        xlab = '', ylab = '', 
        axes = FALSE, frame.plot = TRUE)
    clicktime <- system.time(xyclick <- locator(1))
    list(timestamp = Sys.time(),
        x = x, y = y, 
        xclick = xyclick$x, yclick = xyclick$y, 
        tclick = clicktime[3])
}


###################################################
### chunk number 4: 
###################################################
options(digits = 4)


###################################################
### chunk number 5: 
###################################################
dx <- as.data.frame(click1())
dx <- rbind(dx, data.frame(click1()))
dx


###################################################
### chunk number 6: 
###################################################
options(digits = 7)


###################################################
### chunk number 7: 
###################################################
click <- function(runs = 1){
    dx <- data.frame(click1()) # start up
    for (i in (1:runs)){dx <- rbind(dx, data.frame(click1()))}
    dx <- dx[-1, ] #discard startup
    plot(0, 0, 
        main = paste(runs, " Clicks recorded"), 
        xlab = '', ylab = '', 
        axes = FALSE, frame.plot = TRUE)# clean up plotting area
    #drange <- range(dx$tclick[is.finite(dx$tclick)]); drange[1] <- 0
    #stripchart(dx$tclick, xlim = drange, main = paste(runs, 'Repetitions'), xlab = 'tclick')
    dx
}
#click20 <- click(20)
#write.table(click20n, "click20050416-1")


###################################################
### chunk number 8: clickecdf
###################################################
load("../data/click21prec.Rdata")
plot(ecdf(right$tclick), col.hor="blue", pch="[", xlim=c(0,5), main="Click Time", xlab="time [s]", ylab=expression(F[n]))
plot(ecdf(left$tclick), col.hor="green", pch="[", add=TRUE)
legend("topleft", col=c("blue","green"), legend=c("right hand ","left hand"), lty=c(1,1), bty="n", inset=0.05)
arrows(left$tclick[12]+0.6,0.5,left$tclick[12]+0.2,0.5)
legend(left$tclick[12]+0.65,0.54,"left hand" , bty="n")
arrows(right$tclick[12]-0.6,0.4,right$tclick[12]-0.2,0.4)
legend(right$tclick[12]-2,0.45,"right hand" , bty="n")


###################################################
### chunk number 9: ttestcrit
###################################################
n1<- 6; n2 <- 6
df <-  n1 + n2 -2
alpha <- 0.05
curve(pt(x,df=df),from=-5, to=5, ylab= expression(F[n]),
    main="t-Test: Critical Value")
abline(h=1-alpha, col="red")      # cut at upper quantile
abline(v=qt(1-alpha, df=df), lty=3, col="red") # get critical value
legend("topleft", legend=c("level","critical value"),
    lty=c(1,3),col="red", 
    bty="n", inset=c(0,0.2))


###################################################
### chunk number 10: ttestcritalt
###################################################
n1<- 6; n2 <- 6
df <-  n1 + n2 -2
alpha <- 0.05
curve(pt(x,df=df),from=-5, to=5, ylab= expression(F[n]),
    main="Central and Non-Central t-Distribution")
abline(h=1-alpha, col="red")      # cut at upper quantile
abline(v=qt(1-alpha, df=df), lty=3, col="red") # get critical value

n1 <- 5
n2 <- 5
n <- n1+n2
theta <- 2

ncp <- theta * sqrt(n1 * n2/(n1+n2))
mtext(paste("non-centrality",round(ncp,2)))

curve(pt(x,df=df, ncp=ncp), lty=2, add=TRUE, col="blue")
legend("topleft", legend=c("central t","non-central t"),
    lty=c(1,2), col=c("black","blue"), 
    bty="n", inset=c(0,0.2))



###################################################
### chunk number 11: ttestpow
###################################################
tpower <- function(n1, n2, alpha,...){
	df <-  n1 + n2 -2
	tlim <- qt(1-alpha,df=df)
	prob <- function(theta){
	    pt(tlim, df = df, 
	        ncp = theta * sqrt(n1 * n2/(n1+n2)),
	        lower.tail=FALSE)}
	curve(prob, 0, 5, xlab=expression(theta==mu[1]-mu[2]), ...)
	abline(h=alpha, col="red")
	} 
	
tpower(5, 5, 0.05, main="Power Function for Selected Sample Sizes")
tpower(10, 10, 0.05, add =TRUE, lty = 3)
tpower(100,100, 0.05, add =TRUE, lty = 4)
tpower(1000, 1000, 0.05, add =TRUE, lty = 5)
legend("bottomright", 
    lty=c(1,3,4,5), 
    legend=c("n1 = n2 =5", "n1 = n2 =10", "n1 = n2 =100","n1 = n2 =1000"), 
    inset=0.1, bty="n")


###################################################
### chunk number 12: 
###################################################
power.t.test(delta=2, 
    power=0.8,
    sig.level=0.01, 
    type="two.sample", 
    alternative="one.sided")


###################################################
### chunk number 13: 
###################################################
nsimul <- 300
n1<- 10; n2 <- 10
alpha <- 0.01 #nominal level
x <- 0
for (i in 1:nsimul) {
	if (t.test(exp(rnorm(n1)),exp(rnorm(n2)), 
	alternative="less", 
	var.equal = TRUE)$p.value < alpha){
	    x <- x+1} 
	 }
p <- x/nsimul
cat("estimated level p", p)	


###################################################
### chunk number 14: 
###################################################
prop.test(n=nsimul, x=x)


###################################################
### chunk number 15: 
###################################################
nsimul <- 300
n1<- 10; n2 <-10
alpha <- 0.01
x<-0
for (i in 1:nsimul) {
	if (t.test(exp(rnorm(n1)),exp(rnorm(n2, mean = 1)), 
	    alternative="less", 
	    var.equal = TRUE)$p.value < alpha){
	        x <- x+1} 
	 }
p <- x/nsimul
cat("estim p", p)	
prop.test(n = nsimul, x = x)


###################################################
### chunk number 16: 
###################################################
cat("Generated by Sweave from:\\\\")
cat(chartr("$", " ", "\\verb+$Source: /u/math/j40/cvsroot/lectures/src/SIntro/Rintro/Rnw/S03vergl.Rnw.tex,v $+\\\\"))
cat(chartr("$", " ", "\\verb+$Revision: 1.42 $+\\\\"))
cat(chartr("$", " ", "\\verb+$Date: 2008/08/05 20:04:10 $+\\\\"))
cat(chartr("$", " ", "\\verb+$name:  $+\\\\"))
cat(chartr("$", " ", "\\verb+$Author: j40 $+\\\\"))


