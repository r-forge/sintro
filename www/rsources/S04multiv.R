###################################################
### chunk number 1: ch04pic1
###################################################
graphics.off()
quartz(width = 11, height = 11)# we need larger displays. change this for other displays

#\Stt{text.id()}: markiere Datapunkte \Stt{x, y} by Label.
	text.id <- function(x, y, ind, labels.id = rownames(x), adj.x = TRUE, 
          cex.id = 0.75, label.pos = c(4, 2)) {
          x <- x[ind];y <- y[ind];labels.id <- labels.id[ind]
	    labpos <- 
                if(adj.x) label.pos[1 + as.numeric(x > mean(range(x)))] else 3
	    text(x, y, labels.id, cex = cex.id, xpd = TRUE, 
		 pos = labpos, offset = 0.25)
	}



###################################################
### chunk number 2: volcimage
###################################################
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()
#title(main = "Maunga Whau Volcano", font.main = 4)


###################################################
### chunk number 3: volccontour
###################################################
contour(x, y, volcano, levels = seq(90, 200, by = 5),
         col = "peru", main = "Maunga Whau Volcano", font.main = 4)
#cat("contour done1")


###################################################
### chunk number 4: volcpersp
###################################################
#cat("start pers")
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
#cat("start persp1")
## Don't draw the grid lines :  border = NA
#par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)


###################################################
### chunk number 5:  eval=FALSE
###################################################
## x <- 10*(1:nrow(volcano))
## y <- 10*(1:ncol(volcano))
## image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
## axis(1, at = seq(100, 800, by = 100))
## axis(2, at = seq(100, 600, by = 100))
## box()
## title(main = "Maunga Whau Volcano", font.main = 4)
## 
## contour(x, y, volcano, levels = seq(90, 200, by = 5),
##          col = "peru", main = "Maunga Whau Volcano", font.main = 4)
## 
## z <- 2 * volcano        # Exaggerate the relief
## x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
## y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
## ## Don't draw the grid lines :  border = NA
## persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
##       ltheta = -120, shade = 0.75, border = NA, box = FALSE)


###################################################
### chunk number 7: volcwireframe
###################################################
library(lattice)
print(wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),    ## volcano  ## 87 x 61 matrix
          par.settings = list(axis.line = list(col = "transparent")),
          light.source = c(10,0,10)))


###################################################
### chunk number 8: 
###################################################
library(locfit)
data(chemdiab)


###################################################
### chunk number 9: 
###################################################
summary(chemdiab)


###################################################
### chunk number 10: ch04diabpairs
###################################################
pairs(~fpg + ga + ina + sspg, data = chemdiab, pch = 21, 
    main = "Diabetes-data", 
    bg = c("magenta", "green3", "cyan")[unclass(chemdiab$cc)], 
    oma = c(8, 8, 8, 8))
mtext(c("Colour codes:", levels(chemdiab$cc)), 
    col = c("black", "magenta", "green3", "cyan"), 
    at = c(0.1, 0.4, 0.6, 0.8), side = 1, line = 2)


###################################################
### chunk number 11: ch04diabcloud
###################################################
library("lattice")
diabcloud <- function(y, where, more = TRUE, ...) {
    print(cloud(ga ~ ina + sspg, data = chemdiab, groups = cc, 
        screen = list(x = -90, y = y), distance = .4, zoom = .6, 
        auto.key = TRUE, ...), 
        split = c(where, 3, 2), more = more)
}
supsym <- trellis.par.get("superpose.symbol")
supsymold <- supsym
supsym$col = c("magenta", "green3", "cyan")
trellis.par.set("superpose.symbol" = supsym)
diabcloud(y = 70, where = c(1, 1))
diabcloud(y = 40, where = c(2, 1))
diabcloud(y = 10, where = c(3, 1))
diabcloud(y = -20, where = c(1, 2))
diabcloud(y = -50, where = c(2, 2))
diabcloud(y = -80, where = c(3, 2), more = FALSE)
trellis.par.set("superpose.symbol" = supsymold)
rm(diabcloud, supsymold, supsym)


###################################################
### chunk number 12: ch04diabetesparallel
###################################################
library("lattice")
print(parallel(chemdiab[2:5], groups = chemdiab$cc))


###################################################
### chunk number 13: ch04quakescoplot1
###################################################
quakes$depth <- -quakes$depth 
given.depth <- co.intervals(quakes$depth, number = 4, overlap = .1)
coplot(lat ~ long | depth, data = quakes, given.values = given.depth, columns = 1)


###################################################
### chunk number 14: ch04quakescoplot2
###################################################
coplot(lat ~ long | mag* depth , data = quakes, number = c(5, 4))


###################################################
### chunk number 15: ch04quakesxyplot
###################################################
library("lattice")
Depth <- equal.count(quakes$depth, number = 4, overlap = .1)
print(xyplot(lat ~ long | Depth , data = quakes, columns = 1, layout = c(1, 4)))


###################################################
### chunk number 16: ch04irisplot
###################################################
oldpar <- par(mfrow = c(2, 2))
plot(iris$Species, iris$Petal.Length, 
    ylab = '', main = 'Petal Length', col = c("magenta", "green3", "yellow"))
plot(iris$Species, iris$Petal.Width, 
    ylab = '', main = 'Petal Width', col = c("magenta", "green3", "yellow"))
plot(iris$Species, iris$Sepal.Length, 
    ylab = '', main = 'Sepal Length', col = c("magenta", "green3", "yellow"))
plot(iris$Species, iris$Sepal.Width, 
    ylab = '', main = 'Sepal Width', col = c("magenta", "green3", "yellow"))
par(oldpar)


###################################################
### chunk number 17: ch04irisstrippl
###################################################
library("lattice")
print(stripplot(Petal.Length ~ Species, data = iris, 
    jitter = TRUE, ylab = '', main = 'Petal Length'), split = c(1, 1, 2, 2), more = TRUE)
print(stripplot(Petal.Width ~ Species, data = iris, 
    jitter = TRUE, ylab = '', main = 'Petal Width'), split = c(2, 1, 2, 2), more = TRUE)
print(stripplot(Sepal.Length ~ Species, data = iris, 
    jitter = TRUE, ylab = '', main = 'Sepal Length'), split = c(1, 2, 2, 2), more = TRUE)
print(stripplot(Sepal.Width ~ Species, data = iris, 
    jitter = TRUE, ylab = '', main = 'Sepal Width'), split = c(2, 2, 2, 2))


###################################################
### chunk number 18: ch04irispairst
###################################################
iris$Sepal.Area <- iris$Sepal.Length*iris$Sepal.Width
iris$Petal.Area <- iris$Petal.Length*iris$Petal.Width
iris$Sepal.Ratio <- iris$Sepal.Length/iris$Sepal.Width
iris$Petal.Ratio <- iris$Petal.Length/iris$Petal.Width
pairs(iris[6:9], main = "Anderson's Iris Data -- 3 species", 
      pch = 21, 
      bg = c("magenta", "green3", "yellow")[unclass(iris$Species)], 
      oma = c(8, 8, 8, 8))
mtext(c("Colour codes:", levels(iris$Species)), 
    col = c("black", "magenta", "green3", "yellow"), 
    at = c(0.1, 0.4, 0.6, 0.8), 
    side = 1, line = 2)


###################################################
### chunk number 19: ch04cuspplot
###################################################
y4 <- function(y, u = 0, v = 0) { y^4 + u*(y^2) + v*y}
#y4 <- function(y, u = 0, v = 0) { (y^4)/4 + u*(y^2)/2 + v*y}

#! fix ylab to show actual formula
curvey4 <- function(u = 0, v = 0, main = cat("u = ", u, "v = ", v), 
    from = min(-2, -2*abs(u), -2*abs(v)), 
    to = max( + 2, + 2*abs(u)), ...){

 	y4uv <- function(y){y4(y, u, v)}
	 
    curve(y4uv, from, to, xlab = "", 
        ylab = expression(paste("y^4 + ", u, "y^2 + ", v, "y")), 
        main = paste("u = ", u, "v = ", v), cex.main=2.0, ...)
 }

plomcurvey4 <- function(umax = 1, vmax = 1, steps = 3, ...){
opar <- par(mfrow = c(steps, steps))
for (i in (0:(steps-1))){
 for (j in (0:(steps-1))) {
  curvey4(-umax + i* 2*umax/(steps-1), -vmax + j* 2*vmax/(steps-1), ...)
} 
}
par(opar)
invisible(NULL)}


###################################################
### chunk number 20: ch04plomcusp
###################################################
#plomcurvey4(0, 0, steps = 5)
#plomcurvey4(3, 3, steps = 3
plomcurvey4(umax = 4, vmax = 3, steps = 5, from = -2, to = 2)
#plomcurvey4(umax = 4, vmax = 3, steps = 5, from = -20, to = 20)



###################################################
### chunk number 21: 
###################################################
v4yu <- function(y, u) -4*y**3 - 2*u*y 
#outer(-4:4, -2:2, v4yu)
steps <- 20 # 100
ymax <- 4
umax <- 20
y <- (-ymax + (0:steps)*2*ymax/steps)
u <- (-umax + (0:steps)*2*umax/steps)
v <- outer(y, u, v4yu)


###################################################
### chunk number 22: ch04cuspsurf1
###################################################
persp(x = u, y = y, z = v, zlab = "v", theta = 30, shade = 0.2)


###################################################
### chunk number 23: ch04cusp
###################################################
cusp <- function(v) { -exp(log(27/8*(v^2))/3)}
curve(cusp, from = -1, to = 1, ylim = c(-1.5, 1.5))


###################################################
### chunk number 24: ch04ecurve4
###################################################
ecurve4 <- function(u = 0, v = 0, std = TRUE, ...){
 d4uv <- function(y) {exp(-(y^4 + u*(y^2) + v*y))}
# if (std){
 n4uv <- integrate(d4uv, -Inf, Inf)
# cat ("Integral: ", "\n");n4uv
 
 d4uv <- function(y) {exp(-(y^4 + u*(y^2) + v*y))/n4uv$value}
#cat ("After standardization Integral: ");integrate(d4uv, -Inf, Inf)
 
 yd4uv <- function(y) {y *d4uv(y)}
 d4uvmean <- integrate(yd4uv, -Inf, Inf)	 # mean
 #cat ("Mean: ");d4uvmean
 
d4uv <- function(y) {y <- y + d4uvmean$value
exp(-(y^4 + u*(y^2) + v*y))/n4uv$value}
d4uvmean1 <- integrate(yd4uv, -Inf, Inf)
#cat ("After standardization Mean: "); d4uvmean1
  
y2d4uv <- function(y) {y*y *d4uv(y)}
d4uvvar <- integrate(y2d4uv, -Inf, Inf)
#cat("Variance: "); d4uvvar

d4uv <- function(y) {s <- sqrt(d4uvvar$value-d4uvmean1$value*d4uvmean1$value)
y <- (y + d4uvmean$value)*s
#cat(s, s**2, 1/s, 1/(s**s), "\n");
exp(-(y^4 + u*(y^2) + v*y))/n4uv$value
}
#cat ("After standardization Variance: "); integrate(y2d4uv, -Inf, Inf)
 n4uv1 <- integrate(d4uv, -Inf, Inf)
d4uv <- function(y) {s <- sqrt(d4uvvar$value-d4uvmean1$value*d4uvmean1$value)
y <- (y + d4uvmean$value)/s
#cat(s, s**2, 1/s, 1/(s**s), "\n");
exp(-(y^4 + u*(y^2) + v*y))/n4uv$value/ n4uv1$value
}
#cat ("After standardization Variance: "); integrate(y2d4uv, -Inf, Inf)


 d4uv <- function(y) {
 y <- d4uvvar$value*y + d4uvmean$value
exp(-(y^4 + u*(y^2) + v*y))/n4uv$value/(d4uvvar$value)
}

#checks
#cat ("After standardization Integral: ");integrate(d4uv, -Inf, Inf)

#cat ("Mean: ");integrate(yd4uv, -Inf, Inf)
y2d4uv <- function(y) {y*y *d4uv(y)}
#cat("Variance: "); integrate(y2d4uv, -Inf, Inf)
#}
 curve(d4uv, main = paste("u = ", u, "v = ", v), ...)
 #curve(d4uv, ...)
}

p4e <- function(umax, vmax, steps, ...){
par(mfrow = c(steps, steps))
for (i in (0:(steps-1))){
 for (j in (0:(steps-1))) {
  ecurve4(-umax + i* 2*umax/(steps-1), -vmax + j* 2*vmax/(steps-1), cex.main=2.0, ...)
} 
}
}


###################################################
### chunk number 25: ch04cuspdistr
###################################################

p4e(umax = 2, vmax = 1, steps = 5, from = -5, to = 5)


###################################################
### chunk number 26: MelbourneT
###################################################
library(lshorth)
melbourne3 <- data.frame(read.csv("/data/melbourne/temp v pressure 3 hourly intervals.csv"))
dt<-as.POSIXlt(melbourne3[,1])#lenght 9??

# 15h data
melbourne15h <- melbourne3[dt$hour==15,]
melbourne15h$TomorrowT <- c(melbourne15h[-1,2], NA)
#thigh <- c(32,99); tmed <- c(16,20); tlow <- c(0,13)
thigh <- c(32,99); tmed <- c(25.6,32); tlow <- c(21.7,25.6)

plotcond <- function(tlim,plim,main=NULL,...){
	incond <-melbourne15h[
	(melbourne15h[,2]>= tlim[1]) 	&
	(melbourne15h[,2]<= tlim[2]) 	& 
	(melbourne15h[,3]>= plim[1]) &
	(melbourne15h[,3]<= plim[2]),]
	diffT <- incond[,4]-incond[,2]
	diffT <- diffT[is.finite(diffT)]
	ls <- lshorth(diffT, probs=c( 0.125,0.25,0.5,0.75, 	0.875),plot=FALSE)
	if (is.null(main)){main=paste("T ",tlim,"p ",plim, sep=" ")}
	plot(ls, frame.plot=FALSE, main=main, cex=2.0, ...)
	}
oldpar <- par(mfrow=c(1,3))
#plotcond(thigh,c(0,9999),main=expression(paste(T>= 32,  bquote(.(thigh[1])),degree ,"C")), legend=NULL)

plotcond(tlow,c(0,9999),main=expression(paste(21.7, degree , "C" <=T,"" <= 25.6,  degree ,"C")),legend="bottom")
#plotcond(tmed,c(0,9999),main=expression(paste(25.6<=T, ",",T<= 32,  degree ,"C")), legend=NULL)
#plotcond(tmed,c(0,9999),main=expression(paste(25.6,  degree ,"C" <=T,T<= 32,  degree ,"C")), legend=NULL)
plotcond(tmed,c(0,9999),main=expression(paste(25.6,  degree ,"C" <=T, ""<= 32,  degree ,"C")), legend=NULL)

#plotcond(tmed,c(0,9999),main=expression(paste(25.6,  degree ,"C" <= "T" <= 32,  degree ,"C")), legend=NULL)
plotcond(thigh,c(0,9999),main=expression(paste(T>= 32,  degree ,"C")), legend=NULL)
par(oldpar)


###################################################
### chunk number 27: Melbourne
###################################################

qtemp <- quantile(melbourne15h[,2], probs=seq(0,1,0.125)) # 1..9
qpress <- quantile(melbourne15h[,3], probs=seq(0,1,0.125))

plotcond3 <- function(tlim,main=NULL,...){
	incond <-melbourne15h[
	(melbourne15h[,2]>= tlim[1]) 	&
	(melbourne15h[,2]<= tlim[2]) ,]
	qpress<-quantile(incond[,3], probs=seq(0,1,1/6),na.rm=TRUE)
	plotcond(tlim, plim=c(qpress[6],9999), main=paste("T: ", tlim[1], "...  ","C   p:", qpress[6],"...  ", "hpa", sep=""), legend=NULL)
	plotcond(tlim, plim=c(qpress[3],qpress[4]), main=paste("T: ", tlim[1], "...",tlim[2],"C   p:", qpress[3],"...",qpress[4], "hpa", sep=""), legend=NULL)
	plotcond(tlim, plim=c(0,qpress[2]), main=paste("T: ", tlim[1], "...",tlim[2],"C   p:", "...",qpress[2], "hpa", sep=""), legend=NULL)
	}

oldpar <- par(mfrow=c(3,3))
plotcond3(tlim=thigh)

plotcond3(tlim= tmed)

plotcond3(tlim= tlow)

par(oldpar)


###################################################
### chunk number 28: 
###################################################
library("UsingR")
data(fat)
fat$weightkg <- fat$weight*0.453 
fat$heightcm <- fat$height * 2.54
fat$ffweightkg <- fat$ffweight*0.453


###################################################
### chunk number 29: fatcheckpairs
###################################################
pairs(~body.fat + body.fat.siri + I(1/density), data = fat)


###################################################
### chunk number 30: fatcheckplot
###################################################
oldpar <- par(mfrow = c(2, 2))
plot(1/fat$density, fat$body.fat, ylab = "body fat", xlab = "1/density")
#identify(1/fat$density, fat$body.fat, fat$case, 6)
##$ind
##[1]  33  48  76  96 182 216
##$pos
##[1] 3 1 1 3 3 3
text.id(1/fat$density, fat$body.fat, c(33, 48, 76, 96, 182, 216), fat$case)

plot(1/fat$density, fat$body.fat.siri, ylab = "body fat siri", xlab = "1/density")
#identify(1/fat$density, fat$body.fat.siri, fat$case, 6)
##$ind
##[1]  48  76  96 169 182 216
##$pos
##[1] 2 1 1 1 4 1
text.id(1/fat$density, fat$body.fat.siri, c(48,  76,  96, 169, 182, 216), fat$case)

plot((1-fat$body.fat/100)*fat$weightkg, fat$ffweightkg, ylab = "ffweightkg", xlab = "(1-body.fat/100)*weightkg")
#identify((1-fat$body.fat/100)*fat$weightkg, fat$ffweightkg, fat$case, 3)
##$ind
##[1]  33  39 221
##pos
##[1] 1 4 1

text.id((1-fat$body.fat/100)*fat$weightkg, fat$ffweightkg, c(33,  39, 221), fat$case)

plot(fat$weightkg/(fat$heightcm/100)^2, fat$BMI, ylab = "BMI", xlab = "weightkg/(fat$heightcm/100)^2")
#identify(fat$weightkg/(fat$heightcm/100)^2), fat$BMI, fat$case, 4)
##$ind
##[1]  39  42 163 221
##$pos
##[1] 2 4 4 2
text.id(fat$weightkg/(fat$heightcm/100)^2, fat$BMI, c(39,  42, 163, 221), fat$case)

par(oldpar)


###################################################
### chunk number 31: 
###################################################
fat$height [42] <- 69.5
fat$heightcm[42] <- fat$height[42] * 2.54


###################################################
### chunk number 32: 
###################################################
fat$weight <- NULL
fat$height <- NULL
fat$ffweight <- NULL
fat$ffweightkg <- NULL
fat$body.fat.siri <- NULL


###################################################
### chunk number 33: fat01scat
###################################################
oldpar <- par(mfrow = c(1, 2))
plot(fat$weightkg-(fat$heightcm-100), fat$body.fat, 
    xlab = "weight-(height-100)", 
    ylab = "body.fat")
text.id(fat$weightkg-(fat$heightcm-100), fat$body.fat, c(39, 41, 216), labels.id = fat$case)
#identify(fat$weightkg-(fat$heightcm-100), fat$body.fat, fat$case, 3)
##$ind
##[1]  39 41 216
##$pos
##[1] 1 2 1
plot(fat$BMI, fat$body.fat, ylab = "body.fat", xlab = "BMI")
text.id(fat$BMI, fat$body.fat, c(39, 41, 216), fat$case)
#identify(fat$BMI, fat$body.fat, fat$case, 3)
##$ind
##[1]  39  41 216
##$pos
##[1] 1 4 3
par(oldpar)


###################################################
### chunk number 34: 
###################################################
lm.height <- lm(body.fat~I(weightkg-(heightcm-100)), 
    data = fat, 
    subset = -c(39, 41, 216))
summary(lm.height)


###################################################
### chunk number 35: 
###################################################
lm.BMI <- lm(body.fat~BMI, 
    data = fat, 
    subset = -c(39, 41, 216))
summary(lm.BMI)


###################################################
### chunk number 36: 
###################################################
lm.fullres <- lm(body.fat ~ age + BMI + neck + chest + 
    abdomen + hip + thigh + knee + ankle + 
    bicep + forearm + wrist + weightkg + heightcm, 
    data = fat)
summary(lm.fullres)


###################################################
### chunk number 37: 
###################################################
sel <- runif(dim(fat)[1])
fat$train <- sel < 2/3
rm(sel)


###################################################
### chunk number 38: 
###################################################
fat$train[c(39, 41, 216)] <- FALSE
summary(fat$train)


###################################################
### chunk number 39: 
###################################################
fat$vol <- fat$weightkg/fat$density


###################################################
### chunk number 40: 
###################################################
fat$neckvol <- fat$neck^2 * fat$heightcm
fat$chestvol <- fat$chest^2 * fat$heightcm
fat$abdomenvol <- fat$abdomen^2 * fat$heightcm
fat$hipvol <- fat$hip^2 * fat$heightcm

fat$thighvol <- fat$thigh^2 * fat$heightcm
fat$kneevol <- fat$knee^2 * fat$heightcm
fat$anklevol <- fat$ankle^2 * fat$heightcm
fat$bicepvol <- fat$bicep^2 * fat$heightcm

fat$forearmvol <- fat$forearm^2 * fat$heightcm
fat$wristvol <- fat$wrist^2 * fat$heightcm


###################################################
### chunk number 41: fatparallel
###################################################
print(parallel(fat[ -c(39, 41, 216), c(19:29,19:29)], col =  rgb(red=0, blue=0, green=0, alpha=0.2)))


###################################################
### chunk number 42: 
###################################################
pcfatvol <- prcomp(fat[, 20:29], subset = fat$train)
round(pcfatvol$rotation, 2)


###################################################
### chunk number 43: fatpbiplot
###################################################
biplot(pcfatvol, choices=4:5, 
    col=c("grey50","red"), 
    ylabs=c("nck","chst","abd","hip","thigh","kn","ank","bcps","farm","wr"), 
    cex=c(0.8,1.2))


###################################################
### chunk number 44: 
###################################################
lm.vol <- lm(vol ~ neckvol + chestvol + abdomenvol + 
    hipvol + thighvol + kneevol + 
    anklevol + bicepvol + forearmvol + 
    wristvol, 
    data = fat, subset = fat$train)
summary(lm.vol)  


###################################################
### chunk number 45: leaps
###################################################
library(leaps)
l1 <- leaps(x = fat[, c(6:15, 20:29)], y = fat$vol)
#print(l1)
#summary(l1)


###################################################
### chunk number 46: volf
###################################################
fat$neckvolf <- fat$neckvol / fat$weightkg
fat$chestvolf <- fat$chestvol / fat$weightkg
fat$abdomenvolf <- fat$abdomenvol / fat$weightkg
fat$hipvolf <- fat$hipvol / fat$weightkg

fat$thighvolf <- fat$thighvol / fat$weightkg
fat$kneevolf <- fat$kneevol / fat$weightkg
fat$anklevolf <- fat$anklevol / fat$weightkg
fat$bicepvolf <- fat$bicepvol / fat$weightkg

fat$forearmvolf <- fat$forearmvol / fat$weightkg
fat$wristvolf <- fat$wristvol / fat$weightkg


###################################################
### chunk number 47: volf
###################################################
lm.volf <- lm(body.fat ~  abdomenvolf  + wristvolf, data = fat, subset = fat$train)
summary(lm.volf)  


###################################################
### chunk number 48: eval
###################################################
fat.eval <- fat[fat$train == FALSE, ]
pred <- predict.lm(lm.volf, fat.eval, se.fit = TRUE)


###################################################
### chunk number 49: 
###################################################
cat("Generated by Sweave from:\\\\")
cat(chartr("$", " ", "\\verb + $Source: /u/math/j40/cvsroot/lectures/src/SIntro/Rintro/Rnw/S04multiv.Rnw.tex,v $ + \\\\"))
cat(chartr("$", " ", "\\verb + $Revision: 1.51 $ + \\\\"))
cat(chartr("$", " ", "\\verb + $Date: 2008/08/07 08:42:15 $ + \\\\"))
cat(chartr("$", " ", "\\verb + $name:  $ + \\\\"))
cat(chartr("$", " ", "\\verb + $Author: j40 $ + \\\\"))


