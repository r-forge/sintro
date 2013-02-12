OddOneOut <- function(goodplot = function(n, ...){plot(rnorm(n))}, 
	badplot = function(n, ...){plot(runif(n))},
	n=100, runs=10, nrows=4, ncols= 4, training=TRUE, ...){
cat("One plot is out. Please click on the odd panel in the plot. Abort with <esc>.\n")
cat("There are ",runs, "runs.\n")

restab <-matrix(nrow=runs, ncol=4)
colnames(restab) <- c("oddrow", "oddcol", "selrow", "selcol")
nrplots <- nrows*ncols
for (i in 1: runs){
if (training) {oldpar <- par(mfrow=c(nrows,ncols))} else  {oldpar <- par(mfrow=c(nrows,ncols), ann=FALSE, xaxt="n", yaxt="n")}
oddone <- sample(nrplots,1)-1
for (j in 1:nrplots-1) {   
	row <- (j %/% ncols) +1 #div
	col <- (j %% ncols) +1  #mod
	if (j==oddone) {badplot(n); badrow<- row; badcol <- col} else goodplot(n)
#	print(c(j,row,col))
}
par(mfrow=c(1,1))
plot.window(c(1,ncols),c(nrows,1)) # this is just a hack to get a convenient coordinate system
locres <- locator(1)	
restab[i,] <- c(badrow,badcol, round(locres$y), round(locres$x))
#print(c(i, "odd:",oddone,badrow,badcol, locres$y, locres$x, round(locres$y), round(locres$x)))
}
restab
}
