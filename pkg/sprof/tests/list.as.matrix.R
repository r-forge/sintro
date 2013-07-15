#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# convert list to matrix
# list entries go to matrix columns, filled for equal length 
library(sprof)

x <- list(x1=c(1,2,3),
	x2=3, 
	x3=4:8)

list.as.matrix(x)
list.as.matrix(x,filler=0)

xf <- list(x1=factor(c(1,2,3)),
	x2=factor(3, levels =c(1,2,3,4), labels=c("L1","L2", "L3", "L4")),
	x3=4:8)
	
list.as.matrix(xf)
list.as.matrix(xf,filler=0)

# c( factor(c(1,2,3)), factor(3, levels =c(1,2,3,4), labels=c("L1","L2", "L3", "L4")))
#[1] 1 2 3 3
# str(c( factor(c(1,2,3)), factor(3, levels =c(1,2,3,4), labels=c("L1","L2", "L3", "L4"))))
# int [1:4] 1 2 3 3

list.as.matrixt <- function(x, filler=NA){
	maxlen <- max(sapply(x,length))
    lapply(x, 
	function(xs){fillen <- maxlen-length(xs)
		if (fillen>0) {c(xs,rep(filler,fillen))} else xs
	})
}

list.as.matrixt(xf)
list.as.matrixt(xf,filler=0)
