#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# setwd("")
#! To Do
#!
library(sprof)


x <- matrix(c(
	1,1,1,2,2,
	3,3,4,4,4,
	5,5,6,6,7,
	8,9,9,0,0
),nrow=4, ncol =5, byrow=TRUE)
xrrle <- rrle(x)
xrrle

t(sapply(xrrle, inverse.rle))



xf <- as.factor(x)
xfi <- levels(xf)
xrrlef <- rrle(x)
xrrlef1 <- lapply(xrrlef, function(xl) {xl$values <- factor(xl$values, levels=xfi); xl})
xrrlef1

t(sapply(xrrlef, inverse.rle))
