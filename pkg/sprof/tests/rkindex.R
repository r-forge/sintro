#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# setwd("")
#! To Do
#!
#rank(x, na.last = TRUE,
# ties.method = c("average", "first", "random", "max", "min"))


library(sprof)

	x7 <- 1:7
	str(rkindex(x7))
	
	str(rkindex(-x7))
	
	str(rkindex(factor(x7)))
	
	str(rkindex(x7, pwr=2))
	
	str(rkindex(x7, pwr=0.5, id="tst"))

	str(rkindex(1))
	
	try(rkindex(NULL))
	
	str(rkindex(c(1,1,1)))
	
	str(rkindex(c(1,1,1), ties.method="min"))
