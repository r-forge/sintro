#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# setwd("")
#! To Do
#!
library(sprof)

	res_lm <- sampleRprof(for (i in 1:1000) yy<- lm(runif(1000)~rnorm(1000)), runs=100)
	
	res_lm