#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/tests/rrle.R $
#$Id: rrle.R 178 2013-07-23 19:13:13Z gsawitzki $
#$Revision: 178 $
#$Date: 2013-07-23 21:13:13 +0200 (Tue, 23 Jul 2013) $
#$Author: gsawitzki $

# setwd("")
#! To Do
#!
library(sprof)

	res_lm <- sampleRprof(for (i in 1:1000) yy<- lm(runif(1000)~rnorm(1000)), runs=100)
	
	res_lm