#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$
#
# source('~/projects/rforge/sintro/pkg/sprof/R/barplot_s.R', chdir = TRUE)
#

# convert list to matrix
# list entries go to matrix columns, filled for equal length 
library(sprof)

n <- 32
x <- runif(n)

oldpar <- par(mfrow=c(3,2))

#1
barplot_s(x) #ok

#2
barplot_s(x, decreasing=FALSE) #legend pos

names(x) <- rownames(x,do.NULL=FALSE, prefix="V")

#3
barplot_s(x, lowtrim=0.1, hightrim=0.8) 

#4
barplot_s(x, lowtrim=0.2, hightrim=0.9, trimlegend=FALSE)

#5
barplot_s(x, colfun="grey")

#6
barplot_s(x, colfun=heat.colors, xpd=FALSE) # xpd not honoured

par(oldpar)
