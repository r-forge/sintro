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

barplot_s(x) #ok

barplot_s(x, decreasing=FALSE) #legend pos

barplot_s(x, lowtrim=0.1, hightrim=0.8) 

barplot_s(x, lowtrim=0.2, hightrim=0.9, trimlegend=FALSE)

barplot_s(x, colfun="grey")

barplot_s(x, colfun=heat.colors)
