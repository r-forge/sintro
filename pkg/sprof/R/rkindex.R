# $HeadURL$
# $Id$
# setwd("")
#! To Do
#!
# source('~/projects/rforge/sintro/pkg/sprof/R/rkindex.R', chdir = TRUE)

# recursive run length encoding of a matrix by row, top down
rkindex <- function(x, 
	maxindex=length(x), 
	pwr=1, 
	ties.method="random" ) {
		ix <- rank(x, ties.method=ties.method)
		rg <- range(ix)
		ix <- (ix/rg[2])^pwr
		ix <- ix*(maxindex-1) +1
		ix <- round(ix)
		return(ix)
}
