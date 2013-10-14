# $HeadURL$
# $Id$
# setwd("")
#! To Do
#!
# source('~/projects/rforge/sintro/pkg/sprof/R/rkindex.R', chdir = TRUE)

# create index from data, with gamma correction
rkindex <- function(x, 
	maxindex=length(x), 
	pwr=1, 
	ties.method="random",
	id ) {
		if (is.null(x)) stop("rkindex: x must not be NULL. Got ", deparse(substitute(x)))
		ix <- rank(x, ties.method=ties.method)-1
		rg <- range(ix)
		if (rg[2]>0) ix <- (ix/rg[2])^pwr
		ix <- ix*(maxindex-1) +1
		ix <- round(ix)
		
		
		if (missing(id)) {
			src<-deparse(substitute(x))
			if (pwr==1) pwr<-NULL else pwr <- paste0(", pwr=",pwr)
			id <- paste0(src, pwr)
			}
		attr(ix,"id") <- id;	 
		return(ix)
}
