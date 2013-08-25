# %$HeadURL$
# %$Id$
# %$Revision$
# %$Date$
# %$Author$
# %$Name:$
#
# source('~/projects/rforge/sintro/pkg/sprof/R/asfactormodel.R', chdir = TRUE)
#
# setwd("")
#! To Do
#! clean up

asfactormodel <- function(x, factormodel) {
	
	if (is.factor(factormodel)) {
		levels <- levels(factormodel)
		labels <- labels(factormodel)
		ordered <- is.ordered(factormodel)
	} else if  (is.character(factormodel)) {
		levels <- 1: length(factormodel)
		labels <- factormodel
		ordered = is.ordered(factormodel)
	} else stop("xfactor: factormodel is of unknown type")
	
	if (is.list(x)) {
		sapply(x, 
			function(X){factor(unlist(X), 
				levels, 
				labels,
				ordered=ordered)}
			)
		} else {		
		factor(x, 
			levels, 
			labels,
			ordered=ordered)
	}
}
