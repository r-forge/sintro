# %$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/R/asfactormodel.R $
# %$Id: asfactormodel.R 222 2013-08-25 12:03:07Z gsawitzki $
# %$Revision: 222 $
# %$Date: 2013-08-25 14:03:07 +0200 (Sun, 25 Aug 2013) $
# %$Author: gsawitzki $
# %$Name:$
#
# source('~/projects/rforge/sintro/pkg/sprof/R/asfactormodel.R', chdir = TRUE)
#
# setwd("")
#! To Do
#! clean up

asfactormodel <- function(x, factormodel) {

# for a factor, the labels of the levels are called levels, 
#	and the level indicators are called labels.	
	if (is.factor(factormodel)) {
		labels <- levels(factormodel)
		levels <- labels(factormodel)
		ordered <- is.ordered(factormodel)
	} else if  (is.character(factormodel)) {
		levels <- 1: length(factormodel)
		labels <- factormodel
		ordered = is.ordered(factormodel)
	} else stop("xfactor: factormodel is of unsupported type")
	
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
