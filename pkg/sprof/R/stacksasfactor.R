# $HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/R/stacksasfactor.R $
# %$Id: barplot_s.R 213 2013-08-19 19:48:17Z gsawitzki $
# %$Revision: 213 $
# %$Date: 2013-08-19 21:48:17 +0200 (Mon, 19 Aug 2013) $
# %$Author: gsawitzki $
# %$Name:$
#
# source('~/projects/rforge/sintro/pkg/sprof/R/stacksasfactor.R', chdir = TRUE)
#
# setwd("")
#! To Do
#! clean up


#\details
stacksasfactor <- function(sprof, sel, events)
{
	if (!missing(events) & !missing(sel) )  
		stop("stacksasfactor: stacks can be selected by index of by event, not both.")
	if (!missing(events)) sel <- sprof$profiles$data[events] else {
	 if (missing(sel)) sel <- seq(along=sprof$stacks$nodes)
	}
	 src <- sprof$stacks$nodes[sel]
	 asfactormodel(src, factormodel = as.character(sprof$nodes$name))
}
