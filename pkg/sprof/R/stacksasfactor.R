# $HeadURL$
# %$Id$
# %$Revision$
# %$Date$
# %$Author$
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
