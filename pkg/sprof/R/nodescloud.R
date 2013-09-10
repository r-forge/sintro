#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/nodescloud.R', chdir = TRUE)
#! To Do
#!
nodescloud <- function(sprof, src, min.freq=3, 
	icol, col, ...)
{
if (require(wordcloud)){
	if (inherits(sprof,"sprof")) 
		{	if (missing(src)) src <- sprof$info$id
			nodes <- sprof$nodes 
		} else {
			if (missing(src)) src <- deparse(substitute(sprof))
			nodes <- sprof
		}
	if (missing(icol)) {
		if (is.null(nodes$icol))icol <- rkindex(nodes$self.time, maxindex=16) else
			icol <- nodes$icol
		}
	
	icol <- unclass(icol) # just to make sure...
	
	if (missing(col)) col <- terrain.colors(max(icol))
	
	wordcloud(nodes$name, 
		freq = nodes$total.time,
		min.freq=min.freq, 
		random.order=FALSE,
		rot.per=0.3,
		colors = col[icol],
		ordered.colors=TRUE, ...)
	title(sub=src, col.sub=grey(0.5),font=3)
	} else {
			frame()
			legend("center","package wordcloud \n is required for this plot")
		}
	}
