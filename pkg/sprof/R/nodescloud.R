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
nodescloud <- function(sprof,min.freq=3,icol, col){
if (require(wordcloud)){
	if (inherits(sprof,"sprof")) nodes <- sprof$nodes else nodes <- sprof
	if (missing(icol)) icol <- rkindex(nodes$self.time, maxindex=12)
	if (missing(col)) col <- terrain.colors(max(icol))
	
	wordcloud(nodes$name, 
		freq = nodes$total.time,
		min.freq=min.freq, 
		random.order=FALSE,
		rot.per=0.3,
		colors = col[icol],
		ordered.colors=TRUE)
	} else {
			frame()
			legend("center","package wordcloud \n is required for this plot")
		}
	}
