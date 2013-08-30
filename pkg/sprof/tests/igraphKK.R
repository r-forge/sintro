#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

# source('~/projects/rforge/sintro/pkg/sprof/tests/igraphKK.R', chdir = TRUE)

# setwd("")
#! To Do
#!
library(sprof)
if (require("igraph")) {
	
data(sprof01lm)
sprof <- sprof01lm

as_igraph_sprof <- function(sprof, layoutfun, params=NULL,...){
	adj <- adjacency(sprof)
	adj[adj!=0] <-1
	sprof_igraph <- graph.adjacency(adj)
	sprof_igraph <- set.graph.attribute(sprof_igraph, "layout", 
		layoutfun(sprof_igraph,params=params,...),...)
	V(sprof_igraph)$color <- "yellow"
	E(sprof_igraph)$color <- "#0000FF20"
	E(sprof_igraph)$width <- c(1,2)
	return(sprof_igraph)
}

#<<>>=
sprof_igraph <- graph.adjacency(adjacency(sprof))
sprof_igraph <- set.graph.attribute(sprof_igraph, 
	"layout", layout.kamada.kawai(sprof_igraph))
V(sprof_igraph)$color <- "yellow"
#E(sprof_igraph)$width <- c(1,2)
E(sprof_igraph)$color <- "#0000FF40"
E(sprof_igraph)$width <- c(1,2)
# see 
#<<fig=TRUE, label=sprof_igraphkamada,width=8, height=8>>=
plot(sprof_igraph, 
	main=paste0("igraph kamada.kawai layout\n", sprof$info$id))
	legend("topleft", 
		legend= paste0("class: ",class(sprof_igraph)),
		bg="#FFFFE040",
		seg.len=0
		)
} else warning("could not load igraph", immediate=TRUE)
