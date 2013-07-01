# $HeadURL:$

#\name{adjacency}
#\alias{stackstoadj}
#\title{Adjacency matrix}
#\desciption{Build an adjacency matrix from profile information}

#\details
stackstoadj <-  function(xstacks, xfreq, maxnode)
{
	xs <- list.as.matrix(xstacks)
	nrstacks <- ncol(xs)
	if (missing(maxnode)) maxnode <- max(xs,na.rm=TRUE)
	if (missing(xfreq)) xfreq <- rep(1, nrstacks)
	to <- xs[-1,]; xs <- xs[-ncol(xs),]
	adj <- matrix(0, nrow= maxnode, ncol= maxnode)   #from to
	for (i in (1:nrow(to))){ 
		for  (j in (1:nrstacks)) {
		if (!is.na(to[i,j]) & !is.na(xs[i,j]) ) adj[xs[i,j], to[i,j] ] <- adj[xs[i,j], to[i,j] ]+ xfreq[j]
		}
		}
	adj
}


# xstacks <- list(c(1,2,3), c(2,4), c(1,3))
# xa <- stackstoadj(xstacks)
# dimnames(xa) <-list(from=c("F1","F2","F3","F4"),
		 # to=c("T1","T2","T3","T4"))

#\name{adjacency}
#\alias{adjacency}
#\title{Adjacency matrix}
#\desciption{Build an adjacency matrix from profile information}

adjacency <- function(sprof, keep.names=TRUE) {
	adj <-stackstoadj(sprof$stacks$nodes,sprof$stacks$refcount,sprof$info$nrnodes)
	if (keep.names) dimnames(adj) <-list(from=sprof$nodes$name, to=sprof$nodes$name)
	adj
}

# edgelist <- function(sprof) {
# 	adj <- adjacency(sprof)
# 	nredges <- sum(!is.na(adj))
# 	edges <- matrix(,nredges, ncol, 3) #1:from 2:to #3:: freq
# 	
# }
#library(network)
#nwsa <- as.network(xa) # names is not imported tp nwsa
#network.vertex.names(nwsa) <- c("T1","T2","T3","T4")
#plot(nwsa, label= c("T1","T2","T3","T4"))
