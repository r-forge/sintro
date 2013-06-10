# $HeadURL$
# $Date$
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/summary_prof.R', chdir = TRUE)
#! To Do
#!

str_prof <- function(x){
		# Rprofdata <- list(firstline=firstline, 
		# nodes=nodenames, 
		# stacks= colluniques, 
		# stacksrenc =  stacksnode,
		# data= collencstacks, 
		# timesRLE
		# freq=table(collencstacks))

	cat("First line:", x$firstline,"\n")
	cat(length(x$data),"Sampling intervals ")
	if (length(x$timesRLE[1])==1) cat(" at",x$timesRLE[[2]],"micros\n") else {cat("in micros: ");print(x$timesRLE)}
	cat(length(x$nodes),"nodes in",length(x$stacks$stacks), "stacks\n")	
	cat(length(unique(x$stacks$stackleafnodes)),"Terminals ", "\n")

	roots <- unique(x$stacks$stackheadnodes)
	cat(length(roots), "Roots: ")
	print(table(x$nodes[x$stacks$stackheadnodes]))
	cat("\n", deparse(substitute(x)), " Structure: "); str(x, max.level=1)
	cat("\n","stacks Structure: "); str(x$stacks, max.level=1, vec.len=2)		
}# str_prof 


summary_nodes <- function(x){
	nrstacks <- nrow(x$stacks)
	nrnodes <- length(x$nodes)
	nrprofs <- length(x$data)
	ishead <- rep("-",nrnodes); ishead[x$stacks$stackheadnodes] <- "ROOT"
	isleaf <- rep("-",nrnodes); isleaf[x$stacks$stackleafnodes]  <- "LEAF"
	self.time <- rep(0,nrnodes); 
	total.time <- rep(0,nrnodes); 
	for (i in (1: nrstacks))
	{ whichn <- x$stacks$stackleafnodes[i]
		 self.time[whichn] <-  self.time[whichn]+x$stacks$refcount[i]
	whichn <- unlist(unique(x$stacksrenc[i]))
		total.time[whichn] <- total.time[whichn]+x$stacks$refcount[i]
	}
	nodes <- data.frame(shortname=abbreviate(x$nodes), 
	root= ishead, leaf=isleaf, 
	self.time=self.time, self.pct = self.time/nrprofs*100,
	total.time=total.time, total.pct= total.time/nrprofs*100)
	rownames(nodes)<- x$nodes
	nodes
}

summary_stacks <- function(x){
	nrstacks <- length(x)
	stacksdf <- data.frame(len=x$stacks$stacklength,
	refcount =x$stacks$refcount, 
	root=x$stacks$stackheadnodes,
	leafs=x$stacks$stackleafnodes)
	stacksdf
	}

summary_terminals<- function(x){
	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
}
	

summary_profiles <- function(x){
	if (is.null(x$id)) id <- paste("Profile Summary", date()) else id <- x$id
	len <- length(x$data)
	uniquestacks <- length(unique(x$data))
	runs <- length(rle(x$data)$lengths) 
	list(id=id, len=len, uniquestacks=uniquestacks, runs=runs)	
}

summary.sprof <- function(object, ...){
	summary_nodes(object)
	summary_stacks(object)
	summary_profiles(object)
	invisible(object)
}