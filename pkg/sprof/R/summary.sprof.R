#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
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
	if (is.null(x)) {cat("## str_prof: NULL"); return(0)}
	cat("First line:", x$firstline,"\n")
	cat(length(x$data),"Sampling intervals ")
	if (length(x$timesRLE[1])==1) cat(" at",x$timesRLE[[2]],"micros\n") else {cat("in micros: ");print(x$timesRLE)}
	cat(length(x$nodes$name),"nodes in",length(x$stacks$stacks), "stacks\n")	
	cat(length(unique(x$stacks$stackleafnodes)),"Terminals ", "\n")

	roots <- unique(x$stacks$stackheadnodes)
	cat(length(roots), "Roots: ")
	print(table(x$nodes$name[x$stacks$stackheadnodes]))
	cat("\n", deparse(substitute(x)), " Structure: "); str(x, max.level=1)
	cat("\n","stacks Structure: "); str(x$stacks, max.level=1, vec.len=2)		
}# str_prof 


summary_nodes <- function(x){
	if (is.null(x)) {print("## summary_nodes: NULL"); return(NULL)}
	if (is.null(x$info)) {print("## summary_nodes: NULL info"); return(NULL)}
	nrstacks <- x$info$nrstacks
	if (nrstacks==0) {print("## summary_nodes: no stacks"); return(NULL)}
	nrnodes <- x$info$nrnodes
	if (nrnodes ==0) {print("## summary_nodes: no nodes"); return(NULL)}
	nrprofs <- x$info$nrrecords
	if (nrprofs ==0) {print("## summary_nodes: no records"); return(NULL)}
	
	ishead <- rep("-",nrnodes); ishead[x$stacks$stackheadnodes] <- "ROOT"
	isleaf <- rep("-",nrnodes); isleaf[x$stacks$stackleafnodes]  <- "LEAF"
	self.time <- rep(0,nrnodes); 
	total.time <- rep(0,nrnodes); 
	for (i in (1: nrstacks))
	{ whichn <- x$stacks$stackleafnodes[i]
		 self.time[whichn] <-  self.time[whichn]+x$stacks$refcount[i]
	whichn <- unlist(unique(x$stacks$nodes[i]))
		total.time[whichn] <- total.time[whichn]+x$stacks$refcount[i]
	}
	nodes <- data.frame(shortname=abbreviate(x$nodes$name), 
	root= ishead, leaf=isleaf, 
	self.time=self.time, self.pct = self.time/nrprofs*100,
	total.time=total.time, total.pct= total.time/nrprofs*100)
	rownames(nodes)<- x$nodes$name
	nodes
} # summary_nodes

summary_stacks <- function(x){
	if (is.null(x))return(NULL)
	if (is.null(x$stacks)) return(NULL)
	if (is.null(x$stacks$nodes)) return(NULL)
	stacks_nodes <- list.as.matrix(x$stacks$nodes)
	list(nrstacks = x$info$nrstacks,
	stacklength = range(x$stacks$stacklength),
	nrnodesperlevel = apply(stacks_nodes,1, function(X){sum(!is.na(unique(X)))})
	)
	#nrstacks <- length(x)
	# stacksdf <- data.frame(len=x$stacks$stacklength,
	# refcount =x$stacks$refcount, 
	# root=x$stacks$stackheadnodes,
	# leafs=x$stacks$stackleafnodes)
	# stacksdf
	} # summary_stacks

summary_terminals<- function(x){
	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
}
#stacks_nodes <- list.as.matrix(sprof$stacks$nodes)
#sum(!is.na(unique(stacks_nodes[1,])))	
#apply(stacks_nodes,1, function(x){sum(!is.na(unique(x)))})
summary_profiles <- function(x){
	if (is.null(x)) {return(0)}
	if (is.null(x$profiles)) return(NULL)
	if (is.null(x$id)) id <- paste("Profile Summary", date()) else id <- x$id
	len <- length(x$profiles$data)
	uniquestacks <- length(unique(x$profiles$data))
	runs <- length(rle(x$profiles$data)$lengths) 
	list(id=id, len=len, uniquestacks=uniquestacks, nr_runs=runs)	
} # summary_profiles

summary.sprof <- function(object, ...){
	print(summary_profiles(object))
	print(summary_stacks(object))
	print(summary_nodes(object))
	#invisible(object)
}
