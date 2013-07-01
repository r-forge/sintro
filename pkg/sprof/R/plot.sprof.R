# $HeadURLs:$
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/plot_prof.R', chdir = TRUE)
#! To Do
#!



plot_nodes <- function(x, which=c(1L,2L, 3L), col=NULL, 
	ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
	src=NULL,
	...){## full data
	if (inherits(x,"sprof")) {
		xnodes <- x$nodes
		if (is.null(src)) src<-x$info$id
		}	else if (inherits(x,"sprof_nodes")) {## qualified node data
		xnodes <- x
		if (is.null(src))  src<-deparse(substitute(x))
		} else {## raw data
		xnodes <- x	
		if (is.null(src))  src<-deparse(substitute(x))
	}
	
	if (is.null(col)) col <- terrain.colors(length(xnodes))
	oldpar <- par(no.readonly = TRUE)
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }

	orderself <- order(xnodes$self.time,decreasing=TRUE)
	ordertotal<- order(xnodes$total.time,decreasing=TRUE)
	for (ip in which){
		switch(ip,
		plot(xnodes$self.time, xnodes$total.time, xlab="self", ylab="total", sub=src), 
		barplot(xnodes[orderself,]$self.time,
			main="Nodes: time at end of stack",
			names.arg = xnodes[orderself,]$name, sub=src), 
		barplot(xnodes[ordertotal,]$total.time, 
			main="Nodes: total time in stack",
			names.arg = xnodes[ordertotal,]$name, sub=src) 
		)
	}
		# plot(xnodes$self.time, xnodes$total.time)
	
	# barplot(sort(xnodes$self.time, decreasing=TRUE),
	# main="self",names.arg = xnodes$name)
	
	# barplot(sort(xnodes$total.time, decreasing=TRUE),
	# main="total")
	par(oldpar)
	invisible(xnodes)
}# plot_nodes

plot_stacks <- function(x,which=c(1L, 2L),ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
		src=NULL,
		...){
		if (inherits(x,"sprof")) {
		xstacks <- x$stacks
		if (is.null(src)) src<-x$info$id
		}	else if (inherits(x,"sprof_stacks")) {## qualified stacks data
		xstacks <- x
		if (is.null(src))  src<-deparse(substitute(x))
		} else {## raw data
		xstacks <- x	
		if (is.null(src))  src<-deparse(substitute(x))
	}

	#ss <- summary_stacks(xstacks)
	
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
	}
 
 	for (ip in which){
		switch(ip,
		plot(xstacks$refcount, xstacks$stacklength, xlab="refcount", ylab="stack length", sub=src), 
		{ ordercnt  <- order(xstacks$refcount, decreasing=TRUE); id <-seq(along.with=xstacks$refcount)
			xstacks$id <-  id
			barplot(xstacks[ordercnt,]$refcount, 
			names.arg= xstacks[ordercnt,]$id,
			main="Stacks by reference count", 
		sub=src)}
		)#switch
	}

	#plot(stacks)
	#invisible(ss)
	}#plot_stacks(rpo)

#plot_terminals<- function(x){
#	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
#}
	

plot_profiles <- function(x, which=c(1L,2L), col,ask = prod(par("mfcol")) < length(which) && dev.interactive(), 				src=NULL,
		 ...){
		if (inherits(x,"sprof")) {
		xprof<- x$profiles
		if (is.null(src)) src<-x$info$id
		}	else if (inherits(x,"sprof_profiles")) {## qualified stacks data
		xprof <- x
		if (is.null(src))  src<-deparse(substitute(x))
		} else {## raw data
		xprof <- x	
		if (is.null(src))  src<-deparse(substitute(x))
	}

	warning("RLE and multiple timings not yet supported")
	 #sp <- summary_profiles(x)
	 nrprof <- length(xprof$data)
	z <- xprof$data; dim(z)=c(nrprof,1)
	
	stackrng <- range(z)
	if (missing(col)) {col <- rainbow(stackrng[2]-stackrng[1]+1)}
	
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }

 	for (ip in which){
 		switch(ip,
	barplot(t(z), main="stack ids by event", ylab="stack id", sub=src),
	image(x=1:nrprof,y=1,z, 
	xlab="event", 
	ylab="", yaxt="n" , ylim=c(1,nrprof), 
	col=col, 
	main="stacks by event", sub=src)
	)#switch
	}
	#image(z, xlab="event", ylab="" , xlim=c(1,length(x$data)), yaxs=NULL)
	invisible(x)
}

plot.sprof <- function(x,...){
	plot_nodes(x)
	plot_stacks(x)
	plot_profiles(x)
	invisible(x)
}