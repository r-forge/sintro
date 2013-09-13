# $HeadURL$
# $Id$
# setwd("")
#! To Do
#! ad warning if icol=NA
# source('~/projects/rforge/sintro/pkg/sprof/R/plot.sprof.R', chdir = TRUE)


plot_nodes <- function(x, which=c(1L,2L, 3L, 4L), col=NULL, 
	ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
	src=NULL, mincount=5, horiz=FALSE,
	...){
		## plot_nodes(sprof01)
		## full data
		## xnodes <- sprof01$nodes
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
	
	
	if (horiz) lasb <-2 else lasb <- 0

 	nrnodes <- dim(xnodes)[1]
 	totaltime <- sum(xnodes$self.time)
 	xnodes <- xnodes[xnodes$total.time < totaltime,]
 	xnodes <- xnodes[xnodes$total.time > 0,]
 	nrxnodes <- dim(xnodes)[1]

	if (is.null(col)) {
		if (is.null(xnodes$icol))
			col <- terrain.colors(nrnodes) else
			col <- terrain.colors(max(unclass(xnodes$icol), na.rm=TRUE))
		}
	if (is.null(xnodes$icol))
		colx <- col[1:nrnodes] else
		colx <- col[xnodes$icol]	
		
	
#	oldpar <- par(no.readonly = TRUE)
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }

	#browser()
	if (mincount>0) xnodes <- xnodes[xnodes$total.time>=mincount,]
	trimmed <- nrnodes-dim(xnodes)[1]
	legnd <-function(trimmed=0, fulltime=0){
		ltext <- paste(nrnodes,"nodes\n")
		if (trimmed>0) ltext <- paste(ltext,trimmed," nodes with",mincount,"or less\n total counts omitted\n")
		if (fulltime>0) ltext  <- paste(ltext,fulltime,"permanent nodes  omitted\n")
		legend("topright", legend=ltext, bty="n") 
}
	
	for (ip in which){
		switch(ip,
		
		# 1 scatterplot total ~ self
		#! setting xlim breaks example (hangs) in wordcloud::textplot
				# { if (is.null(xnodes$icol)){
			# plot(xnodes$self.time, xnodes$total.time, xlab="self", 
				# ylab="total", sub=src, main="Nodes by time")
			# if (require(wordcloud)) textplot(xnodes$self.time, xnodes$total.time,xnodes$name, new=FALSE, xlim= par("usr")[1:2]) else
				# text(xnodes$self.time, xnodes$total.time,xnodes$name)
			# } else { # has icol
				# plot(xnodes$self.time, xnodes$total.time, xlab="self", 
					# ylab="total", pch=16, col= colx, sub=src, main="Nodes by time", ...)
				# if (require(wordcloud)) textplot(xnodes$self.time, xnodes$total.time,xnodes$name, new=FALSE, col=col[xnodes$icol], xlim= par("usr")[1:2] ) else
				# text(xnodes$self.time, xnodes$total.time,xnodes$name, col=col[xnodes$icol])
		# }
		
		# }, 
		{ if (is.null(xnodes$icol)){
			plot(xnodes$self.time, xnodes$total.time, xlab="self", 
				ylab="total", sub=src, main="Nodes by time")
			if (require(wordcloud)) textplot(xnodes$self.time, xnodes$total.time,xnodes$name, new=FALSE) else
				text(xnodes$self.time, xnodes$total.time,xnodes$name)
			} else { # has icol
				plot(xnodes$self.time, xnodes$total.time, xlab="self", 
					ylab="total", pch=16, col= colx, sub=src, main="Nodes by time", ...)
				if (require(wordcloud)) textplot(xnodes$self.time, xnodes$total.time,xnodes$name, new=FALSE, col=col[xnodes$icol] ) else
				text(xnodes$self.time, xnodes$total.time,xnodes$name, col=col[xnodes$icol])
		}
		
		}, 
		#2
		{   orderself <- order(xnodes$self.time,decreasing=TRUE);
			xn <- xnodes[orderself,]
			xn <- xn[xn$self.time>0,]
			#barplot(xnodes$self.time,
			#main="Nodes: time as last of stack",
			#names.arg = xnodes$name, sub=src, ylab="count", ...);
			if (is.null(xn$icol)){
				barplot(xn$self.time,
					main="Nodes: time as last of stack",
					names.arg = xn$name, cex.names=0.5,
					sub=src, ylab=NULL, xpd=FALSE, horiz=horiz, las=lasb, ...);
			} else
			{ 	barplot(xn$self.time,
					main="Nodes: time as last of stack",
					names.arg = xn$name, cex.names=0.5,
					sub=src, ylab=NULL,col=col[xn$icol], xpd=FALSE, horiz=horiz,  las=lasb,...)
			}

			legnd(trimmed=trimmed, fulltime=0)
			}, 
			
		#3
		{   totaltime <- sum(xnodes$self.time)
			fulltime <- dim(xnodes)[1]
 			xn <- xnodes[xnodes$total.time < totaltime,]
 			fulltime <- fulltime - dim(xn)[1]
			ordertotal<- order(xn$total.time,decreasing=TRUE);
			if (is.null(xn$icol)){
				barplot(xn[ordertotal,]$total.time, 
					main="Nodes: total time in stack",
					names.arg = xn[ordertotal,]$name,cex.names=0.5,
					 sub=src, ylab=NULL, xpd=FALSE,
					 horiz=horiz, las=lasb,...)
				} else {
				barplot(xn[ordertotal,]$total.time, 
					main="Nodes: total time in stack",
					names.arg = xn[ordertotal,]$name, cex.names=0.5,
					sub=src, ylab=NULL, col=col[xn[ordertotal,]$icol], xpd=FALSE,
					horiz=horiz, las=lasb,...)
				}
			
			legnd(trimmed=trimmed, fulltime=fulltime)},
			
		#4 scatterplot log total ~ log (self+1)
		{ if (is.null(xnodes$icol)){
			plot(xnodes$self.time+1, xnodes$total.time, 
				xlab="log(self+1)", ylab="log(total)", log="xy",
		 		sub=src, main="Nodes by time")
			if (require(wordcloud)) textplot(xnodes$self.time, 
				xnodes$total.time,xnodes$name, new=FALSE) else
				text(xnodes$self.time+1, xnodes$total.time,xnodes$name)
		} else { # has icol
			plot(xnodes$self.time+1, xnodes$total.time, 
				xlab="log(self+1)", ylab="log(total)", log="xy",
				pch=16, col= col[xnodes$icol], sub=src, main="Nodes by time", ...)
			if (require(wordcloud)) textplot(xnodes$self.time+1, 
				xnodes$total.time,xnodes$name, new=FALSE, col=col[xnodes$icol]) else
				text(xnodes$self.time+1, xnodes$total.time,xnodes$name, col=col[xnodes$icol])
		}},
		
		#5
		{ if (is.null(xnodes$icol)){
			frame()
			legend("center","icol \n is required for this plot")} else
			plot(table(xnodes$icol), type="h", lwd=20, col=col, lend="square", ylab="count", las=2,...)
		},
		#6
		{nodescloud(xnodes, min.freq=mincount, col=col)
			if (nrnodes!=nrxnodes){
				title(main=paste0("At most ",nrxnodes, " of ", nrnodes, " nodes shown"),col.sub=grey(0.5),font=3 )
			}
		}
		
	
)#switch
}#for
#	par(oldpar)
	invisible(xnodes)
}# plot_nodes

plot_stacks <- function(x,which=c(1L, 2L),ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
		src=NULL, mincount=5, horiz=FALSE,
		...){
		#$Id$
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

	
	
	if (horiz) lasb <-2 else lasb <- 0


	#ss <- summary_stacks(xstacks)
	
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
	}
	#browser()
 	nrstacks <- dim(xstacks)[1]
	#browser()
	if (mincount>0) xstacks <- xstacks[xstacks$refcount>=mincount,]
	trimmed <- nrstacks-dim(xstacks)[1]
	legnd <-function(){
		if (trimmed>0) 
		legend("topright", legend=paste(nrstacks,"stacks\n", 
		trimmed," stacks with",mincount,"or less\n total references omitted\n"),bty="n") else 
		legend("topright", legend=paste(nrstacks,"stacks"),bty="n") 
	}

 	for (ip in which){
		switch(ip,
		#1
		plot(xstacks$refcount, xstacks$stacklength, xlab="refcount", ylab="stack length", sub=src), 
		{ ordercnt  <- order(xstacks$refcount, 
			decreasing=TRUE); id <-seq(along.with=xstacks$refcount)
			xstacks$id <-  id
			barplot(xstacks[ordercnt,]$refcount, 
			names.arg= xstacks[ordercnt,]$id,
			main="Stacks by reference count", 
			ylab="count",
			xlab="stack",
			horiz=horiz,
			las=lasb,
		    sub=src,...);
		legnd()}
		)#switch
	} #for

	#plot(stacks)
	#invisible(ss)
	}#plot_stacks(rpo)

#plot_terminals<- function(x){
#	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
#}
	

plot_profiles <- function(x, which=c(1L,2L,3L, 4L), col,ask = prod(par("mfcol")) < length(which) && dev.interactive(), 				src=NULL,
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

	#warning("RLE and multiple timings not yet supported")
	 #sp <- summary_profiles(x)
	 nrprof <- length(xprof$data)
	z <- xprof$data; dim(z)=c(length(z),1)
	
	stackrng <- range(z,na.rm = TRUE)
	if (missing(col)) {col <- rainbow(stackrng[2]-stackrng[1]+1)}
	
	    if (ask) {
	oask <- devAskNewPage(TRUE)
	on.exit(devAskNewPage(oask))
    }

 	for (ip in which){
 		switch(ip,
 	#1
	plot(z[,1], main="stack ids by event", ylab="stack id", sub=src),
	#2
	{ zref <- sapply(z, function(xx) {x$stacks$refcount[xx] })
	plot(zref, main="stack reference count by event", ylab="count", xlab="event",sub=src)
	},
	#3
	{ zref <- sapply(z, function(xx) {x$stacks$stacklength[xx] })
	plot(zref, main="stack length by event", ylab="count",  xlab="event", sub=src)
	},

	#4
	image(x=1:nrprof,y=1,z, 
	xlab="event", 
	ylab="", yaxt="n" , ylim=c(1,nrprof), 
	col=col, 
	main="stacks by event", sub=src),
	#5
	{
		if(!is.null(xprof$mem)) {vsize.small <- xprof$mem[,"vsize.small.8by"]; plot(diff(vsize.small))}
		
	},
	#6
	{
		if(!is.null(xprof$mem)) {vsize.large <- xprof$mem[,"vsize.large.8by"]; plot(diff(vsize.large))}
		
	},
	#7
	{
		if(!is.null(xprof$mem)) {nodes <- xprof$mem[,"nodes"]; plot(diff(nodes))}
		
	},
	#8
	{
		if(!is.null(xprof$mem)) {duplications <- xprof$mem[,"duplications"]; plot(diff(duplications))}
		
	}	
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