# $HeadURLs:$
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/plot_prof.R', chdir = TRUE)
#! To Do
#!



plot_nodes <- function(x){
	sn <- summary_nodes(x)
	plot(sn)
	invisible(sn)
}

plot_stacks <- function(x){
	ss <- summary_stacks(x)
	plot(ss)
	invisible(ss)
	}

#plot_terminals<- function(x){
#	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
#}
	

plot_profiles <- function(x){
	 sp <- summary_profiles(x)
	 nrprof <- length(x$data)
	z <- x$data; dim(z)=c(nrprof,1)
	image(x=1:nrprof,y=1,z, 
	xlab="event", 
	ylab="", yaxt="n" , ylim=c(1,nrprof), 
	col=rainbow(length(x$stacks$stacks)), 
	main=sp$id)
	#image(z, xlab="event", ylab="" , xlim=c(1,length(x$data)), yaxs=NULL)
	invisible(x)
}

plot.sprof <- function(x,...){
	plot_nodes(x)
	plot_stacks(x)
	plot_profiles(x)
	invisible(x)
}