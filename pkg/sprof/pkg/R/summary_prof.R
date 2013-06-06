# $Head:$
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

	cat("First line:", dQuote(x$firstline),"\n")
	cat(length(x$data),"Sampling intervals ")
	if (length(x$timesRLE[1])==1) cat(" at",x$timesRLE[[2]],"micros\n") else {cat("in micros: ");print(x$timesRLE)}
	cat(length(x$nodes),"nodes in",length(x$stacks$stacks), "stacks\n")	
	cat(length(unique(x$stacks$stackleafnodes)),"Terminals ", "\n")

	roots <- unique(x$stacks$stackheadnodes)
	cat(length(roots), "Roots: ")
	print(table(x$nodes[x$stacks$stackheadnodes]))
	cat("Structure: ");str(x, max.level=1)
	cat("stacks Structure: ");str(x$stacks, max.level=1, vec.len=2)		
}# str_prof 

summary_terminals<- function(x){
	
	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
}

summary_prof <- function(x){
	str_prof(x)
}