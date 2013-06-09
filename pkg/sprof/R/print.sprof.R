# $Head:$
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/print_prof.R', chdir = TRUE)
#! To Do
#!



print_nodes <- function(x){
	sn <- summary_nodes(x)
	print(sn)
	invisible(sn)
}

print_stacks <- function(x){
	ss <- summary_stacks(x)
	print(ss)
	invisible(ss)
	}

#print_terminals<- function(x){
#	table(x$stacks$stackleafnodes)
#	x$stacks$refcount
#}
	

print_profiles <- function(x){
	sp <- summary_profiles(x)
	print(sp)
	invisible(sp)
}

print.sprof <- function(x,...){
	print_nodes(x)
	print_stacks(x)
	print_profiles(x)
	invisible(x)
}