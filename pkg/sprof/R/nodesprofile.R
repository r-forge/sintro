#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/nodesprofile.R', chdir = TRUE)
#! To Do
#!
nodesprofile <- function(sprof){
profile_nodes_rle<- rrle(profiles_matrix(sprof), collapseNA=FALSE) 

maxnode <-0
maxlen <-0
maxlevel <-length(profile_nodes_rle)
for (lev in (1:maxlevel) ) {
	prlv <- profile_nodes_rle[[lev]]
	if (!is.null(prlv)) {
		maxn <- max(prlv$values, na.rm=TRUE)
		if (maxn>maxnode) maxnode <- maxn
		maxl <- max(prlv$lengths, na.rm=TRUE)
		if (maxl>maxlen) maxlen <- maxl
		# cat("Level ",lev,maxn," Length:",maxl,"\n")
	}
}
## collapse profile_nodes_rle to 3d array. Allocate memory first.
profile_nodes_rlearray <- array(0, 
	dim=c(maxnode,length(profile_nodes_rle), maxlen), 
	dimnames= list("node"=sprof$nodes$name[1:maxnode], 
		"level"=1:length(profile_nodes_rle), 
		"run_length"=1:maxlen))
		
		for (lev in (1:maxlevel) ) {
	prlv <- profile_nodes_rle[[lev]]
	if (!is.null(prlv)) {
	  for (j in (1: length(prlv$lengths))){
		 if (!is.na(prlv$values[j])){
			profile_nodes_rlearray[prlv$values[j],lev,prlv$lengths[j]] <-
			profile_nodes_rlearray[prlv$values[j],lev,prlv$lengths[j]] +1
			#cat(lev,j,":",prlv$values[j],lev,prlv$lengths[j],"\n")
		 }#if (!is.na
	   }#for j
	}
}

return(profile_nodes_rlearray)
} # nodesprofile