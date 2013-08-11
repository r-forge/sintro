#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/nodesrunlength.R', chdir = TRUE)
#! To Do
#!
nodesrunlength <- function(sprof, clean=TRUE){
	profile_nodes_rlearray <- nodesprofile(sprof)
	mt <- margin.table(profile_nodes_rlearray, margin = c(1,3))

	mts <- apply(mt,1,sum)
	mtt <- apply(mt,1,function (x) {sum(x*seq(along.with=x))})
	mtav <- mtt/mts
	mtav[mtt==0] <- 0
	amt=cbind(mt,nr_runs=mts, total_time=mtt, avg_time=mtav)
	if (clean) {
		amt <- amt[amt[,"nr_runs"]>0,]
		amt <- amt[order(amt[,"total_time"], decreasing=TRUE),]
	} else
	{
		while (dim(amt)[1]< sprof$info$nrnodes ) amt <- rbind(amt,0)
	}
	return(amt)
}#nodesrunlength
