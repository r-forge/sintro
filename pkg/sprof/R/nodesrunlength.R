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
nodesrunlength <- function(sprof){
profile_nodes_rlearray <- nodesprofile(sprof)
mt <- margin.table(profile_nodes_rlearray, margin = c(1,3))
#amt <- addmargins(mt)
mts <- apply(mt,1,sum)
mtt <- apply(mt,1,function (x) {sum(x*seq(along.with=x))})
mtav <- mtt/mts
amt=cbind(mt,count=mts, total_time=mtt, avg=mtav)
amt <- amt[amt[,"count"]>0,]
amt <- amt[order(amt[,"total_time"], decreasing=TRUE),]
return(amt)
}#nodesrunlength
