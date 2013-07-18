#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/shownodes.R', chdir = TRUE)
#! To Do
#!

shownodes <- function(sprof, col) {
#oldpar <- par(mfrow=c(1,3))
oldpar<- par(no.readonly = TRUE)
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

#plot_nodes # 3
xnodes <- sprof$nodes
src <- sprof$info$src
mincount <- 5
nrnodes <- dim(xnodes)[1]
 totaltime <- sum(xnodes$self.time)
if(missing(col)) col <- rainbow(nrnodes)

ordertotal<- order(xnodes$total.time,decreasing=TRUE);
if (is.null(xnodes$icol)) {warning("nodes$icol is undefined. Generated on the fly.")
icol <-ordertotal
xnodes$icol <- icol
sprof$xnodes$icol <- icol
}
xnodes <- xnodes[xnodes$total.time < totaltime,]
#browser()
if (mincount>0) xnodes <- xnodes[xnodes$total.time>=mincount,]
trimmed <- nrnodes-dim(xnodes)[1]

totaltime <- sum(xnodes$self.time)
fulltime <- dim(xnodes)[1]
xnodes <- xnodes[xnodes$total.time < totaltime,]
fulltime <- fulltime - dim(xnodes)[1]
ordertotal<- order(xnodes$total.time,decreasing=TRUE);

# nodes
plot_nodes(sprof, which=3, ask=FALSE, col=col)

stacks_nodes <- list.as.matrix(sprof$stacks$nodes)
sn <- stacks_nodes
sn <- sprof$nodes$icol[sn]
dim(sn)<-dim(stacks_nodes)
image(x=1:ncol(stacks_nodes),y=1:nrow(stacks_nodes), 
t(sn), col=col,
xlab="stack", ylab="depth", main="nodes by stack")

#image(x=1:ncol(stacks_nodes),y=1:nrow(stacks_nodes), 
#t(stacks_nodes), col=col,
#xlab="stack", ylab="depth", main="nodes by stack")

profile_nodes <- profiles_matrix(sprof)
pn <- profile_nodes
pn <- sprof$nodes$icol[pn]
dim(pn)<-dim(profile_nodes)

image(x=1:ncol(profile_nodes),y=1:nrow(profile_nodes), 
t(pn),col=col,
xlab="event", ylab="depth", main="nodes by event")
par(oldpar)
}
