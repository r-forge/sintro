# edit(file="~/Documents/lectures/src/insider/profile/sprof/pkg/man/nodei.Rd")
# source('~/projects/rforge/sintro/pkg/sprof/R/nodei.R', chdir = TRUE)
# file.edit('~/projects/rforge/sintro/pkg/sprof/R/nodei.R', chdir = TRUE)


nodei <- function(sprofx, node, warn = TRUE)
{
	i <- match(node, sprofx$nodes$name, nomatch=0)
	if (i==0){
		sprofx$nodes$name <<- as.character(sprofx$nodes$name)
		sprofx$nodes <<- rbind(sprofx$nodes,NA)
		i <- length(sprofx$nodes$name)
		sprofx$nodes$name[i] <<- node
		if (warn) warning("node added. An updateRprof() may be necessary.")
	}
	return(i)
}
# sprof <- sprof01; nodei(sprof,"kiki"); sprof$nodes