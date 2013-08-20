# filename = "Rprofsr01.out"; chunksize = 100, interval = 0.02
# edit(file="~/Documents/lectures/src/insider/profile/sprof/pkg/man/readProf.Rd")
# source('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)
# file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)


trimstacks <- function(sprof,  level, trimnode ){
	ts <- sprof$stacks$nodes
	if (!missing(level)){
		ts <- lapply(sprof$stacks$nodes, function(x) {unlist(x)[-(1:level)]})
	}
	if (!missing(trimnode)){
		if (is.character(trimnode)) 
		{ trimnode <- match(trimnode,sprof$nodes$name)
		  if (is.na(trimnode)) return(ts)
		}
	ts <- lapply(ts, 
		function(x) {wh <- match(trimnode,unlist(x))
			if (is.na(wh)) NULL else unlist(x)[-(1:wh)]})
	}
	return(ts)
}
