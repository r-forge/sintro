# filename = "Rprofsr01.out"; chunksize = 100, interval = 0.02
# edit(file="~/Documents/lectures/src/insider/profile/sprof/pkg/man/readProf.Rd")
# source('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)
# file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)


roots_sprof <- function(sprof, stacks){
	if (!missing(stacks)){
	} else {stacks <- sprof$stacks$nodes}
	rts <- sapply(stacks, function(x) {unlist(x)[1]})
	rts <- unlist(unique(rts))
	if (!missing(sprof)) names(rts) <- sprof$nodes$name[unclass(rts)]
	rts
}
