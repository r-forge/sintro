# $HeadURL$
# convert list to matrix
# list entries go to matrix columns, filled for equal length 

list.as.matrix <- function(x, filler=NA){
	maxlen <- max(sapply(x,length))
    sapply(x, 
	function(xs){fillen <- maxlen-length(xs)
		if (fillen>0) {c(xs,rep(filler,fillen))} else xs
	})
}


# x <- list(x1=c(1,2,3),x2=3, x3=4:8)
# list.as.matrix(x)
# list.as.matrix(x,filler=0)
