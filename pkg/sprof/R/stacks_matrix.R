stacks_matrix <- function(x){
if (length(x$stacksrenc)>2)
maxlen <- max(sapply(x$stacksrenc,length))
filledstacksrenc  <- sapply(x$stacksrenc, 
	function(xs){fillen <- maxlen-length(xs)
	if (fillen>0) {c(xs,rep(NA,fillen))} else xs
	}
	)
	filledstacksrenc
}