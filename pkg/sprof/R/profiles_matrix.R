profiles_matrix <- function(x){
	sm <-  list.as.matrix(x$stacks$nodes)


	filledprofile <- matrix(NA, nrow=nrow(sm), ncol=length(x$profiles$data))

	for (iprof in (1:ncol(filledprofile))) 
		filledprofile[,iprof] <- sm[,x$profiles$data[iprof]]
	filledprofile
}# profiles_matrix