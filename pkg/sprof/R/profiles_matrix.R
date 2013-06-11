profiles_matrix <- function(x){
	sm <- stacks_matrix(x)

filledprofile <- matrix(NA, nrow=nrow(sm), ncol=length(x$data))

for (iprof in (1:ncol(filledprofile))) filledprofile[,iprof] <- sm[,x$data[iprof]]
	filledprofile
}# profiles_matrix