#
# setwd("")
#! To Do
#!

# recursive run length encoding of a matrix by row, top down
rrle <- function(x){
	prevrle <- rle(x[1,])
	collrle <- list(prevrle)
	#browser()
	if (nrow(x)>1) {for  (i in (2: nrow(x))) {
		ni <- length(prevrle$lengths)
		endp <-cumsum(prevrle$lengths)
 		startp <- c(1,endp[-ni]+1)
		thisrle <- rle(x[i, startp[1] : endp[1] ] )
		 if (ni>=2) {for (j in 2:ni) {
		 	newrle <- rle(x[i, startp[j] : endp[j] ] )
		 	thisrle$lengths <- c(thisrle$lengths, newrle$lengths) 
		 	thisrle$values <- c(thisrle$values, newrle$values)
		  }# j
		  }
		collrle <-c(collrle,list(thisrle))
		prevrle <- thisrle
	} # i
	}
	collrle
}# rrle


# recursive run length encoding of a matrix by row, bottom up
# may be removed
rrleb <- function(x){
	prevrle <- rle(x[nrow(x),])
	collrle <- list(prevrle)
	#browser()
	if (nrow(x)>1) {for  (i in (nrow(x)-1): 1) {
		ni <- length(prevrle$lengths)
		endp <-cumsum(prevrle$lengths)
 		startp <- c(1,endp[-ni]+1)
		thisrle <- rle(x[i, startp[1] : endp[1] ] )
		 if (ni>=2) {for (j in 2:ni) {
		 	newrle <- rle(x[i, startp[j] : endp[j] ] )
		 	thisrle$lengths <- c(thisrle$lengths, newrle$lengths) 
		 	thisrle$values <- c(thisrle$values, newrle$values)
		  }# j
		  }
		collrle <-c(list(thisrle),collrle)
		prevrle <- thisrle
	} # i
	}
	collrle
}# rrleb

# x <- matrix(c(
	# 1,1,1,2,2,
	# 3,3,4,4,4,
	# 5,5,6,6,7,
	# 8,9,9,0,0
# ),nrow=4, ncol =5, byrow=TRUE)
# xrrle <- rrle(x)
# xrrle

# t(sapply(xrrle, inverse.rle))


