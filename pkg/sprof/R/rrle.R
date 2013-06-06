#
# setwd("")
#! To Do
#!
# recursive run length encoding of a matrix by row, bottom up
rrle <- function(x){
	prevrle <- rle(x[nrow(x),])
	collrle <- list(prevrle)
	browser()
	if (nrow(x)>1) {for  (i in (nrow(x)-1): 1) {
		ni <- length(prevrle$lengths)
		endp <-cumsum(prevrle$lengths)
 		startp <- c(1,endp[-ni]+1)
		thisrle <- rle(x[i, startp[1] : endp[1] ] )
		 if (ni>2) {for (j in 2:ni) {
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
}# rrle

nrr<- 4
nrc<- 5
n <- nrr*nrc
x <- matrix(sample(1:4, n, replace=TRUE),nrow=nrr, ncol =nrc)
rrle(x)

prevrle <- rle(x[nrow(x),])
collrle <- list(prevrle)

i <-  nrow(x)-1
ni <- length(prevrle$lengths)
 endp <-cumsum(prevrle$lengths)
 startp <- c(1,endp[-ni]+1)
 
 thisrle <- rle(x[i, startp[1] : endp[1] ] )
  for (j in 2:ni) {
  	newrle <- rle(x[i, startp[j] : endp[j] ] )
 thisrle$lengths <- c(thisrle$lengths, newrle$lengths)
 thisrle$values <- c(thisrle$values, newrle$values)
  	}
collrle <-c(list(thisrle),collrle)
 

