updateRprof <- function(sprof){
	sprof$info$date_updated <- Sys.time()
	
	nrstacks <- length(sprof$stacks$nodes)
	nrnodes <- length(sprof$nodes$name)
	collstacksdict <- sprof$stacks$stackssrc
	# stacks must be coded as node references


	# bubble down statistics -- profiles -> stacks
	profile_lines <- sprof$profiles$data
	collinterval <- inverse.rle(sprof$profiles$timesRLE)
	stackrefcount <- double(nrstacks)
	for (i in seq_along(profile_lines))  {
		j<-profile_lines[i] ;
		stackrefcount[j]<- stackrefcount[j]+collinterval[i]}



	stacklength <- sapply(sprof$stacks$nodes,length)
	# is in reverse order root=[1]
	# recode: root = first node
	stacks_nodesL <- sapply(sprof$stacks$nodes,rev)

	stackleafnodes <- sapply(stacks_nodesL,function(x){if (is.null(x[[1]])) NA else x[[1]]})
	stackleafnodes <- unlist(stackleafnodes)

	
	
	# bubble down statistics -- stacks -> nodes
	# leaf records "by.self"
	leafcount <- double(nrnodes)
	for (i in seq_len(nrstacks)) {		istack <- rev(sprof$stacks$nodes[[i]])
		leafcount[stacks_nodesL[[i]][1]] <- 	
			leafcount[stacks_nodesL[[i]][1]] + stackrefcount[i]}
	
	leafnodes <- unique(sapply(stacks_nodesL,function(x){x[[1]]}))
	
	totalcount <- double(nrnodes)
	for (i in seq_len(nrstacks)) {
		sunodes <- unique(sprof$stacks$nodes[[i]])
		totalcount[sunodes] <- totalcount[sunodes] + stackrefcount[i]
		}
		
		
	# recode not necessary: root = first node for sprof$stacks$nodes
	
	rootnodes <- unique(sapply(sprof$stacks$nodes,function(x){x[[1]]}))
	stackheadnodes <- sapply(sprof$stacks$nodes,function(x){if (is.null(x[[1]])) NA else x[[1]]})
	stackheadnodes <- unlist(stackheadnodes)

 	nodenames <- sprof$nodes$name
 	sprof$stacks$collstacksdictrev <- sapply(stacks_nodesL, function(x){paste(nodenames[x], collapse=" ")})

	sprof$stacks$refcount <- stackrefcount					
	sprof$stacks$stacklength <- stacklength   # length(stacksrenc)
	sprof$stacks$stackheadnodes <- stackheadnodes # stacksrenc[first]
	sprof$stacks$stackleafnodes <- stackleafnodes # stacksrenc[last]
		# a convenience to allow textual matching -- may be removed
	sprof$stacks$stackssrc<-  collstacksdict  # headers and control lines removed
	row.names(sprof$stacks) <- seq_along(sprof$stacks$nodes)

	sprof$nodes$self.time <- leafcount
	sprof$nodes$self.pct <-  round(leafcount/sum(leafcount)*100,2)
	sprof$nodes$total.time <- totalcount
	sprof$nodes$total.pct  <-  round(totalcount/sum(totalcount)*100,2)
	
	sprof$info$nrnodes <-nrnodes
	sprof$info$nrstacks <-nrstacks
	sprof$info$nrrecords <-length(sprof$profiles$data)
	
	return(sprof)
}

# updateRprof(sprof02)
