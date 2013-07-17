# filename = "Rprofsr01.out"; chunksize = 100, interval = 0.02
# edit(file="~/Documents/lectures/src/insider/profile/sprof/pkg/man/readProf.Rd")
# source('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)
# file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)


readRprof <- function(filename = "Rprof.out", 
	chunksize = 5000, 
	interval = 0.02, 
	head=c("auto", "none", "Rprofmem"),
	id=NULL){
	con <- file(filename, "rt")
    if (is.null(id)) id <- paste(deparse(substitute(filename)), file.info(filename)$mtime)
	firstline <- readLines(con, n = 1L)
	
	head <- match.arg(head)
	
   # avoid to copy an initial chunk in case firstline is not special
   pushBack(firstline, con, newLine = TRUE)

   #close(con)
   #con <- file(filename, "rt")
   #on.exit(if i(sOpen(con,"")  close(con))
   
	if(!length(firstline))
		stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA)
	sampleinterval <- as.numeric(strsplit(firstline, "sample.interval=")[[1L]][2L])  /1e3
	# in ms. (summaryRprof keeps intervals in micros.)
	if (is.na(sampleinterval)) sampleinterval <- {interval* 1e3}
	
	memory.profiling <- substr(firstline, 1L, 6L) == "memory"
    line.profiling <- grepl("line profiling", firstline)
    if (line.profiling)
    	filenames <- character(0)


	linesread <-0

	# collected stack line directory. Preferably unique, but this is not a hard requirement.
	# special lines are included.
	collstacksdict <- NULL
	
	# collected input lines, only stack part. Head is removed.
	# These are references to the collstacksdict dictionary.
	profile_lines <- NULL
	
	collinterval <- NULL
	collmemcounts <- NULL
	collmalloccounts <- NULL
	collcontrols <- NULL
	collcontrollinenr <- NULL
	
	prevline <- ""
	prevlineindex <- 0 #invalid
	
	repeat({
		chunk <- readLines(con, n = chunksize)
		if (length(chunk)==0L) break; linesinchunk <- length(chunk)

		chunk <- sapply(chunk, function(x){gsub("\"","",x)}, USE.NAMES=FALSE)

		chunkinterval = rep(sampleinterval, linesinchunk)
	
		if (head=="auto")	{
		silines <- grep("sample\\.interval=", chunk)
		if (length(silines)>0){
		for (i in silines) { #? should this give a msg?
			#browser()
			newinterval <- as.numeric(strsplit(chunk[i], "sample.interval=")[[1L]][2L])/ 1e3 # ms
			if (!is.na(newinterval)) {
				if (newinterval != sampleinterval){
					message(paste("Line ",i, dQuote(chunk[i])),"\n")
					chunkinterval[i:linesinchunk] <- newinterval; sampleinterval<-newinterval}
				}
		}
		collcontrols <- unlist(rbind(collcontrols,chunk[silines]))
		collcontrollinenr <- c(collcontrollinenr, silines+ linesread)
		chunk <- chunk[-silines]
		chunkinterval <- chunkinterval[-silines]
		}
		}
		#if (head != "auto")	{warning("Input may contain control lines ")}
		
				
		if (head=="Rprofmem")
	#handle Rprofmem output
	{
			#browser()
			cx <- sapply(chunk, function(x){sub("new page:", "0 :",x)}, USE.NAMES = FALSE)
			cx <- strsplit(cx," :")
			chunkmalloccounts <- as.numeric(sapply(cx, function(x){x[1]}, USE.NAMES = FALSE))
			chunk <- sapply(cx, function(x){x[2]}, USE.NAMES = FALSE)          
           # chunk <- substr(chunk, mallocprefix+1L, nchar(chunk,  "c"))
           # if(any((nc <- nchar(chunk, "c")) == 0L)) {
                # chunk <- chunk[nc > 0L]
                # chunkmalloccounts <- chunkmalloccounts[nc > 0L]
           # }
           collmalloccounts <- c(collmalloccounts, chunkmalloccounts)
       }
       
	
	       if (memory.profiling) {
           memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", chunk), "match.length")
           
               memstuff <- substr(chunk, 2L, memprefix-1L)
               chunkmemcounts <- pmax(apply(sapply(strsplit(memstuff, ":"), as.numeric), 1, diff), 0)
               ##  chunkmemcounts <- c(0, rowSums(chunkmemcounts[, 1L:3L]))
               ## convert to bytes.
               chunkmemcounts <- c(0, rowSums(cbind(chunkmemcounts[, 1L:2L] * 8, chunkmemcounts[, 3L])))
               rm(memstuff)
          
           chunk <- substr(chunk, memprefix+1L, nchar(chunk,  "c"))
           if(any((nc <- nchar(chunk, "c")) == 0L)) {
                chunk <- chunk[nc > 0L]
                chunkmemcounts <- chunkmemcounts[nc > 0L]
           }
           collmemcounts <- c(collmemcounts, chunkmemcounts)
       }
			
		chunku <- unique(chunk)
		matchcollu <- match(chunku, collstacksdict, nomatch=0)
		newuniques <- chunku[matchcollu==0]
		collstacksdict <- c(collstacksdict,newuniques)  # growing only.
	
		# these must be aligned. 
		# profile_lines are references to collstacksdict. 
		collinterval<-c(collinterval, chunkinterval)  
		profile_lines <- unlist(c(profile_lines, (match(chunk, collstacksdict))))

		linesread <- linesread+ linesinchunk;
	})
	
	close(con)

	if (!is.null(collcontrols)) dim(collcontrols) <-NULL
# end read data	 

# the following contains an inlined version of updateRprof
# NUll structure -- fallback result. Keep this aligned with final struct.
{Rprofdata <- list(
		# diagnostics support
		firstline=firstline, 
		ctllines=collcontrols,
		ctllinenr=collcontrollinenr,
		nodes=NULL,
		stacksrenc=NULL,
		data=NULL,
		mem=NULL,
		malloc=NULL,
		timesRLE=NULL
			)}
			
	if (length(collstacksdict)>0) {

#browser()	
	nrstacks <- length(collstacksdict)	

    # split stacks to node level		
	stackssplit <- strsplit(collstacksdict, " ")
	nodenames <- sort(unique(unlist(strsplit(collstacksdict, " "))))
	nrnodes <- length(nodenames)

	# recode stack to node references
	stacks_nodes <- sapply(stackssplit,match,nodenames)
	
	# bubble down statistics -- profiles -> stacks
	stackrefcount <- double(nrstacks)
	for (i in seq_along(profile_lines))  {
		j<-profile_lines[i] ;
		stackrefcount[j]<- stackrefcount[j]+collinterval[i]}
	#stackrefcount <- as.integer(table(factor(profile_lines, levels=1:length(stacks_nodes),ordered=FALSE)))
	
	
	
	stacklength <- sapply(stacks_nodes,length)
	stackleafnodes <- sapply(stacks_nodes,function(x){x[[1]]})
	
	
	# bubble down statistics -- stacks -> nodes
	# leaf records "by.self"
	leafcount <- double(nrnodes)
	for (i in seq_len(nrstacks)) {
		leafcount[stacks_nodes[[i]][1]] <- 
			leafcount[stacks_nodes[[i]][1]] + stackrefcount[i]}
	
	leafnodes <- unique(sapply(stacks_nodes,function(x){x[[1]]}))
	
	#stacks_nodes <- rpo$stacks$node
	totalcount <- double(nrnodes)
	for (i in seq_len(nrstacks)) {
		sunodes <- unique(stacks_nodes[[i]])
		totalcount[sunodes] <- totalcount[sunodes] + stackrefcount[i]
		}
	
	
	# recode: root = first node
	stacks_nodes <- sapply(stacks_nodes,rev)
	
	rootnodes <- unique(sapply(stacks_nodes,function(x){x[[1]]}))
	stackheadnodes <- sapply(stacks_nodes,function(x){x[[1]]})

	# bubble down statistics -- stacks -> nodes
	
	
	#stackrefcount profile_lines
	# browser()

	# nodes <- data.frame(name=nodenames, row.names=1)
	# attr(nodes,"roots") <- rootnodes
	# attr(nodes,"terminals") <- leafnodes
	
	# #stacks <- data.frame(sourcestr= collstacksdict,stacksrenc =  stacks_nodes)
	# #attr(stacks, "freq") <- table(profile_lines)

	# data <- data.frame(stack=profile_lines, mem = collmemcounts,malloc = collmalloccounts)
	# attr(data,"times") <-  rle(collinterval) # expand and add when reading
	
	#renc -> reversed  source
	#browser()
 	collstacksdictrev <- sapply(stacks_nodes, function(x){paste(nodenames[x], collapse=" ")})
   #browser()
   
   # stacks
	stacks <- data.frame(
		nodes = as.matrix(stacks_nodes),
		shortname = abbreviate(collstacksdictrev), 
		# headers and control lines removed
		
		# a convenience for accounting
		#nr of lines using stack --! should be adjusted for interval
		refcount = stackrefcount, 		
		stacklength =stacklength,    # length(stacksrenc)
		stackheadnodes =stackheadnodes, # stacksrenc[first]
		stackleafnodes =stackleafnodes, # stacksrenc[last]
		# a convenience to allow textual matching -- may be removed
		stackssrc= collstacksdict  # headers and control lines removed
		)	
		row.names(stacks) <- seq_along(stacks$nodes)
#		row.names(stacks) <- seq_along(stacks)  -- no. gives name by column
#		rownames(stacks) <- rownames(stacks,do.NULL=FALSE, prefix="s")
		#browser()
		
	Rprofdata <- list(
		info= data.frame(
		id = id,
		date= Sys.time(),
		nrnodes =nrnodes,
		nrstacks = nrstacks,
		nrrecords = length(profile_lines),
	   # diagnostics support
		firstline=firstline, 
		ctllines=collcontrols,
		ctllinenr=collcontrollinenr
		),
		
		# basic data tables
		nodes=data.frame(name=nodenames, 
			self.time=leafcount, self.pct = round(leafcount/sum(leafcount)*100,2),
			total.time=totalcount, total.pct = round(totalcount/sum(totalcount)*100,2)
		),		
		
		stacks= stacks,
		# profiles
		# these are conceptually a data frame and must be line aligned
        # shoule be improved to allow multiple profile collections 	
        profiles =list(
		data= profile_lines,	# references to stacksrenc
		mem = collmemcounts, # additional, line-synced  --- merge to data
		malloc = collmalloccounts, # additional, line-synced  --- merge to data
		timesRLE = rle(collinterval)  # --- merge to data
		)
		)
		}
		
		class(Rprofdata) <- c("sprof","list")
	return(Rprofdata)

}# readRprof
# Examples
# rpo <- readRprof("Rprof.out")
# str(rpo, max.level=1)


# rpo <- readProf("Rprofsr01.out")
# recover stack entries (in reverse order) from sprof  object

	# string representation for stack istack
#	stackstr <- function(sprof, istack, topdown=TRUE){
	# nodes table is in topdown order
# 	if (rev) st <- sprof$stacks$nodes[[istack]] else
# 		st <- rev(sprof$stacks$nodes[[istack]])
# 	paste(sprof$nodes$name[st[!is.na(st)] ], collapse = ' ')
# 	}

# re_stackssource <- function(sprof) {
#    sapply(sprof$stacks$nodes, function(xl) {sprof$nodesme[xl]})
# }# re_stackssource

writeRprof <- function(sprof, filename="Rprof.Out") {
	stackst <- sapply(sprof$stacks$nodes, 
		function(st){paste(sprof$nodes$name[st[!is.na(st)] ], collapse = ' ')	
	})
	
	proft <- sapply(sprof$profiles$data, function(x){stackst[x]})
	write(proft, file=filename)
	invisible(proft)
}
# writeRprof(rpo, filename="rpo.out")

#
#recover profile source (in reverse order ?) from sprof  object
# re_profilessource <- function(sprof) {
# 	pm <- profiles_matrix(x)
# 	pp <- function(x) {xr <- rev(sprof$nodes$name[x]); 
# 		paste(xr[!is.na(xr)], collapse = ' ')}
# 	return( apply(pm, 2, pp)) #by columns
# }# re_profilessource

updateRprof <- function(sprof){
}
