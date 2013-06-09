# filename = "Rprofsr01.out"; chunksize = 100, interval = 0.02
# edit(file="~/Documents/lectures/src/insider/profile/sprof/pkg/man/readProf.Rd")
# source('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)
# file.edit('~/projects/rforge/sintro/pkg/sprof/R/readProf.R', chdir = TRUE)


readProf <- function(filename = "Rprof.out", 
	chunksize = 5000, 
	interval = 0.02, 
	head=c("auto", "none", "Rprofmem"),
	id=NULL){
	con <- file(filename, "rt")
    if (is.null(id)) id <- paste(deparse(substitute(filename)),  date())
	firstline <- readLines(con, n = 1L)
	
	head <- match.arg(head)
	
   # avoid to copy an initial chunk in case firstline is not special
   close(con)
   con <- file(filename, "rt")
   #on.exit(if i(sOpen(con,"")  close(con))
   
	if(!length(firstline))
		stop(gettextf("no lines found in %s", sQuote(filename)), domain = NA)
	sampleinterval <- as.numeric(strsplit(firstline, "sample.interval=")[[1L]][2L]) # /1e6
	# in micros. summaryRprof keeps intervals in s.
	if (is.na(sampleinterval)) sampleinterval <- {interval*1e6}
	
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
	collenclines <- NULL
	
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
			newinterval <- as.numeric(strsplit(chunk[i], "sample.interval=")[[1L]][2L])
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
	
		# these must be aligned. collenclines are references to 	collstacksdict. 
		collinterval<-c(collinterval, chunkinterval)  
		collenclines <- unlist(c(collenclines, (match(chunk, collstacksdict))))

		linesread <- linesread+ linesinchunk;
	})
	
	close(con)


	if (!is.null(collcontrols)) dim(collcontrols) <-NULL
	if (length(collstacksdict)>0) {
	stackssplit <- strsplit(collstacksdict, " ")
	nodenames <- sort(unique(unlist(strsplit(collstacksdict, " "))))
	#browser()
	stacksnode <- sapply(stackssplit,match,nodenames)
	stacklength <- sapply(stacksnode,length)
	stackleafnodes <- sapply(stacksnode,function(x){x[[1]]})
	leafnodes <- unique(sapply(stacksnode,function(x){x[[1]]}))
	stacksnode <- sapply(stacksnode,rev)
	rootnodes <- unique(sapply(stacksnode,function(x){x[[1]]}))
	stackheadnodes <- sapply(stacksnode,function(x){x[[1]]})
	
	stackrefcount <- as.integer(table(factor(collenclines, levels=1:length(stacksnode),ordered=FALSE)))
	#stackrefcount collenclines
	# browser()

	# nodes <- data.frame(name=nodenames, row.names=1)
	# attr(nodes,"roots") <- rootnodes
	# attr(nodes,"terminals") <- leafnodes
	
	# #stacks <- data.frame(sourcestr= collstacksdict,stacksrenc =  stacksnode)
	# #attr(stacks, "freq") <- table(collenclines)

	# data <- data.frame(stack=collenclines, mem = collmemcounts,malloc = collmalloccounts)
	# attr(data,"times") <-  rle(collinterval) # expand and add when reading
	
	#renc -> reversed  source
	#browser()
 	collstacksdictrev <- sapply(stacksnode, function(x){paste(nodenames[x], collapse=" ")})
   #browser()
	stacks <- data.frame(
		shortname = abbreviate(collstacksdictrev), # headers and control lines removed
		# a convenience for accounting#nr of lines using stack --! should be adjusted for interval
		refcount = stackrefcount, 		
		stacklength =stacklength,    # length(stacksrenc)
		stackheadnodes =stackheadnodes, # stacksrenc[first]
		stackleafnodes =stackleafnodes, # stacksrenc[last]
		# a convenience to allow textual matching
		stacks= collstacksdict  # headers and control lines removed
		)	
	Rprofdata <- list(
	   # diagnostics support
		firstline=firstline, 
		ctllines=collcontrols,
		ctllinenr=collcontrollinenr,
		
		
		# basic data tables
		nodes=nodenames, 
		
		# these are conceptually a data frame and must be item aligned
		stacksrenc =  stacksnode, # list of arrays of references to nodes
		# a convenience to allow textual matching
		
		stacks= stacks,
		# these are conceptually a data frame and must be line aligned
		data= collenclines,	# references to stacksrenc
		mem = collmemcounts, # additional, line-synced  --- merge to data
		malloc = collmalloccounts, # additional, line-synced  --- merge to data
		timesRLE = rle(collinterval)  # --- merge to data
		
		)
		} else
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
		class(Rprofdata) <- c("sprof","list")
	return(Rprofdata)

}# readProf
