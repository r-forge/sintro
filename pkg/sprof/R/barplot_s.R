# %$HeadURL$
# %$Id$
# %$Revision$
# %$Date$
# %$Author$
# %$Name:$
#
# source('~/projects/rforge/sintro/pkg/sprof/R/barplot_s.R', chdir = TRUE)
#
# setwd("")
#! To Do
#! add trimming
#! add auto-colour

# #  sorted barplot
# \todo{move to plot\_stacks}
# <<fig=TRUE, label= barplotStacks4>>=
# stacksperm <- order(sprof01$stacks$refcount,decreasing=TRUE)
# stacksnrobsok <- sprof01$stacks$refcount > 4
# sp4 <- sprof01$stacks$refcount[stacksperm][stacksnrobsok[stacksperm]]
# names(sp4) <- sprof01$stacks$shortname[stacksperm][stacksnrobsok[stacksperm]]
# barplot(sp4,
# main="Stacks, by reference count (4 obs. minimum)", ylab="count > 4")

barplot_s <-function(height,
	sort_by, 
	decreasing=TRUE,
	lowtrim, hightrim, 
	trimlegend=TRUE,
	col,
	coli,
	colfun,
	main, ...)
{
	if (!is.numeric(height)) stop("only numeric data supported so far.")
		lenx <- length(height)

		
		trimmedlow=0; trimmedhigh=0
		
	legnd <-function(trimmedlow=0, trimmedhigh=0){
		ltext <- paste(lenx,"entries\n")
		if (trimmedlow>0) ltext <- paste(ltext, trimmedlow," with",lowtrim,"or less\n omitted\n")
		if (trimmedhigh>0) ltext  <- paste(ltext, trimmedhigh," with",hightrim,"or more\n omitted\n")
		if (decreasing)
		legend("topright", legend=ltext, bty="n", cex=0.8)  else
		legend("topleft", legend=ltext, bty="n", cex=0.8)
}


	if (missing(main)) {main <- deparse(substitute(x))}

	if (missing(sort_by)) {
		sort_by <- height
		main <- paste(main,", by height", sep="")	
	} else {
		main <- paste(main,", by ",deparse(substitute(x)) )	}	
	
	trimmedlow <- 0
	
	perm <- order(sort_by,decreasing=decreasing)
	
	if (!missing(lowtrim)){sort_by[sort_by <=lowtrim] <- NA; trimmedlow <- sum(is.na(sort_by))}
	if (!missing(hightrim)){sort_by[sort_by>=lowtrim] <- NA; trimmedhigh <- sum(is.na(sort_by))-trimmedlow}
	
		# if (!missing(lowtrim)){perm[sort_by <=lowtrim] <- NA; trimmedlow <- sum(is.na(perm))}
	# if (!missing(hightrim)){perm[sort_by>=lowtrim] <- NA; trimmedhigh <- sum(is.na(perm))-trimmedlow}


	
	
	if (missing(coli)) coli <- rank(sort_by,ties.method="random")
	if (decreasing) coli <- lenx- coli
	
	if (missing(col)) { if (missing(colfun)) usecol<- FALSE else
		{	usecol<- TRUE
			if (is.character(colfun))
				{if (colfun %in% c("grey", "gray")) col<-grey( (1:lenx)/lenx) else col<-colfun(lenx)} else
			col <- colfun(lenx)
		}
	} else {usecol<- TRUE}
	
	
	if (usecol){
		barplot(height[perm], main=main, col=col[coli[perm]], ...)
		if (trimlegend) {legnd(trimmedlow,trimmedhigh)}
		invisible(data.frame(x=height,perm=perm,coli=coli, col=col ))
	} else {
		barplot(height[perm], main=main,  ...)
		if (trimlegend) {legnd(trimmedlow,trimmedhigh)}
		invisible(data.frame(x=height,perm=perm,coli=coli ))

	}
}

