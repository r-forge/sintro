#
# setwd("")
#! To Do
#!
# A wrapper around Rprof


sampleRprof <- function(expr, runs=NULL, gcFirst = TRUE, ...){
	#browser()
	#sinknull <- textConnection(NULL, "w"); sink(sinknull)
	gcin <- NULL; gcout <- NULL
	if (gcFirst) 
        gcin <- gc(FALSE)
    if (is.null(runs)){
	Rprof(tmp <- tempfile(), ...)
	    expr
	Rprof(NULL)} else { #! should use "append"
	Rprof(tmp <- tempfile(), ...)
	    for (i in seq(length.out=max(runs,0))) expr
	Rprof(NULL)
	}

    browser()
	sprof_out <- readProf(tmp)

	unlink(tmp)
	#sink(); close(sinknull)
	if (gcFirst) 
        gcout <- gc(FALSE)
	if (!is.null(gcin)) {sprof_out$gcin <- gcin;sprof_out$gcout <- gcout}
	browser()
		if (is.null(sprof_out) || (sprof_out$info$nrrecords==0)) warning("No event data recorded.")
	return(sprof_out)
}# sampleRprof

# res_runif <- sampleRprof( yy<- runif(1000), runs=100)
# res_lm <- sampleRprof(for (i in 1:1000) yy<- lm(runif(1000)~rnorm(1000)), runs=100)