#
# setwd("")
#! To Do
#!
# A wrapper around Rprof


sampleRprof <- function(expr, runs=NULL, gcFirst = TRUE, ...){
	browser()
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

    #browser()
	Rprof_out <- readProf(tmp)

	unlink(tmp)
	#sink(); close(sinknull)
	if (gcFirst) 
        gcout <- gc(FALSE)
	if (!is.null(gcin)) {Rprof_out$gcin <- gcin;Rprof_out$gcout <- gcout}
		if (is.null(Rprof_out) || is.null(Rprof_out$data)) warning("No event data recorded.")
	return(Rprof_out)
}# sampleRprof

# ress0Rp <- sampleRprof( yy<- runif(1000), runs=100)
# ressRp <- sampleRprof(for (i in 1:1000) yy<- lm(runif(1000)~rnorm(1000)), runs=100)