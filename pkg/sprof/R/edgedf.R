#$HeadURL$
#$Id$
#$Date$
#$Author$
#$Revision$
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/edgedf.R', chdir = TRUE)
#! To Do
#!

#\name{edgedf}
#\title{edgem matrix}
#\desciption{Build an edge matrix matrix from profile information}

#\details


		 # to=c("T1","T2","T3","T4"))

#\name{edgedf}
#\alias{edgedf}
#\title{edgedf matrix}
#\desciption{Build an edgedf matrix from profile information}

edgedf <- function(data, counts=TRUE, na.rm=TRUE, no.name="<nn>") {
# allow sprof as parameter
  if (inherits(data,"sprof"))   data <- adjacency(data)
  if (!is.matrix(data)) stop("edgedf: data must be a matrix")
# adj -> edgelist
  #varnames = names(dimnames(data))
  dn <- dimnames(data)
  names(dn) <- names(dimnames(data))
  labels <- expand.grid(lapply(dn, function(x) 
  		if(is.character(x)) type.convert(x) else x), 
  		KEEP.OUT.ATTRS = FALSE,
    	stringsAsFactors = FALSE)

  if (na.rm) {
    missing <- is.na(data)
    data <- data[!missing]
    labels <- labels[!missing, ]
 }
 
 	if (!is.null(no.name)){ 
			labels[labels[]==""]  <- no.name
			}

if (counts) {
  count <- setNames(data.frame(as.vector(data)), "count")
  em <- cbind(labels, count)
    return(em[em[,"count"]!=0,])
  }  else return(labels[count!=0])
}

