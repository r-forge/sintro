#$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/R/shownodes.R $
#$Id: shownodes.R 178 2013-07-23 19:13:13Z gsawitzki $
#$Date: 2013-07-23 21:13:13 +0200 (Tue, 23 Jul 2013) $
#$Author: gsawitzki $
#$Revision: 178 $
#\encoding{utf8}
# setwd("")
# source('~/Documents/lectures/src/insider/profile/sprof/pkg/R/edgematrix.R', chdir = TRUE)
#! To Do
#!

#\name{edgematrix}
#\title{edgem matrix}
#\desciption{Build an edge matrix matrix from profile information}

#\details


		 # to=c("T1","T2","T3","T4"))

#\name{edgematrix}
#\alias{edgematrix}
#\title{edgematrix matrix}
#\desciption{Build an edgematrix matrix from profile information}

edgematrix <- function(data, counts=TRUE, na.rm=TRUE) {
# allow sprof as parameter
  if (inherits(data,"sprof"))   data <- adjacency(data)
  if (!is.matrix(data)) stop("edgematrix: data must be a matrix")
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
if (counts) {
  count <- setNames(data.frame(as.vector(data)), "count")
  em <- cbind(labels, count)
    return(em[em[,"count"]!=0,])
  }  else return(labels[count!=0])
}

