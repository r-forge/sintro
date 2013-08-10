%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}
\name{nodesrunlength}
\alias{nodesrunlength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Marginal information for run length from profile}
\description{
Run length count, by node and run lenght, from profile.}
\usage{
nodesrunlength(sprof, clean=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a \code{sprof} data structure with profiling information.}
  \item{clean}{boolean.}
}
\details{
If \code{clean=TRUE}, zero results are removed and the nodes are sorted by average time.

If \code{clean=TRUE}, zero results and sorting are preserved. Trailing zero count nodes 
may have been lost in the process, and are  added.

}
\value{A matrix \code{count[node, run length]} with additonal columns
 \item{nr_runs}{sum of counts over all run lengths.}
 \item{total_time }{sum of \code{count*run length}}
 \item{avg_time }{\code{total_time / count}}
 }
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
		data(sprof01lm)
 		nodesrunlength(sprof01lm) 
		
		
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
