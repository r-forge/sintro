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
nodesrunlength(sprof)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a \code{sprof} data structure with profiling information.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A matrix \code{count[node, level|]} with additonal columns
 \item{count}{sum of counts over all run lengths.}
 \item{total\_time }{sum of \code{count*run length}}
 \item{tavg }{\code{total\_time / count}}
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
