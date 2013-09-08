%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{rkindex}
\alias{rkindex}
\title{Index, based on rank.}
\description{
Convert \code{x} to an index in 1\ldots \code{maxindex}}
\usage{
rkindex(x, maxindex = length(x), pwr = 1, ties.method = "random", id)
}
\arguments{
  \item{x}{
A vector of data to covert.}
  \item{maxindex}{
Maximum for result}
  \item{pwr}{See details.}
  \item{ties.method}{passed to \code{rank}.}
  \item{id}{A string to be passed as id argument. If missing, an id will be generated from the arguments.}
}
\details{
\code{x} is transformed to a rank scale, using \code{ties.method}.
It is then rescaled to [0,1], and (optionally) a power transformation is
applied. In visualisation terms, this is a gamma correction. The result is rescaled to 1 \dots maxindex.}
\value{
A vector of rescaled values. }
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
 x <- runif(7)
 x
 str(rkindex(x, maxindex=5))
 str(rkindex(x, maxindex=5, pwr=0.5))
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{util}
