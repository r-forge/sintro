%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{list.as.matrix}
\alias{list.as.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert list to matrix}
\description{
Convert list to matrix. List entries go to matrix columns, filled for equal length. 
}
\usage{
list.as.matrix(x, byrow=FALSE, filler = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list of numeric vectors.}
  \item{byrow}{boolean. Arrange list entries as rows. Default is to use columns.}
  \item{filler}{a value to be used as a filler}
}
\note{This could go to \code{as.matrix()}.}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A matrix with the values from x, filled to matrix shape.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

\section{To do}{Arguments should be as \code{as.matrix}.

A corresponding list.as.data.frame should be added for mixed types.

Name synchronisation/preservations should be added.
} 

\seealso{\code{\link{as.matrix}}.}
\examples{
x <- list(x1=c(1,2,3),x2=3, x3=4:8)
list.as.matrix(x)
list.as.matrix(x,filler=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{matrix}
\keyword{list}
