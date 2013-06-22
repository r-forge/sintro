\name{list.as.matrix}
\alias{list.as.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert list to matrix}
\description{
Convert list to matrix. List entries go to matrix columns, filled for equal length. 
}
\usage{
list.as.matrix(x, filler = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a list of numeric vectors.
}
  \item{filler}{
a value to be used as a filler}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A matrix with the values from x, filled to matrix shape.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
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
