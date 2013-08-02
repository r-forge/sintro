%$HeadURL$
\encoding{utf8}
\name{rrleb}
\alias{rrleb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recursive run length encoding bottom up.}
\description{
Encode a matrix as  run-length, bottom up.
Encoding  respects previous runs, e.g line n-1 encodes rns in each run of line n.

May be removed.
}
\usage{
rrleb(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a matrix.}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
list of run length encoded lines
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
 \code{\link{rle}}, ~~~
}
\examples{
x <- matrix(c(
	3,3,4,4,4,
	 5,5,6,6,7,
	 8,9,9,0,0,
	 1,1,1,2,2

 ),nrow=4, ncol =5, byrow=TRUE)
 xrrleb <- rrleb(x)
 xrrleb

 t(sapply(xrrleb, inverse.rle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
\keyword{matrix}

