%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/man/sprof-package.Rd $
\encoding{utf8}
\name{rrle}
\alias{rrle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recursive run lenth encoding.}
\description{
Encode a matrix as  run-length, top down.
Encoding  respects previous runs, e.g line 2 encodes rns in each run of line 1.
}
\usage{
rrle(x)
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
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
x <- matrix(c(
	 1,1,1,2,2,
	3,3,4,4,4,
	 5,5,6,6,7,
	 8,9,9,0,0
 ),nrow=4, ncol =5, byrow=TRUE)
 xrrle <- rrle(x)
 xrrle

 t(sapply(xrrle, inverse.rle))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{util}
