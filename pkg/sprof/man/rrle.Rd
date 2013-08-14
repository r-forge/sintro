%#$HeadURL$
%#$Id$
%#$Date$
%#$Author$
%#$Revision$
\encoding{utf8}
\name{rrle}
\alias{rrle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Recursive run length encoding.}
\description{
Encode a matrix as  run-length, top down.
Encoding  respects previous runs, e.g line 2 encodes runs in each run of line 1.
}
\usage{rrle(x, collapseNA =FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix.}
  \item{collapseNA}{boolean. Collapse runs of NA.}
}
\details{By default, different NA data are not considered equal. \code{collapseNA} collapses runs of NA in the result. For 
recursion however they are treated as singular data, not as runs. This may need discussion.}
\value{
list of run length encoded lines
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
\note{This could go to \code{rle} in package \code{base}.

The date structure used for \code{rle} would be better represented as a data.frame.

Run length and other compressions might be implemented in data.frames by default.}

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{rrleb}}
}
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
\keyword{manip}
\keyword{utilities}
\keyword{matrix}

