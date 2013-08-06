%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{adjacency}
\alias{adjacency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sprof to adjacency matix}
\description{
convert node information from a \code{sprof} structure to adjacancy matrix.}
\usage{
adjacency(sprof, keep.names = TRUE, rmzero=TRUE,  no.name="<nn>")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{ a \code{sprof} structure.}
  \item{keep.names}{boolean. Copy node names as row- and column names.}
  \item{rmzero}{boolean. Remove nodes with no edges.}
  \item{no.name}{If not null: replacement for empy strings as name.}
  }
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{a  correspondency matrix}
\references{See the vignette of package \code{sprof}.}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
 data(sprof01lm)
 adjacency(sprof01lm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
