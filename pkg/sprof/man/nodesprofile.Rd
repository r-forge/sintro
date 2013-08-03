%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/man/nodescloud.Rd $
%$Id: nodescloud.Rd 192 2013-08-02 09:59:03Z gsawitzki $
%$Date: 2013-08-02 11:59:03 +0200 (Fri, 02 Aug 2013) $
%$Author: gsawitzki $
%$Revision: 192 $
\encoding{utf8}
\name{nodesprofile}
\alias{nodesprofile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run length matrix
}
\description{
Extracts run length information from a \code{sprof} data structure with profiling information.}
\usage{
nodesprofile(sprof)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{
a \code{sprof} data structure with profiling information.
}
}
\details{
 Run lenght counts by node, level and run length.
}
\value{
    \code{counts[ node, level, run length]} .}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
\note{This expands a sparse matrix to full. Avoid to use it.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
		data(sprof01lm)
 		str(nodesprofile(sprof01lm) )
		}
\keyword{manip}
