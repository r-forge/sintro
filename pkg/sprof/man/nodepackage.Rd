%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{nodepackage}
\alias{nodepackage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find a package that may contain a node}

\description{ \code{\link{getAnywhere}}() is used to look up \code{x}, and the package or namespace information
is used to give a source package.
}
\usage{nodepackage(x)}
\arguments{
  \item{x}{a character string or name, or a vector.}
}
\details{
There is no indication whether the information is from a namespace or from a package information.

No indication is given if multiple hits are encountered.

The information is based on the run time environment of this function. This may be different from the environment
the object is taken from.

See the help information for \code{\link{getAnywhere}}() for more warnings.
}
\value{a character string or a vector of strings with the package names.}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getAnywhere}}()
}
\examples{
nodepackage("getAnywhere")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{util}
