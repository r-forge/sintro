%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/man/summary.sprof.Rd $
\encoding{utf8}
\name{writeRprof}
\alias{writeRprof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write profile data
}
\description{
Write a profile data file from a \code{sprof} data structure.
}
\usage{
writeRprof(sprof, filename = "Rprof.Out")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a data structure from package  \code{sprof}
}
  \item{filename}{
    The file to be used for exporting the profiling results.
  }
}
\details{
\code{writeRprof} only writes the stack entries for the profile.

This can be used to export information   after preprocessing with \code{sprof} to 
some package designed for \code{Rprof} output.
}
\value{
An invisble list with the profile entries, headers removed.
}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
\references{  \url{http://sintro.r-forge.r-project.org/} }
\note{
See the vignette for in-context explanations.

Displays of the graph structure are given in the vignette.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
	data(sprof01lm)
	writeRprof(sprof01lm)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
