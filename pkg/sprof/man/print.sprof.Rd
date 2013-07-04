%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}

\name{print.sprof}
\alias{print.sprof}
\alias{print_nodes}
\alias{print_stacks}

\title{print for profiles}
\description{
Print a print for the output of class \code{scode}.
}
\usage{
%print(x, \dots)
\method{print}{sprof}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data structure as returned by  \code{\link{readRprof}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{None.}
\references{  \url{http://sintro.r-forge.r-project.org/} }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{summaryRprof}} \code{\link{plot_profile}}
}
\examples{
data(sprof01lm)
print(sprof01lm)
}

\keyword{methods}
\keyword{utilities}
