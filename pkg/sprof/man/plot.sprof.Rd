%$HeadURL$
\encoding{UTF-8}

\name{plot.sprof}
%\alias{plot}
\alias{plot.sprof}

\title{plot for profiles}
\description{
plot a plot for the output of class \code{scode}.
}
\usage{
%plot(object, \dots)
\method{plot}{sprof}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data structure as returned by  \code{\link{readRprof}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{subject to change}
\references{  \url{http://sintro.r-forge.r-project.org/} }
\note{
See the vignette for in-context explanations.

Displays of the graph structure are given in the vignette.
}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{summaryRprof}}  
 
 \code{\link{plot_profiles}}
 
 \code{\link{plot_nodes}}
\code{\link{plot_stacks}}

}
\examples{
data(sprof01lm)

plot.sprof(sprof01lm)

%oldpar <- par(mfrow=c(2,2))
%plot_nodes(sprof01lm)
%par(oldpar)
%
%oldpar <- par(mfrow=c(2,2))
%plot_stacks(sprof01lm)
%par(oldpar)
%
%oldpar <- par(mfrow=c(2,2))
%plot_profiles(sprof01lm)
%par(oldpar)

}


\keyword{methods}
\keyword{utilities}
