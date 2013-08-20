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
\details{
%\if{html}{\figure{logo.jpg}{Our logo}}
%\if{latex}{\figure{logo.jpg}{options: width=0.5in}}
These displays may be outdated. Please run the examples. Plots are from this collection:

\if{html}{\figure{plotsprof.png}{options: width=6.0in}}
\if{latex}{\figure{plotsprof.pdf}}
}
\value{subject to change}
\references{  \url{http://sintro.r-forge.r-project.org/} }
\note{
See the vignette for in-context explanations.

Displays of the graph structure are given in the vignette.

The \code{plot.sprof} method for \code{sprof} objects concatenates 
 three plot functions. Using the plot functions one by one allows better control and will be preferred. \code{\link{shownodes}} may be a sufficient summary.
}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{shownodes}}

\code{\link{summaryRprof}}  
 
\code{\link{plot_nodes}}
\code{\link{plot_stacks}}
\code{\link{plot_profiles}}
}
\examples{
data(sprof01lm)

oldpar <- par(mfrow=c(3,4))
plot.sprof(sprof01lm)
par(oldpar)

}


\keyword{hplot}
\keyword{utilities}
