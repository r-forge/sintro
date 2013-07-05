\name{plot_nodes}
\alias{plot_nodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot profiling information on node level.}
\description{Various plots of a profile. }

\usage{
plot_nodes(x, which = c(1L, 2L, 3L), col = NULL, 
ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
src = NULL, mincount = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	  \item{x}{preferably a \code{sprof} object. Other data structures may be extended}
  \item{which}{Selector of plots to show.}
  \item{col}{
Colour table
}
  \item{ask}{boolean. Ask for a new page?}
  \item{src}{
String to be used as source identifier.}
  \item{mincount}{minimum total frequency count for node to be shown in barcharts.}

  \item{\dots}{passed.}

}

\details{
%\if{html}{\figure{logo.jpg}{Our logo}}
%\if{latex}{\figure{logo.jpg}{options: width=0.5in}}
Plots are from this collection:

\if{html}{\figure{rpoplotnodes.png}}
\if{latex}{\figure{rpoplotnodes.pdf}}

}
\value{
	To come.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{See the vignette of package \code{sprof}.}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
\note{See the vignette of package \code{sprof}.}

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{plot.sprof}}, ~~~
}
\examples{
 data(sprof01lm)
 oldpar <- par(mfrow=c(2,2))
 plot_nodes(sprof01lm) 
 par(oldpar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
