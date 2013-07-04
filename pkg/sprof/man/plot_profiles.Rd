% $HeadURL:$
% $Id:$
\name{plot_profiles}
\alias{plot_profiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot profiling information on prfiel level.
}
\description{
Various plots of a profile. }
\usage{
plot_profiles(x, which = c(1L, 2L, 3L, 4L), col, ask = prod(par("mfcol")) < length(which) && dev.interactive(), src = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
preferably a \code{sprof} object. Other data structures may be extended
}
  \item{which}{
Selector of plots to show.
}
  \item{col}{
Colour table
}
  \item{ask}{
Ask for a new page
}
  \item{src}{
String to be used as source idetifier.}
  \item{\dots}{
passed.
}
}
\details{
%\if{html}{\figure{logo.jpg}{Our logo}}
%\if{latex}{\figure{logo.jpg}{options: width=0.5in}}
A current sample

\figure{sREplotprofiles4.png}
}
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
\references{See the vignette of package \code{sprof}.}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{plot.sprof}}, ~~~
}
\examples{
 data(sprof01lm)
 oldpar <- par(mfrow=c(2,2))
 plot_profiles(sprof01lm) 
 par(oldpar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
