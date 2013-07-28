%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}
\name{shownodes}
\alias{shownodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Show node information from a profile}

\description{
Plot node information from a profile in various plots.}
\usage{
shownodes(sprof, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{A data structure as returned by  \code{\link{readRprof}}.}
  \item{col}{A colour palette for the plots.}
}
\details{
Note: these figures may be outdated. Please run the examples.

Plots are from this collection:

\if{html}{\figure{rpoplotstacks.png}}
\if{latex}{\figure{rpoplotstacks.pdf}}
}
\value{
	Used for the side effect of showing the plots.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	\dontrun{
 		data(sprof01lm)
 		shownodes(sprof01lm) 
 	}
}
\keyword{hplot}
