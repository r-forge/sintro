%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}
\name{profiles_matrix}
\alias{profiles_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract a node incidence matrix from profile information.
}
\description{
Extract a node incidence matrix from profile information.}
\usage{
profiles_matrix(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{sprof} data structure.
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{an incidencematrix, NA filled.}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
 data(sprof01lm)
 smat <-profiles_matrix(sprof01lm)
 image(smat)
 }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{util}
