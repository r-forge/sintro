\name{setcolalpha}
\alias{setcolalpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set alpha channel for colours}

\description{Set or modify the alpha chanels for colour specification}
\usage{
setcolalpha(col, alpha = "#40")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{
a colour specification, as 7 or 9 character hex specifcation (e.g. '#2050A0')
}
  \item{alpha}{
a hex specification for the alpha channel (e.g. '#80')}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a modified colour specifcation}
%\references{
%% ~put references to the literature/web site here ~
%}
 \author{G. Sawitzki \email{gsawitzki@r-forge.r-project.org}}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
col <- "#0A0B0C"
setcolalpha(col)

col <- "#0A0B0C10"
setcolalpha(col)

setcolalpha(col, alpha='#80')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
