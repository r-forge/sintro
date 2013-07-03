%$URLHead:$
\encoding{UTF-8}
\name{sampleRprof}
\alias{sampleRprof}
\title{Get a sample profile}
\description{
Get a sample profile and return it as a \code{sprof} data structure.
}
\usage{
sampleRprof(expr, runs = NULL, gcFirst = TRUE, interval = 0.001, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{an expression to be profiled.}
  \item{runs}{nr of runs to profile.}
  \item{gcFirst}{boolean. Profile GC.}
   \item{interval}{Real: time interval between samples, in s.}
   \item{\dots}{additional parameters, passed to \code{Rprof}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A list of type \code{sprof}
}
\references{  \url{http://sintro.r-forge.r-project.org/} }
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
	res_lm <- sampleRprof(for (i in 1:1000) yy<- lm(runif(1000)~rnorm(1000)), runs=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
