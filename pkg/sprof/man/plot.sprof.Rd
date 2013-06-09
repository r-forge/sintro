%$HeadURL:$
\encoding{UTF-8}

\name{plot.sprof}
\alias{plot}
\alias{plot_nodes}
\alias{plot_stacks}
\alias{plot_profiles}

\title{plot for profiles}
\description{
plot a plot for the output of class \code{scode}.
}
\usage{
plot(object, \dots)
\method{plot}{sprof}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data structure as returned by  \code{\link{readProf}}.
}
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
 \code{\link{summaryRprof}}
}
\examples{
	\dontrun{
## Rprof() is not available on all platforms
profinterval <- 0.001
simruns <- 100

n <- 10000
x <- runif(n)
y0 <- 2+ 3 * x

sinknull <- textConnection(NULL, "w"); sink(sinknull)
Rprof(tmp <- tempfile(), interval = profinterval)
for (i in 1:simruns) {y <- y0 +  rnorm(n); xxx<- plot(lm(y~x))}
Rprof()

Rprof_out <- readProf(tmp)

unlink(tmp)
sink(); close(sinknull)

plot(Rprof_out)
}

}

\keyword{methods}
\keyword{utilities}
