%$HeadURL:$
\encoding{UTF-8}

\name{print.sprof}
\alias{print}
\alias{print_nodes}
\alias{print_stacks}
\alias{print_profiles}

\title{print for profiles}
\description{
Print a print for the output of class \code{scode}.
}
\usage{
print(object, \dots)
\method{print}{sprof}(object, \dots)
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
for (i in 1:simruns) {y <- y0 +  rnorm(n); xxx<- print(lm(y~x))}
Rprof()

Rprof_out <- readProf(tmp)

unlink(tmp)
sink(); close(sinknull)

print(Rprof_out)
}

}

\keyword{methods}
\keyword{utilities}
