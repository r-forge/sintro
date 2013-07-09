%$HeadURL$
\encoding{utf8}
\name{sprof-package}
\alias{sprof-package}
\alias{sprof}
\docType{package}
\title{sprof: Analysis of R profiles}
\description{
Profiling, timing and optimization utilitites}
\details{
\tabular{ll}{
Package: \tab sprof\cr
Type: \tab Package\cr
Version: \tab 0.0-1\cr
Date: \tab 2013-05-26\cr
License: \tab GPL-2\cr
}
\code{readProf} reads a profile file from \code{\link{Rprof}} or other profilers and 
returns a composite structure of class \code{sprof}.

To create a profile on the fly, use \code{\link{sampleRprof}}.

To import profile information written by \code{\link{Rprof}} or other profilers, 
use \code{\link{readRprof}}.

For  \code{sprof}, the usual access functions are supported.
}

\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
%\references{
%~~ Literature or other references for background information ~~
%}
\keyword{utilities}
\seealso{
%\code{\link[proftools:proftools-package]{proftools}}.
 
%\code{\link[profr:profr-package]{profr}}.
%\code{\link[profr]{profr-package}}

%\code{\link{profr}}.
 }
\examples{
\dontrun{
 data(sprof01lm)
 str(sprof01lm) 
 plot(sprof01lm) 
 }
}
