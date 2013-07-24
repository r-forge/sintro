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
Version: \tab 0.0-5\cr
Date: \tab 2013-07-23\cr
License: \tab GPL-2 | GPL-3\cr
}
\code{\link{readRprof}()} reads a profile file from \code{\link{Rprof}()} or other profilers and 
returns a composite structure of class \code{sprof}. The basic components of \code{sprof} are (conceptually) four data frames
\tabular{ll}{
\code{info} \tab general information and summaries\cr
\code{nodes} \tab node specific information\cr
\code{stacks} \tab node specific information. Stacks are random snapshots from
the program execution, possibly including side information such as traces of the memory management\cr
\code{profiles} \tab collected records of a profile, encoded as references to stacks
}

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
%\seealso{
%\code{\link[proftools:proftools-package]{proftools}}.
 
%\code{\link[profr:profr-package]{profr}}.
%\code{\link[profr]{profr-package}}

%\code{\link{profr}}.
% }
\examples{
\dontrun{
 data(sprof01lm)
 str(sprof01lm) 
 plot(sprof01lm) 
 }
}
