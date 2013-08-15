%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{sprof-package}
\alias{sprof-package}
\alias{sprof}
\docType{package}
\title{sprof: Analysis of R profiles}
\description{
Profiling, timing and optimisation utilities}
\details{
\tabular{ll}{
Package: \tab sprof\cr
Type: \tab Package\cr
Version: \tab 0.0-6\cr
Date: \tab 2013-08-15\cr
License: \tab GPL-2 | GPL-3\cr
}
\code{\link{readRprof}()} reads a profile file from \code{\link{Rprof}()} or other profilers and 
returns a composite structure of class \code{sprof}. The basic components of \code{sprof} are (conceptually) four data frames
\tabular{ll}{\code{info} \tab general information and summaries\cr
\code{nodes} \tab node specific information\cr
\code{stacks} \tab node specific information. Stacks are random snapshots from\cr
 \tab 
the program execution, possibly including side 
information such as traces \cr
\tab of the memory management.\cr
\code{profiles} \tab collected records of a profile, encoded as references to stacks
}

To create a profile on the fly, use \code{\link{sampleRprof}}.

To import profile information written by \code{\link{Rprof}} or other profilers, 
use \code{\link{readRprof}}.

For  \code{sprof}, the usual access functions are supported.
}

\note{Version 0.0-6 is a clean-up version. Recommendations/requests for the interface 
definition are requested at this point.}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
%\references{
%~~ Literature or other references for background information ~~
%}
\keyword{utilities}
%\keyword{internal}
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
