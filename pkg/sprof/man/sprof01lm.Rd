%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}
\name{sprof01lm}
\alias{sprof01lm}
\docType{data}
\title{
sprof sample data
}
\description{
An example data set for the functions in package \code{sprof}.
}
\usage{data(sprof01lm)}
\format{
  The format is:
A List of 4
 $ info    :'data.frame':	1 obs. of  8 variables:
 $ nodes   :'data.frame':	62 obs. of  5 variables:
 $ stacks  :'data.frame':	50 obs. of  7 variables:
 $ profiles:List of 4
 }
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{See the vignette of package \code{sprof}.}
\examples{
 data(sprof01lm)
 str(sprof01lm) 
 plot(sprof01lm) 
}
\keyword{datasets}
