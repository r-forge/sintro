%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/man/sprof-package.Rd $
%$Id: sprof-package.Rd 214 2013-08-20 18:21:08Z gsawitzki $
%$Revision: 214 $
%$Date: 2013-08-20 20:21:08 +0200 (Tue, 20 Aug 2013) $
%$Author: gsawitzki $
%$Name:$
\encoding{utf8}
\name{stacksasfactor}
\alias{stacksasfactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Represent stacks as factor
}
\description{Represent stacks as factor, using the node information of the profile record}
\usage{
stacksasfactor(sprof, sel, events)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a \code{sprof} data structure.}
  \item{sel}{Indices of stacks to convert.}
  \item{events}{Events, stacks of which to convert}
}

\details{\code{sel} and \code{events} are exclusive.

If none is selected, all stacks are given.
}
\value{A vector, or a list of vectors, repreenting the selected stacks as factors.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
\note{event based selection should report event numbers.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 data(sprof01lm)
stacksasfactor(sprof01lm, 1:3)

stacksasfactor(sprof01lm, events=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
