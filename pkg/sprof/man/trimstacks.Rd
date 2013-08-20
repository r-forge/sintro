%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{trimstacks}
\alias{trimstacks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trim \code{sprof} stacks.
}
\description{
Trim \code{sprof} stack information by top level or by node.
}
\usage{
trimstacks(sprof, level, trimnode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a \code{sprof} data structure, if available, or a \code{stacks$nodes} information.}
  \item{level}{Level to cut off.}
  \item{trimnode}{A node considered end of scaffold. Entries up to and
  including this node are trimmed. Nodes can be marked as index, or as node name.)
}
}
\details{The \code{level} information is evaluated first, and then the node information is evaluated.
}
\value{
A data structure corresponding to \code{stacks$nodes}, with the trimmed parts cut off. This may contain NULL entries.}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
 data(sprof01lm)
 trimmed <- trimstacks(sprof01lm, trimnode="summary")
sprof01lm$stacks$nodes <- trimmed  #now the refereces are update, and graphs will work
sprof02lm <- updateRprof(sprof01lm) #now the info is updated, and summeries will be consistent.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
