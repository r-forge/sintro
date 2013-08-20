%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{roots_sprof}
\alias{roots_sprof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Root nodes list for \code{sprof}
}
\description{
Return a list of root nodes of all stacks.
}
\usage{
roots_sprof(sprof, stacks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sprof}{a \code{sprof} data structure, if available}
  \item{stacks}{an optional list of stacks as references to nodes
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{A vector of unique entries as root of stacks. If \code{sprof} is provided, nmaes are imported from the node name table of \code{sprof}
}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

\section{To do }{\code{stacks} should support any reasonable representation of the stacks, and preserve format.} 

\examples{
data(sprof01lm)
roots_sprof(sprof01lm)
trimmed <- trimstacks(sprof01lm, trimnode= "summary")
roots_sprof(sprof01lm, stacks=trimmed)

}


\keyword{misc}
