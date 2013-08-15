%$HeadURL$
%$Id$
%$Revision$
%$Date$
%$Author$
%$Name:$
\encoding{utf8}
\name{edgematrix}
\alias{edgematrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expand adjacency information to an edge table
}
\description{
Expand adjacency information from an adjacency matrix or a \code{sprof} data structure to an edge table
}
\usage{
edgematrix(data, counts = TRUE, na.rm = TRUE,  no.name="<nn>")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{an adjacency matrix or a \code{sprof} data structure.
}
  \item{counts}{include a column of counts
}
  \item{na.rm}{remove lines with a count \code{NA}.}
  \item{no.name}{If not null: replacement for empty strings as name.}

}
\details{The adjacency matrix is flattened. Lines with a count zero are eliminated.}
\value{
A data frame.
\item{from}{Name of from node.}
\item{to}{Name of to node.}
\item{count}{optional. Frequencies of edges.}
}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
\keyword{manip}
