%$Id$
\name{parcoord}
\alias{parcoord}
\title{
Parallel Coordinates Plot
}
\description{
Parallel coordinates plot
}
\usage{
parcoord(x, col = 1, lty = 1, var.label = FALSE, 
group, perm, negate, rep = FALSE, \ldots)
}
%
\arguments{
\item{x}{
a matrix or data frame who columns represent variables.  Missing values
are allowed.
}
\item{col}{
A vector of colours, recycled as necessary for each observation.
}
\item{lty}{
A vector of line types, recycled as necessary for each observation.
}
\item{var.label}{
If \code{TRUE}, each variable's axis is labelled with maximum and
minimum values.
}
  \item{group}{
A group variable to select colours. \code{col} may supersede this. 
}
  \item{perm}{
A permutation, or any index sequence, selecting the variable to display and their order. Repetitions are allowed.
}
  \item{negate}{
Variables to be shown in inverse scale. Names are modified to mark this.
Indices are in original (unpermuted, unselected) order.
}
  \item{rep}{
Repeat display cyclically.}

\item{\dots}{
Further graphics parameters which are passed to \code{matplot}.
}
}
\section{Side Effects}{
a parallel coordinates plots is drawn.
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{

  

Inselberg, A. (1985) {The plane with parallel coordinates}
	\emph{The Visual Computer} \bold{1}, 69--91.
	
Inselberg, A. (2008) \emph{Parallel Coordinates}
	Springer, Heidelberg.


  Wegman, E. J. (1990) Hyperdimensional data analysis using parallel
  coordinates.
  \emph{Journal of the American Statistical Association}
  \bold{85}, 664--675.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\author{
  B. D. Ripley.  Enhancements based on ideas and code by Fabian Scheipl.\\
  Modified by G. Sawitzki.
}
\examples{
parcoord(state.x77[, c(7, 4, 6, 2, 5, 3)])

parcoord(state.x77, perrm=c(7, 4, 6, 2, 5, 3))

ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)\%/\%50)
}
\keyword{hplot}

