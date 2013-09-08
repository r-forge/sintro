%$HeadURL$
% source('~/projects/rforge/sintro/pkg/sprof/R/barplot_s.R', chdir = TRUE)
\encoding{utf8}
\name{barplot_s}
\alias{barplot_s}
\title{Sorted Bar Plots}
\description{
  Creates a sorted bar plot with vertical or horizontal bars.}

\usage{
barplot_s(height, 
	horiz = FALSE,
	sort_by, 
	decreasing = TRUE, 
	lowtrim, hightrim, trimlegend = TRUE, 
	col, coli, colfun, 
	main, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{height}{either a vector or matrix of values describing the
bars which make up the plot. See \code{\link{barplot}}.}
\item{horiz}{boolean. Arrange bars horizontally.}
\item{sort_by}{a variable to sort by. Defaults to \code{height}.}
\item{decreasing}{boolean. Sorting direction.}
\item{lowtrim}{A optional lower trim value. Observations with \code{sort_by} values up to \code{lowtrim} are discarded.}
\item{hightrim}{A optional upper trim value. Observations with \code{sort_by} values from \code{lowtrim} are discarded.}  
\item{trimlegend}{Boolean. Show a legend about trimmed values.}
\item{col}{a vector of colors for the bars or bar components.
    By default, grey is used if \code{height} is a vector, and a
    gamma-corrected grey palette if \code{height} is a matrix.}
    
\item{coli}{An index into the  \code{col} table, based on original sorting. If no index is given, the colour will be allocated after sorting.}
\item{colfun}{A function or function name to generate a \code{col} palette. \code{"grey"} or \code{"gray"} is rescaled to 1..n.}
\item{main}{overall title for the plot}
\item{\dots}{Passed to \code{barplot} }
}
\details{
If no names are supplied, they are generated in the form x...

You man want to adjust the scale of the bar labels by using an additional argument such as \code{cex.names =0.5}.

More to come.
	Plots are among others from this collection:

\if{html}{\figure{barplots.png}}
\if{latex}{\figure{barplots.pdf}}

}
\value{Invisible: A data frame with components
  \item{\code{x}}{height}
  \item{\code{perm}}{the permutation applied}
  \item{coli}{the colour index applied}
  \item{col}{optional: the colours selected}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

\note{Part of this could go to the \R base function \code{\link{barplot}}.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{barplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- runif(100)

barplot_s(x)

x <- rnorm(100)

barplot_s(x, colfun=heat.colors, lowtrim=-1)

rm(x)

}
\keyword{hplot}
