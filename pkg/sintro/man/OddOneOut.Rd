%
\name{OddOneOut}
\alias{OddOneOut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OddOneOut
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Gives a series of plot matrices and waits for the user to select the figure in the matrix that is off.
}
\usage{
OddOneOut(goodplot = function(n, ...){plot(rnorm(n))}, 
badplot = function(n, ...){plot(runif(n))}, 
n = 100, runs = 10, nrows = 4, ncols = 4, training = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{goodplot}{
A function that shows \code{n} random variates from a ``good'' distribution. This is shown in all but one panels of the
plot matrix.
%%     ~~Describe \code{goodplot} here~~
}
  \item{badplot}{
%%     ~~Describe \code{badplot} here~~
A function that shows \code{n} random variates from a ``good'' distribution. This is shown in a random panel of the
plot matrix.
}
  \item{n}{
%%     ~~Describe \code{n} here~~
Sample size.
}
  \item{runs}{
%%     ~~Describe \code{runs} here~~
Number of plot matrices to generate.
}
  \item{nrows}{
%%     ~~Describe \code{nrows} here~~
Number of rows in the plot matrix.
}
  \item{ncols}{
%%     ~~Describe \code{ncols} here~~
Number of columns in the plot matrix.
}
  \item{training}{
%%     ~~Describe \code{training} here~~
Flag. if set to \code{FALSE}, the axis and labels are not shown in the panels.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
Additional parameters, passed to the panel functions.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This is the wrapper for the running exercises in Chapter 1 of (Sawitzki, 1989).
In short, the task is to find the empirical power of some diagnostic plot, and later to estimate the
empirical relative efficiency.
}
\value{
%%  ~Describe the value returned
A matrix with columns \code{"oddrow", "oddcol", "selrow", "selcol"}  with the location of the odd
sample, and the location of the user response.
}
\references{
Sawitzki, G.: Computational Statistics/Introduction to R.
	Chapman & Hall/CRC Press, Boca Raton (FL), 2009.
        ISBN: 978-1-4200-8678-2.
	 \url{http://sintro.r-forge.r-project.org/}
}
\author{G. Sawitzki <gsawitzki@users.r-forge.r-project.org>}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{

%rnormplot <- function(n, ...){plot(rnorm(n))}
%runifplot <- function(n, ...){plot(runif(n))}

OddOneOut(runs=5, ncols=3)
OddOneOut(runs=5, ncols=3, training=FALSE)

}
## The function is currently defined as
function (goodplot = function(n, ...){plot(rnorm(n))}, 
	badplot = function(n, ...){plot(runif(n))}, 
	n = 100,  runs = 10, 
	nrows = 4, ncols = 4, training = TRUE, ...) 
{
cat("One plot is out. Please click on the odd panel in the plot. Abort with <esc>.\n")
cat("There are ",runs, "runs.\n")
    restab <- matrix(nrow = runs, ncol = 4)
    colnames(restab) <- c("oddrow", "oddcol", "selrow", "selcol")
    nrplots <- nrows * ncols
    for (i in 1:runs) {
        if (training) {
            oldpar <- par(mfrow = c(nrows, ncols))
        }
        else {
            oldpar <- par(mfrow = c(nrows, ncols), ann = FALSE, 
                xaxt = "n", yaxt = "n")
        }
        oddone <- sample(nrplots, 1) - 1
        for (j in 1:nrplots - 1) {
            row <- (j\%/\%ncols) + 1
            col <- (j\%\%ncols) + 1
            if (j == oddone) {
                badplot(n)
                badrow <- row
                badcol <- col
            }
            else goodplot(n)
        }
        par(mfrow = c(1, 1))
        plot.window(c(1, ncols), c(nrows, 1))
        locres <- locator(1)
        restab[i, ] <- c(badrow, badcol, round(locres$y), round(locres$x))
    }
    restab
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
