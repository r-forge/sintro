% File sintro/pkg/man/text.id.Rd
% Part of the R package, http://www.r-forge.R-project.org
% Copyright 1912-2013 G. Sawitzki
% Distributed under GPL 2 or later

\name{text_id}
\alias{text_id}
\alias{text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add labels to a plot.}
\description{Adds label to points in a a plot, unsing an index.}
\usage{
text_id(x, y, ind, labels.id = rownames(x), adj.x = TRUE, 
cex.id = 0.75, label.pos = c(4, 2), ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x, y}{numeric vectors of coordinates where the text
    \code{labels} should be written. }
 \item{ind}{vector of the indices of data points to be marked with a label.}
 \item{labels.id}{vector of the identtification strings of data points to be marked with a label.}
  \item{adj.x}{used to calculate \code{pos}.}  
  \item{cex.id}{numeric \bold{c}haracter \bold{ex}pansion factor; passed to \code{text}.}
  \item{label.pos}{used to calculate \code{pos}.}
  \item{\dots}{further \link{graphical parameters} (from \code{\link{par}}),
    such as \code{srt}, \code{family} and \code{xpd}.}

}

%\details{This is code extracted from function \code{locator}. }
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{This is code extracted from function \code{locator}. }
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{locator}},\code{\link{identify}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, ind, labels.id = rownames(x), adj.x = TRUE, cex.id = 0.75, 
    label.pos = c(4, 2), ...) 
{
    x <- x[ind]
    y <- y[ind]
    labels.id <- labels.id[ind]
    labpos <- if (adj.x) 
        label.pos[1 + as.numeric(x > mean(range(x)))]
    else 3
    text(x, y, labels.id, cex = cex.id, xpd = TRUE, pos = labpos, 
        offset = 0.25, ...)
  }
}% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
