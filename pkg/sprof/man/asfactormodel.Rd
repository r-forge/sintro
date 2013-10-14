%$HeadURL$
% source('~/projects/rforge/sintro/pkg/sprof/R/barplot_s.R', chdir = TRUE)
\encoding{utf8}
\name{asfactormodel}
\alias{asfactormodel}
\title{Convert to factor, like model}

\description{
Convert factor-like entries in a data structure to factor, with factor as in
\code{factormodel}.}
\usage{
asfactormodel(x, factormodel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data structure. Currently only integer vectors or lists of vectors are supported.}
  \item{factormodel}{A data structure to serve as model for the factor specification. Currently, a factor or a vector of type \code{character}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a data structure of same type as \code{x}, with numeric vectors converted to factors.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}

\note{This functionality could go to  \R base function \code{\link{factor}}.

Should be extended to cover a wide range of data structures, and identify substructures for conversion.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
	data(sprof01lm)
	
xf <- asfactormodel(sprof01lm$stacks$nodes, sprof01lm$nodes$name)

# To get a character listing, use
sapply(xf, function (x) {as.character(x)} )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
