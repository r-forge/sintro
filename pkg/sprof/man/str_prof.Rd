%$HeadURL: svn+ssh://gsawitzki@svn.r-forge.r-project.org/svnroot/sintro/pkg/sprof/man/sprof-package.Rd $
\encoding{utf8}
\name{str_prof}
\alias{str_prof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
str_prof(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    cat("First line:", x$firstline, "\n")
    cat(length(x$data), "Sampling intervals ")
    if (length(x$timesRLE[1]) == 1) 
        cat(" at", x$timesRLE[[2]], "micros\n")
    else {
        cat("in micros: ")
        print(x$timesRLE)
    }
    cat(length(x$nodes$name), "nodes in", length(x$stacks$stacks), 
        "stacks\n")
    cat(length(unique(x$stacks$stackleafnodes)), "Terminals ", 
        "\n")
    roots <- unique(x$stacks$stackheadnodes)
    cat(length(roots), "Roots: ")
    print(table(x$nodes$name[x$stacks$stackheadnodes]))
    cat("\n", deparse(substitute(x)), " Structure: ")
    str(x, max.level = 1)
    cat("\n", "stacks Structure: ")
    str(x$stacks, max.level = 1, vec.len = 2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
