%$HeadURL$
\encoding{utf8}
\name{summary_terminals}
\alias{summary_terminals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tabulate leaf nodes}
% \description{
% %%  ~~ A concise (1-5 lines) description of what the function does. ~~
% }
\usage{
summary_terminals(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 an  \code{sprof} data structure.
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{A table of frequencies, bystack.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Günther Sawitzki <gsawitzki@r-forge.r-project.org>}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
	 data(sprof01lm)
	 summary_terminals(sprof01lm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{util}
