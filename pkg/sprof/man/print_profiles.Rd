%$HeadURL$
%$Id$
%$Date$
%$Author$
%$Revision$
\encoding{utf8}
\name{print_profiles}
\alias{print_profiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print profile information
}
\description{
Print profile information.

}
\usage{
print_profiles(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a \code{sprof} data structure.
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{none
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Günther Sawitzki <gsawitzki@users.r-forge.r-project.org>}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
	 data(sprof01lm)
	print_profiles(sprof01lm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
