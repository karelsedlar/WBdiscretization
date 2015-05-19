\name{get.consensus}
\alias{get.consensus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a consensus from discrete-valued time series
}
\description{
Get a consensus vector from discrete-valued time series matrix.
}
\usage{
get.consensus(D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
Data frame or matrix of discrete-valued time series, each row corresponding to one time series. Each row of this
matrix contains discrete-valued measurements for one gene on a time line, i. e.
column i+1 contains the successor states of column i.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{K}{
Vector of one discrete-valued time series, containing consensual result for input matrix. Vector contain discrete-valued measurements for on a time line, i. e. column i+1 contains the successor states of column i.
}
}
\references{
Sedlar. K. Zpracovani experimentalnich dat ve vazbe na modely FGFR signalnich drah, diplomova prace, 2015
}
\author{
Karel Sedlar
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
frs2_d <- matrix(c(1,1,1,0,0,1,0,1,1,1,1,0,1,1,0), nrow=3)
get.consensus(frs2_d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
