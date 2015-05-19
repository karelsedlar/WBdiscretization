\name{hier.discretization}
\alias{hier.discretization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discretize a set of real-valued time series using hierarchical clustering
}
\description{
Discretize a set of real-valued time series using hierarchical clustering into one consensual discrete-valued time series.
}
\usage{
hier.discretization(X,
                    l = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame or matrix of real-valued time series, each row corresponding to one time series. Each row of this
matrix contains real-valued measurements for one gene on a time line, i. e.
column i+1 contains the successor states of column i.
}
  \item{l}{
Number of dicrete level used for dicretization. Default is 2. 
}
}
\details{
Euclidean distance is used for distance computations and UPGMA technique for data clustering.
}
\value{
\item{D}{
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
frs2 <- matrix(rnorm(30,10000,50000),nrow=3)
hier.discretization(frs2, l=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
