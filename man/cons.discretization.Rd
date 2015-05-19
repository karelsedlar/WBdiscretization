\name{cons.discretization}
\alias{cons.discretization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Binarize a set of real-valued time series into consensual discrete-valued time series
}
\description{
Binarize a set of real-valued time series row by row according to chosen absolute value into one consensual discrete-valued time series. Every row represents one time serie.
}
\usage{
cons.discretization(X,
                   method = c("mean","median","max","percentile"),
                   p = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame or matrix of real-valued time series, each row corresponding to one time series. Each row of this
matrix contains real-valued measurements for one gene on a time line, i. e.
column i+1 contains the successor states of column i.
}
  \item{method}{
The employed binarization technique. "mean" (default) uses mean of time series as the absolute value. "median" uses median of time series as the absolute value. "max" uses chosen p\% of max of time series as the absolute value. "percentile" uses chosen p\% percentile of time series as the absolute value. 
}
  \item{p}{
If method="max" or "percentile", this specifies the percentage. Default is 0.5.
}
}
\details{
}
\value{
\item{K}{
Vector of one discrete-valued time series, containing consensual result for input matrix. Vector contain discrete-valued measurements for on a time line, i. e. column i+1 contains the successor states of column i.
}
%% ...
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
cons.discretization(frs2, method = "max", p= 0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
