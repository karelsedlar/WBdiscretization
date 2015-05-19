\name{stat.discretization}
\alias{stat.discretization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimates the quality of discretization
}
\description{
Estimates the quality of discretization by comparing matrix of discrete time series with matrix of expected results. It computes confusion matrix and precision, recall and overall accuracy.
}
\usage{
stat.discretization(X,
                    D,
                    states = c(0, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame or matrix of discrete-valued time series, each row corresponding to one time series. Each row of this
matrix contains discrete-valued measurements for one gene on a time line, i. e.
column i+1 contains the successor states of column i.
}
  \item{D}{
Data frame or matrix of expected discrete-valued time series, each row corresponding to one time series. 
}
  \item{states}{
Vector of discrete states, usually c(0,1), or c(1,2). Default is c(0,1).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list with the following elements:
\item{TP}{true positives}
\item{FP}{false positives}
\item{TN}{true negative}
\item{FN}{false negatives}
\item{precision}{precision}
\item{recall}{recall}
\item{accuracy}{overall accuracy}
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
data <- matrix(c(1,1,1,0,0,1,0,1,1,1,1,0,1,1,0), nrow=3)
expect <- matrix(c(1,1,0,1,1,1,0,1,1,0,1,0,1,1,0), nrow=3)
stat.discretization(X,D,states = c(0, 1))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
