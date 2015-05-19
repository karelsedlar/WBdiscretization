\name{data.norm}
\alias{data.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize a set of real-valued time series
}
\description{
Normalize a set of real-valued time series row by row. Every row represents one time serie.
}
\usage{
data.norm(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Data frame or matrix, each row corresponding to one time series. Each row of this
matrix contains real-valued measurements for one gene on a time line, i. e.
column i+1 contains the successor states of column i.
}
}
\details{
Every row is normalized according to its max value.
}
\value{
\item{X}{
Data frame or matrix of normalized values.
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
frs2 <- matrix(rnorm(20,5),nrow=2)
data.norm(frs2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
