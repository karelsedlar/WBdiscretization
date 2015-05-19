\name{WBdiscretization-package}
\alias{WBdiscretization-package}
\alias{WBdiscretization}
\docType{package}
\title{
Western blot time series discretization
}
\description{
This package provides several tools for Western blot time series data disretization. Overview of techniques can be found in author's diploma thesis.
}
\details{
\tabular{ll}{
Package: \tab WBdiscretization\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-01\cr
License: \tab freeware\cr
}
Further usage can be found in functions help[]
}
\author{
Karel Sedlar

Maintainer: Karel Sedlar <sedlar@feec.vutbr.cz>
}
\references{
Sedlar. K. Zpracovani experimentalnich dat ve vazbe na modely FGFR signalnich drah, diplomova prace, 2015
}

\keyword{ package }
\seealso{


}
\examples{
abs.discretization(X, method = "max", p = 0.5)
kmeans.discretization(X,l = 2, nstart= 100, iter.max = 250)
}
