% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_ersd.r
\name{makePDcorr}
\alias{makePDcorr}
\title{Get the nearest positive definite correlation matrix to the matrix we have.}
\usage{
makePDcorr(cormat)
}
\arguments{
\item{cormat}{A correlation matrix for asset class returns.}
}
\value{
cormat if correlation matrix is positive definite, FALSE if not.
}
\description{
Note that this insists upon returning a positive definite correlation matrix. We would be
satisfied if it only returned a positive semi-definite matrix, but that does not seem
to be an option.
}
\details{
See:
  Higham, Nicholas J. (2002) Computing the Nearest Correlation Matrix---A Problem from Finance. IMA Journal of Numerical Analysis, 22 (3). pp. 329-343. ISSN 0272-4979
  http://eprints.ma.man.ac.uk/232/
}
\examples{
library("magrittr")

is.PSD(rvk$cormat)
cormat2 <- makePDcorr(rvk$cormat)
is.PSD(cormat2)

# compare:
rvk$cormat \%>\% round(., 2)
cormat2 \%>\% round(., 2)
(cormat2 - rvk$cormat) \%>\% round(., 2)

}
